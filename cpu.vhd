-- This file is part of an as yet unnamed RISC-V core.
--
-- The core is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- The core is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with the core.  If not, see <https://www.gnu.org/licenses/>.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.types.all;
use work.common.all;
use work.cpu_defs.all;

entity cpu is
  generic (
    boot_addr : u32     := 32x"0";
    exc_addr  : u32     := 32x"100";
    lattice   : boolean := false;
    aligned   : boolean := true;
    ram_type  : integer := 0;
    ram_depth : integer := 1024;
    init_file : string  := ""
  );
  port (
    -- Interface towards external single cycle RAM (type 0).
    ext_addr  : out w32;
    ext_r     : in  w32;
    ext_w     : out w32;
    ext_be    : out std_logic_vector(3 downto 0) := (others => '0');

    -- External interface to program internal RAM (type 2).
    ext_clk   : in  std_logic;
    ext_rst   : in  std_logic;
    ext_waddr : in  w32;
    ext_din   : in  w32;
    ext_we    : in std_logic_vector(3 downto 0);
    ext_en    : in std_logic;
    ext_valid : in  std_logic := '1';

    -- Dummy output to make sure everything is not optimized out.
    ext_out   : out w8;

    irq   : in  std_logic := '0';   -- qqq Synchronize this!

    clk   : in  std_logic;
    reset : in  std_logic
  );
end cpu;

architecture rtl of cpu is

  subtype s32 is signed(31 downto 0);
  subtype s33 is signed(32 downto 0);
  type mem_t  is array(integer range <>) of u32;
  type memb_t is array(integer range <>) of w8;
  type ram_t  is array(3 downto 0) of memb_t(1023 downto 0);

  signal reg : mem_t(0 to 31);
  signal pc  : u32;

  -- Lattice
  signal reg_1 : mem_t(0 to 31);
  signal reg_2 : mem_t(0 to 31);
  -- End Lattice

  signal ram_b : ram_t;

  signal ram_be    : std_logic_vector(3 downto 0) := "0000";
  signal ram_w     : w32;
  signal ram_r     : w32;
  signal ram_b_w   : memb_t(3 downto 0);
  signal ram_b_r   : memb_t(3 downto 0);

  signal addr : u32;

  signal irq_sync   : std_logic;
  signal irq_ack    : std_logic := '0';
  signal irq_active : std_logic := '0';
  signal irq_taken  : std_logic := '0';

  signal idata   : u32;
  alias width32 is idata(1 downto 0);
  alias opcode  is idata(6 downto 2);
  alias rd      is idata(11 downto 7);
  alias funct3  is idata(14 downto 12);
  alias rs1     is idata(19 downto 15);
  alias rs2     is idata(24 downto 20);
  alias special is idata(30);
  alias funct7  is idata(31 downto 25);
  alias funct12 is idata(31 downto 20);

  signal imm20u : std_logic_vector(19 downto 0);
  signal imm12i : std_logic_vector(11 downto 0);
  signal imm12s : std_logic_vector(11 downto 0);
  signal imm13b : std_logic_vector(12 downto 0);
  signal imm21j : std_logic_vector(20 downto 0);

  signal imm32i : s32;
  signal imm32s : s32;
  signal imm32b : s32;
  signal imm32u : s32;
  signal imm32j : s32;

  signal reg_write  : std_logic := '0';
  signal reg_dst    : unsigned(4 downto 0);
  signal reg_wdata  : u32;

  signal pc_addr : u32 := boot_addr;

  signal csr_r     : std_logic;
  signal csr_w     : std_logic;
  signal csr_done  : std_logic;
  signal csr_addr  : unsigned(11 downto 0);
  signal csr_clear : w32;
  signal csr_set   : w32;
  signal csr_read  : w32;


--  signal pmpcfg : u8;
--  alias  pmp_r is pmpcfg(0);           -- Readable
--  alias  pmp_w is pmpcfg(1);           -- Writable
--  alias  pmp_x is pmpcfg(2);           -- eXecutable
--  alias  pmp_a is pmpcfg(4 downto 3);  -- Alignment
  -- 0 - disabled, 3 - aligned ^2, 1 - top boundary
                                 -- (bottom from previous one), 2 - naturally
                                 -- aligned 4 byte
--  alias  pmp_l is pmpcfg(7);           -- Lock (also applies rwx in machine mode)

  signal opname : op_t;

  signal instr_retired : std_logic := '1';   -- qqq Dummy!

    -- Always direct mapped, for now (qqq).
  signal csr_mtvec : w32 := std_logic_vector(exc_addr);

begin

  imm12i <= ram_r(31 downto 20);
  imm12s <= ram_r(31 downto 25) & ram_r(11 downto 7);
  imm13b <= ram_r(31) & ram_r(7) & ram_r(30 downto 25) & ram_r(11 downto 8) & '0';
  imm20u <= ram_r(31 downto 12);
  imm21j <= ram_r(31) & ram_r(19 downto 12) & ram_r(20) & ram_r(30 downto 21) & '0';

  imm32i <= suit(signed(imm12i), imm32i);
  imm32s <= suit(signed(imm12s), imm32s);
  imm32b <= suit(signed(imm13b), imm32b);
  imm32u <= suit(signed(imm20u) & x"000", imm32u);
  imm32j <= suit(signed(imm21j), imm32j);

  idata <= suit(ram_r, idata);

  ram_type_0: if ram_type = 0 generate
    ext_addr <= suit(addr,   ext_addr);
    ext_be   <= suit(ram_be, ext_be);
    ext_w    <= suit(ram_w,  ext_w);
    ram_r    <= suit(ext_r,  ram_r);
  end generate;

 ram_type_1: if ram_type = 1 generate
  ram_access_gen: for i in 3 downto 0 generate
    ram_r(i * 8 + 7 downto i * 8) <= ram_b_r(i);
    ram_b_w(i) <= ram_w(i * 8 + 7 downto i * 8);
  end generate;

  ram_access: process(clk)
    variable i : integer range ram_be'high downto ram_be'low;
--    variable addr_4 : unsigned(addr'high downto 2);
    variable addr_4 : unsigned(13 downto 2);
  begin
    if rising_edge(clk) then
      addr_4 := addr(13 downto 2);
      for i in ram_be'range loop
        if ram_be(i) then
          ram_b(i)(to_integer(addr_4)) <= suit(ram_b_w(i), ram_b(0)(0));
        else
          ram_b_r(i) <= suit(ram_b(i)(to_integer(addr_4)), ram_b_r(0));
        end if;
      end loop;
    end if;
  end process;
 end generate;

 ram_type_2: if ram_type = 2 generate
 ram: block
   signal dpram_addr  : w32;
   signal dpram_be    : std_logic_vector(3 downto 0);
   signal dpram_w     : w32;
   signal dpram_r     : w32;
   signal dpram_douta : w32;  -- Dummy!
 begin
    dpram_addr <= suit(addr,    dpram_addr);
    dpram_be   <= suit(ram_be,  dpram_be);
    dpram_w    <= suit(ram_w,   dpram_w);
    ram_r      <= suit(dpram_r, ram_r);

  dpram_1: entity work.dpram
    generic map (
      RAM_DEPTH       => ram_depth,
      RAM_PERFORMANCE => "LOW_LATENCY",
      INIT_FILE       => init_file
    )
    port map (
      clka   => ext_clk,
      rsta   => ext_rst,
      addra  => ext_waddr(ext_waddr'high downto 2),
      dina   => ext_din,
      wea    => ext_we,
      ena    => ext_en,
      regcea => '0',
      douta  => dpram_douta,
      clkb   => clk,
      rstb   => reset,
      addrb  => dpram_addr(dpram_addr'high downto 2),
      dinb   => dpram_w,
      web    => dpram_be,
      enb    => '1',
      regceb => '0',
      doutb  => dpram_r
    );
 end block;
 end generate;


  -- qqq Do this properly!
  sync_irq: process(clk, irq, reset)
    variable mark : std_logic;
    variable sync : std_logic_vector(0 to 2) := (others => '0');
  begin
    if rising_edge(clk) then
      sync := mark & sync(0 to sync'high - 1);
      if irq_ack then
        mark := '0';
        sync := (others => '0');
      end if;
      irq_sync <= sync(sync'high);
    end if;
    if irq then
      mark := '1';
    end if;
    if reset then
      irq_sync <= '0';
      mark     := '0';
      sync     := (others => '0');
    end if;
  end process;


  -- qqq Only separate to ensure no issues with memory generation.
  reg_access: process(clk)
  begin
    if rising_edge(clk) then
      if reg_write = '1' then                  -- Write to r0 as well, zeroing on read.
        if not lattice then
          reg(to_integer(reg_dst)) <= reg_wdata;
        else
          reg_1(to_integer(reg_dst)) <= reg_wdata;
          reg_2(to_integer(reg_dst)) <= reg_wdata;
        end if;
      end if;
    end if;
  end process;


  -- Two bits for r_mode (addr) are somehow critical?!?
  -- Use strobe/ready for memory accesses!
  -- Opt costs a reg: store already fetched next instruction on read and write.
  -- Opt costs an adder: calculate branch address on decode.
  -- Opt: it might now actually be better to store instruction (avoid some registers, faster RAM access).
  -- Opt: deliver csr address directly from decode, saves a cycle and some registers.
--  -- Opt: perhaps ot have result as a register (assign -). Should be good but wrecks SHIFT.
  engine: process(clk)
    type state_t is (INIT, IFETCH, DECODE, EXECUTE,
                     HANDLE_EXCEPTION, HANDLE_EXCEPTION_1, HANDLE_EXCEPTION_2,
                     FENCE_WAIT, SHIFT, BRANCH,
                     RAM_WAIT, RAM_READ, RAM_READ_NEXT_WAIT, RAM_READ_NEXT, RAM_STORED, RAM_STORE_NEXT,
                     CSR_WAIT, AWAIT_MEPC, AWAIT_IRQ);
    variable state     : state_t := INIT;
    variable add       : s33;
    variable less      : std_logic;
    variable equal     : boolean;
    variable was_equal : boolean;
    variable ram_rd    : boolean;
    variable rw_addr   : u32;
    variable pc_next   : u32;
    variable result    : s32;
    variable count     : unsigned(4 downto 0) := (others => '0');
    variable count_8   : boolean;
    variable dst       : unsigned(reg_dst'range);
    variable r_mode    : unsigned(4 downto 0);
    variable prefetch  : boolean := false;
    variable v1x       : s33 := (others => '0');
    variable v2x       : s33 := (others => '0');
    variable dox_subtract : boolean := false;
    variable exception : boolean;
    variable exception_addr : boolean;
    variable exception_cause : integer range 0 to 15;
    variable shifted   : w32;

    -- Lattice
    variable preg_r1data : s32;
    variable preg_r2data : s32;
    variable do_regw     : std_logic := '0';
    variable was_do_regw : std_logic := '0';
    -- End Lattice
    variable reg_r1data : s32;
    variable reg_r2data : s32;
    variable v1         : s33;
    variable v2         : s33;
    type     v1sel_t   is (REG1, REGPC);
    type     v2sel_t   is (REG2, IMMEDIATE);
    variable v1sel      : v1sel_t;
    variable v2sel      : v2sel_t;
    variable imm        : s33;
    variable do_unsigned : boolean;
    variable do_subtract : boolean;
  begin
    if rising_edge(clk) then
      result := (others => '-');   -- Whatever unless otherwise specified!

      ram_be <= "0000";
      ram_rd := false;

      csr_r <= '0';
      csr_w <= '0';

      irq_taken <= '0';

      reg_write <= '0';

      pc_next := pc_addr + 4;
      if count >= 8 then
        count   := count - 8;
        count_8 := true;
      else
        count   := count - 1;
        count_8 := false;
      end if;

      exception := false;

     if not lattice then
      -- On Xilinx, complete value selection was done during decode.
      -- Only addition/subtraction left for execute stage.
      if dox_subtract then
        add := v1x - v2x;
      else
        add := v1x + v2x;
      end if;
      less  := add(add'high);
--      less  := not add(add'high);  -- qqq Is this right?
     end if;



      -- Actual decoding

      -- For SLTU/BLTU/BGEU we need to do subtraction as unsigned.
      do_unsigned := false;
      if ((opcode = cat_op or opcode = cat_opi) and funct3 = op_sltu) or      -- slt/sltu
          (opcode = cat_bxx and (funct3 = bxx_ltu or funct3 = bxx_geu)) then  -- bxx (not ltu/geu)
        do_unsigned := true;
      end if;

      -- Add or subtract?
      do_subtract := false;
      if ((opcode = cat_op or opcode = cat_opi) and (funct3 = op_slt or funct3 = op_sltu)) or -- slt/sltu
         (opcode = cat_op and funct3 = op_add and special = '1') or                           -- sub
         (opcode = cat_bxx and funct3 /= bxx_eq and funct3 /= bxx_ne) then                    -- bxx (not eq/ne)
        do_subtract := true;
      end if;


     if lattice then
        -- On Lattice, register file access is clocked.
        -- Do everything else in execute stage.
        reg_r1data := preg_r1data;
        reg_r2data := preg_r2data;

        -- Register forwarding (and zero for r0).

        if was_do_regw then      -- Zeroed for r0 below.
          if reg_dst = rs1 then
            reg_r1data := suit(reg_wdata, reg_r1data);
          end if;
          if reg_dst = rs2 then
            reg_r2data := suit(reg_wdata, reg_r2data);
          end if;
        end if;
        was_do_regw := do_regw;
        do_regw := '0';

        if rs1 = 0 then
          reg_r1data := (others => '0');
        end if;
        if rs2 = 0 then
          reg_r2data := (others => '0');
        end if;

        equal := reg_r1data = reg_r2data;
        was_equal    := equal;

        case v1sel is
        when REG1 =>
          v1 := suit(reg_r1data, v1);
        when REGPC =>
          v1 := suit(pc_addr, v1);
        end case;

        case v2sel is
        when REG2 =>
          v2 := suit(reg_r2data, v2);
        when IMMEDIATE =>
          v2 := suit(imm, v2);
        end case;

        if state /= BRANCH and
          state /= RAM_STORE_NEXT and
          state /= RAM_WAIT and state /= RAM_READ then
          v1x := v1;
          v2x := v2;
          if do_unsigned then
            v1x(v1x'high) := '0';
            v2x(v2x'high) := '0';
          end if;
        end if;

        if dox_subtract then
          add := v1x - v2x;
        else
          add := v1x + v2x;
        end if;
        less  := add(add'high);

        dox_subtract := do_subtract;

        -- This is the clocked register file access.
        preg_r1data := suit(reg_1(to_integer(rs1)), preg_r1data);
        preg_r2data := suit(reg_2(to_integer(rs2)), preg_r2data);

     else
        do_regw   := '0';
       -- On Xilinx, fetch register data during decode (non-clocked reg file).

       reg_r1data := suit(reg(to_integer(rs1)), reg_r1data);
       reg_r2data := suit(reg(to_integer(rs2)), reg_r2data);

      -- Register forwarding (and zero for r0).

      if reg_write = '1' then      -- Zeroed for r0 below.
        if reg_dst = rs1 then
          reg_r1data := suit(reg_wdata, reg_r1data);
        end if;
        if reg_dst = rs2 then
          reg_r2data := suit(reg_wdata, reg_r2data);
        end if;
      end if;

      if rs1 = 0 then
        reg_r1data := (others => '0');
      end if;
      if rs2 = 0 then
        reg_r2data := (others => '0');
      end if;
     end if;


      case to_integer(opcode) is
      when cat_op | cat_opi | cat_load | cat_store | cat_jalr =>
        v1sel := REG1;
      when cat_auipc | cat_jal =>
        v1sel := REGPC;
      when cat_bxx =>
        if funct3 = bxx_eq or funct3 = bxx_ne then
          v1sel := REGPC;
        else
          v1sel := REG1;
        end if;
      when cat_sys =>         -- Correct for csrrw/rs/rc at least.
        v1sel := REG1;
      when others =>             -- Does not matter!
        v1sel := REG1;
      end case;

      case to_integer(opcode) is
      when cat_op =>
        v2sel := REG2;
      when cat_opi | cat_jalr | cat_load =>
        v2sel := IMMEDIATE;
        imm   := suit(imm32i, imm);
      when cat_auipc =>
        v2sel := IMMEDIATE;
        imm   := suit(imm32u, imm);
      when cat_jal =>
        v2sel := IMMEDIATE;
        imm   := suit(imm32j, imm);
      when cat_bxx =>
        if funct3 = bxx_eq or funct3 = bxx_ne then
          v2sel := IMMEDIATE;
          imm   := suit(imm32b, imm);
        else
          v2sel := REG2;
        end if;
      when cat_store =>
        v2sel := IMMEDIATE;
        imm   := suit(imm32s, imm);
      when cat_sys =>         -- Correct for csrrw/rs/rc at least.
        v2sel := IMMEDIATE;
        imm   := suit(imm32i, imm);
      when others =>             -- Does not matter!
        v2sel := IMMEDIATE;
        imm   := suit(imm32i, imm);
      end case;

     if not lattice then
      case v1sel is
      when REG1 =>
        v1 := suit(reg_r1data, v1);
      when REGPC =>
        v1 := suit(pc_addr, v1);
      end case;

      case v2sel is
      when REG2 =>
        v2 := suit(reg_r2data, v2);
      when IMMEDIATE =>
        v2 := suit(imm, v2);
      end case;

      equal := reg_r1data = reg_r2data;
     end if;

       -- End of actual decoding.



      case state is
      when INIT =>
        state  := IFETCH;

      when IFETCH =>
        if ext_valid then
          state := DECODE;
        end if;

      when DECODE =>
        -- Interrupts only accepted when we are not executing an instruction.
        if irq_active then
          irq_taken       <= '1';
          exception       := true;
          exception_cause := irq_m_external;
        end if;

        state := EXECUTE;
        addr  <= pc_next;     -- Fetch next instruction, in case we can.

       if not lattice then
        v1x := v1;
        v2x := v2;
        if do_unsigned then
          v1x(v1x'high) := '0';
          v2x(v2x'high) := '0';
        end if;

        was_equal    := equal;
        dox_subtract := do_subtract;
       end if;

        -- qqq Why only these for prefetch?
        --     load/store can definitely prefetch, but will never go to decode directly after execcute.
        case to_integer(opcode) is
        when cat_op | cat_opi | cat_load | cat_store | cat_auipc =>
          prefetch := true;
        when others =>
          prefetch := false;
        end case;

      when HANDLE_EXCEPTION =>
        csr_addr  <= suit(addr_mepc, csr_addr);
        csr_clear <= (others => '1');
        csr_set   <= suit(pc_addr, csr_set);
        csr_w     <= '1';
        pc_addr   <= suit(csr_mtvec, pc_addr);
        addr      <= suit(csr_mtvec, addr);
        if exception_addr then
          state     := HANDLE_EXCEPTION_1;
        else
          state     := HANDLE_EXCEPTION_2;
        end if;

      when HANDLE_EXCEPTION_1 =>
        csr_addr  <= suit(addr_mtval, csr_addr);
        csr_clear <= (others => '1');
        csr_set   <= suit(shifted, csr_set);
        csr_w     <= '1';
        state     := HANDLE_EXCEPTION_2;

      when HANDLE_EXCEPTION_2 =>
        csr_addr  <= suit(addr_mcause, csr_addr);
        csr_clear <= (others => '1');
        csr_set   <= suit(exception_cause, csr_set);
        csr_w     <= '1';
        exception := false;
        state     := DECODE;

      when EXECUTE =>
        reg_dst   <= rd;

        if prefetch then
          state   := DECODE;
        else
          state   := IFETCH;
        end if;
        reg_write <= '1';
        do_regw   := '1';
        pc_addr   <= pc_next;

        case to_integer(opcode) is
        when cat_op | cat_opi =>
          case to_integer(funct3) is
          when op_add =>
            result := suit(add, result);
          when op_slt | op_sltu =>
--            result := suit(less, result);
            if less then
              result := suit(1, result);
            else
              result := suit(0, result);
            end if;
          when op_sll | op_srl =>
            count     := suit(v2x, count);    --x
            result    := suit(v1x, result);   --x
            shifted   := suit(v1x, shifted);  --x
            if count /= 0 then
              reg_write <= '0';
              do_regw   := '0';
              r_mode    := suit(special & funct3, r_mode);
              state     := SHIFT;
            end if;
          when op_xor =>
            result := suit(v1x xor v2x, result);  --x
          when op_or =>
            result := suit(v1x or v2x, result);   --x
          when op_and =>
            result := suit(v1x and v2x, result);  --x
          when others =>     -- Cannot happen!
            exception       := true;
            exception_cause := exc_impossible;
          end case;

        when cat_lui =>
          result := suit(imm32u, result);

        when cat_auipc =>
          result := suit(add, result);

        when cat_jal | cat_jalr =>
          pc_addr <= suit(add, pc_addr);
          pc_addr(0) <= '0';
          addr    <= suit(add, addr);
          addr(0) <= '0';
          result  := suit(pc_next, result);
          if add(1) then
            pc_addr         <= pc_addr;          -- Do not update yet - exception!
            exception       := true;
            exception_addr  := true;
            exception_cause := exc_i_addr_align;
            shifted         := suit(add(add'high downto 1) * 2, shifted);
          end if;

        -- qqq Make this use standard prefetch logic and go to ifetch when not OK.
        when cat_bxx =>
--      if doing_branch then
--        v1sel := REGPC;
--        v2sel := IMMEDIATE;
--        imm   := suit(imm32b, imm);
--      end if;
--
--      case v1sel is
--      when REG1 =>
--        v1 := suit(reg_r1data, v1);
--      when REGPC =>
--        v1 := suit(pc_addr, v1);
--      end case;
--
--      case v2sel is
--      when REG2 =>
--        v2 := suit(reg_r2data, v2);
--      when IMMEDIATE =>
--        v2 := suit(imm, v2);
--      end case;

          pc_addr         <= pc_addr;          -- Do not update yet, in case of exception!
          reg_write <= '0';
          do_regw   := '0';
          case to_integer(funct3) is
          when bxx_eq =>
            if was_equal then
              addr    <= suit(add, addr);
              if add(1) then
                exception       := true;
                exception_addr  := true;
                exception_cause := exc_i_addr_align;
                shifted         := suit(add, shifted);
              else
                pc_addr <= suit(add, pc_addr);
              end if;
            else
              pc_addr <= pc_next;
              state   := DECODE;
            end if;
          when bxx_ne =>
            if not was_equal then
              addr    <= suit(add, addr);
              if add(1) then
                exception       := true;
                exception_addr  := true;
                exception_cause := exc_i_addr_align;
                shifted         := suit(add, shifted);
              else
                pc_addr <= suit(add, pc_addr);
              end if;
            else
              pc_addr <= pc_next;
              state   := DECODE;
            end if;
          when bxx_lt | bxx_ltu =>
            if less then
              v1x := suit(pc_addr, v1x);  -- Calculate branch address next cycle.
              v2x := suit(imm32b, v2x);
              dox_subtract := false;
              state        := BRANCH;
            else
              pc_addr <= pc_next;
              state := DECODE;
            end if;
          when bxx_ge | bxx_geu =>
            if not less then
              v1x := suit(pc_addr, v1x);  -- Calculate branch address next cycle.
              v2x := suit(imm32b, v2x);
              dox_subtract := false;
              state        := BRANCH;
            else
              pc_addr <= pc_next;
              state := DECODE;
            end if;
          when others =>            -- Illegal branch!
            exception       := true;
            exception_cause := exc_illegal_bxx;
          end case;

        when cat_load =>
          reg_write <= '0';
          do_regw   := '0';
          dst       := rd;
          r_mode    := unsigned(add(1 downto 0)) & funct3;
--          ram_rd    := true;
--          rw_addr   := suit(add, rw_addr);
          addr      <= suit(add, addr);
          if not aligned then
            v1x     := suit(add, v1x);   -- Prepare for misaligned read.
            v2x     := suit(4, v2x);
          end if;
          pc_addr   <= pc_addr;          -- Do not update yet, in case of exception!
          state     := RAM_WAIT;

        when cat_store =>
          reg_write <= '0';
          do_regw   := '0';
--          rw_addr   := suit(add, rw_addr);
          addr      <= suit(add, addr);
          ram_w     <= suit(reg_r2data, ram_w);   -- Assume aligned.
          state := RAM_STORED;
          case to_integer(funct3) is
          when mem_b =>
            case add(1 downto 0) is
            when "01"   => ram_be <= "0010"; ram_w  <= suit(reg_r2data & x"00", ram_w);
            when "10"   => ram_be <= "0100"; ram_w  <= suit(reg_r2data & x"0000", ram_w);
            when "11"   => ram_be <= "1000"; ram_w  <= suit(reg_r2data & x"000000", ram_w);
            when others => ram_be <= "0001";        -- "00"
            end case;

          when mem_h =>
--            if add(1) then
--              ram_be <= "1100";
--              ram_w  <= suit(reg_r2data & x"0000", ram_w);
--            else
--              ram_be <= "0011";
--            end if;
            if not aligned then
              case add(1 downto 0) is
              when "01" =>
                ram_be <= "0110";
                ram_w  <= suit(reg_r2data & x"00", ram_w);
              when "10" =>
                ram_be <= "1100";
                ram_w  <= suit(reg_r2data & x"0000", ram_w);
              when "11" =>
                ram_be  <= "1000";
                ram_w   <= suit(reg_r2data & x"000000", ram_w);
                v1x     := suit(add, v1x);
                v2x     := suit(4, v2x);
                dox_subtract := false;
                r_mode  := suit(std_logic_vector'("0001"), r_mode);
                shifted := suit(reg_r2data(15 downto 8), shifted);
                state   := RAM_STORE_NEXT;
              when others =>   -- "00"
                ram_be <= "0011";
              end case;
            else
              case add(1 downto 0) is
              when "10" =>
                ram_be <= "1100";
                ram_w  <= suit(reg_r2data & x"0000", ram_w);
              when "00" =>
                ram_be <= "0011";
              when others =>
                pc_addr         <= pc_addr;          -- Do not update yet - exception!
                exception       := true;
                exception_addr  := true;
                exception_cause := exc_s_addr_align;
                shifted         := suit(add, shifted);
              end case;
            end if;

          when mem_w | mem_wu =>
            if not aligned then
              case add(1 downto 0) is
              when "01" =>
                ram_be <= "1110";
                ram_w  <= suit(reg_r2data & x"00", ram_w);
                v1x     := suit(add, v1x);
                v2x     := suit(4, v2x);
                dox_subtract := false;
                r_mode  := suit(std_logic_vector'("0001"), r_mode);
                shifted := suit(reg_r2data(31 downto 24), shifted);
                state   := RAM_STORE_NEXT;
              when "10" =>
                ram_be <= "1100";
                ram_w  <= suit(reg_r2data & x"0000", ram_w);
                v1x     := suit(add, v1x);
                v2x     := suit(4, v2x);
                dox_subtract := false;
                r_mode  := suit(std_logic_vector'("0011"), r_mode);
                shifted := suit(reg_r2data(31 downto 16), shifted);
                state   := RAM_STORE_NEXT;
              when "11" =>
                ram_be  <= "1000";
                ram_w   <= suit(reg_r2data & x"000000", ram_w);
                v1x     := suit(add, v1x);
                v2x     := suit(4, v2x);
                dox_subtract := false;
                r_mode  := suit(std_logic_vector'("0111"), r_mode);
                shifted := suit(reg_r2data(31 downto 8), shifted);
                state   := RAM_STORE_NEXT;
              when others =>   -- "00"
                ram_be <= "1111";
              end case;
            else
              case add(1 downto 0) is
              when "00" =>
                ram_be <= "1111";
              when others =>
                pc_addr   <= pc_addr;          -- Do not update yet - exception!
                exception       := true;
                exception_addr  := true;
                exception_cause := exc_s_addr_align;
                shifted         := suit(add, shifted);
              end case;
            end if;

          when mem_d  =>
            exception       := true;
            exception_cause := exc_64_bit;

          when others =>
            exception := true;
            exception_cause := exc_illegal_store;
          end case;

        when cat_sys =>
          case to_integer(funct3) is
          when sys_csrrw | sys_csrrwi =>
            reg_write <= '0';
            do_regw   := '0';
            csr_addr  <= suit(unsigned(v2x), csr_addr);  --x
            if rd /= 0 then
              csr_r   <= '1';
            end if;
            csr_w     <= '1';
            csr_clear <= (others => '1');
            if to_integer(funct3) = sys_csrrw then
              csr_set <= suit(v1x, csr_set);  --x
            else
              csr_set <= suit(rs1, csr_set);
            end if;
            state     := CSR_WAIT;

          when sys_csrrs | sys_csrrsi =>
            reg_write <= '0';
            do_regw   := '0';
            csr_addr  <= suit(unsigned(v2x), csr_addr);  --x
            csr_r     <= '1';
            if rs1 /= 0 then
              csr_w   <= '1';
            end if;
            csr_clear <= (others => '0');
            if to_integer(funct3) = sys_csrrs then
              csr_set <= suit(v1x, csr_set);  --x
            else
              csr_set <= suit(rs1, csr_set);
            end if;
            state     := CSR_WAIT;

          when sys_csrrc | sys_csrrci =>
            reg_write   <= '0';
            do_regw     := '0';
            csr_addr    <= suit(unsigned(v2x), csr_addr);  --x
            csr_r       <= '1';
            if rs1 /= 0 then
              csr_w     <= '1';
            end if;
            if to_integer(funct3) = sys_csrrc then
              csr_clear <= suit(v1x, csr_set);  --x
            else
              csr_clear <= suit(rs1, csr_set);
            end if;
            csr_set     <= (others => '0');
            state       := CSR_WAIT;

          when sys_normal =>
            reg_write <= '0';
            do_regw   := '0';
            -- qqq rs1 and rd _must_ be zero for these. Illegal!
            case to_integer(funct12) is
            when sysn_ecall =>
              exception       := true;
              exception_cause := exc_m_ecall;
            when sysn_ebreak =>
              exception       := true;
              exception_cause := exc_breakpoint;
            when sysn_mret =>
              csr_addr    <= suit(addr_mepc, csr_addr);
              csr_r       <= '1';
              csr_clear   <= (others => '0');
              csr_set     <= (others => '0');
              state       := AWAIT_MEPC;
            when sysn_wfi =>
              csr_addr  <= suit(addr_mepc, csr_addr);
              csr_w     <= '1';
              csr_clear <= (others => '1');
              csr_set   <= suit(pc_next, csr_set);
              state     := AWAIT_IRQ;
            when others =>
              null;     -- qqq Illegal
            end case;

          when others =>
            exception       := true;
            exception_cause := exc_illegal_cat;
          end case;

        when cat_fence =>
          -- Do not do anything!
          -- qqq Check better!
          reg_write <= '0';
          do_regw   := '0';
          count     := suit(7, count);     -- qqq Just wait a while.
          state     := FENCE_WAIT;

        when others =>
          exception       := true;
          exception_cause := exc_illegal_cat;
        end case;

      when FENCE_WAIT =>
        if count = 0 then
          state := DECODE;
        end if;
        
      when SHIFT =>
        if r_mode(2 downto 0) = op_sll then
--          result := suit(result & '0', result);    -- Using result register takes quite some time.
          if count_8 then
            shifted := suit(shifted & x"00", shifted);
          else
            shifted := suit(shifted & '0', shifted);
          end if;
        else
          if count_8 then
            shifted := (31 downto 24 => shifted(shifted'high) and r_mode(3)) & shifted(shifted'high downto 8);
          else
            shifted := (shifted(shifted'high) and r_mode(3)) & shifted(shifted'high downto 1);
          end if;
        end if;
        result := suit(shifted, result);
        if count /= 0 then
          state     := SHIFT;
        else
          reg_write   <= '1';
          do_regw     := '1';
          state       := DECODE;
        end if;

      when BRANCH =>
        addr     <= suit(add, addr);
        if add(1) then
          exception       := true;
          exception_addr  := true;
          exception_cause := exc_i_addr_align;
          shifted         := suit(add, shifted);
        else
          pc_addr <= suit(add, pc_addr);
        end if;
        state    := IFETCH;

      when RAM_WAIT =>
--        addr  <= pc_addr;   -- Assume data address must be clocked already in here!
        if not aligned then
          v1x     := suit(addr, v1x);   -- Prepare for misaligned read.
          v2x     := suit(4, v2x);
        end if;
        state := RAM_READ;

      when RAM_READ =>
        if not aligned then
          v1x := suit(addr, v1x);   -- Prepare for misaligned read.
          v2x := suit(4, v2x);
        end if;
        if ext_valid then
          pc_addr   <= pc_next;
          addr      <= pc_next;
          reg_write <= '1';
          do_regw   := '1';
          reg_dst   <= dst;
          state     := IFETCH;   -- Perhaps use old stored instr and go to decode?
        end if;
        case to_integer(r_mode(1 downto 0)) is
        when mem_b =>
          case r_mode(4 downto 3) is
          when "01"   => result := suit(ram_r(15 downto  8), result);
          when "10"   => result := suit(ram_r(23 downto 16), result);
          when "11"   => result := suit(ram_r(31 downto 24), result);
          when others => result := suit(ram_r, result); -- "00"
          end case;
          for i in 31 downto 8 loop
            result(i) := result(7) and not r_mode(2);
          end loop;

        when mem_h =>
--          if r_mode(4) then
--            result := suit(ram_r(31 downto 16), result);
--          else
--            result := suit(ram_r, result);
--          end if;
          if not aligned then
            case r_mode(4 downto 3) is
            when "00"   => result := suit(ram_r, result);
            when "01"   => result := suit(ram_r(23 downto  8), result);
            when "10"   => result := suit(ram_r(31 downto 16), result);
            when others =>   -- "11"
              shifted := suit(ram_r, shifted);
              reg_write <= '0';
              do_regw   := '0';
              addr      <= suit(add, addr);
              state     := RAM_READ_NEXT_WAIT;
              dox_subtract := false;
            end case;
          else
            if r_mode(4) then
              result := suit(ram_r(31 downto 16), result);
            else
              result := suit(ram_r, result);
            end if;
            case r_mode(4 downto 3) is
            when "00" | "10" => null;
            when others =>
--              reg_write <= '0';
--              do_regw   := '0';
              pc_addr   <= pc_addr;          -- Do not update yet - exception!
              exception       := true;
              exception_addr  := true;
              exception_cause := exc_l_addr_align;
              shifted         := suit(addr, shifted);
            end case;
          end if;
          for i in 31 downto 16 loop
            result(i) := result(15) and not r_mode(2);
          end loop;

        when mem_w =>
          if not aligned then
            case r_mode(4 downto 3) is
            when "00"   => result := suit(ram_r, result);
            when others =>
              shifted := suit(ram_r, shifted);
              reg_write <= '0';
              do_regw   := '0';
              addr      <= suit(add, addr);
              state     := RAM_READ_NEXT_WAIT;
              dox_subtract := false;
            end case;
          else
            result := suit(ram_r, result);
            case r_mode(4 downto 3) is
            when "00"   => null;
            when others =>
--              reg_write <= '0';
--              do_regw   := '0';
              pc_addr   <= pc_addr;          -- Do not update yet - exception!
              exception       := true;
              exception_addr  := true;
              exception_cause := exc_l_addr_align;
              shifted         := suit(addr, shifted);
            end case;
          end if;

        when mem_d =>    -- 64 bit, right?
          exception       := true;
          exception_cause := exc_64_bit;

        when others =>     -- Illegal load;
          exception       := true;
          exception_cause := exc_illegal_load;
        end case;

      when RAM_READ_NEXT_WAIT =>
        state := RAM_READ_NEXT;

      when RAM_READ_NEXT =>
        if ext_valid then
          pc_addr   <= pc_next;
          addr      <= pc_next;
          reg_write <= '1';
          do_regw   := '1';
          reg_dst   <= dst;
          state     := IFETCH;   -- Perhaps use old stored instr and go to decode?
        end if;
        case to_integer(r_mode(1 downto 0)) is
        when mem_h =>
          result := suit(ram_r(7 downto 0) & shifted(31 downto 24), result);
          for i in 31 downto 16 loop
            result(i) := result(15) and not r_mode(2);
          end loop;
        when others =>   -- mem_w
          case r_mode(4 downto 3) is
          when "01"   => result := suit(ram_r( 7 downto 0) & shifted(31 downto  8), result);
          when "10"   => result := suit(ram_r(15 downto 0) & shifted(31 downto 16), result);
          when others => result := suit(ram_r(23 downto 0) & shifted(31 downto 24), result);
          end case;
        end case;

      when RAM_STORED =>
        if ext_valid then
           addr  <= pc_addr;
           state := IFETCH;   -- Perhaps use old stored instr and go to decode?
         end if;

      when RAM_STORE_NEXT =>
        if ext_valid then
           addr   <= suit(add, addr);
           ram_be <= suit(r_mode, ram_be);
           ram_w  <= suit(shifted, ram_w);
           state := RAM_STORED;
         end if;

      when CSR_WAIT =>
        result      := suit(csr_read, result);
        if csr_done then
          reg_write <= '1';
          do_regw   := '1';
          state     := DECODE;
        end if;

      when AWAIT_MEPC =>
        pc_addr <= suit(csr_read, pc_addr);
        addr    <= suit(csr_read, addr);
        if csr_done then
          state := IFETCH;
        end if;

      when AWAIT_IRQ =>
        if irq_sync then
          exception       := true;
          exception_cause := irq_m_external;
        end if;
        -- qqq And then what?
        -- Just handle interrupt and continue?
      end case;

--      if ram_rd or ram_be /= "0000" then
--        addr <= rw_addr;
--      else
--        addr <= pc_addr;
--      end if;

      -- No write to destination if exception!
      if exception then
        state     := HANDLE_EXCEPTION;
        pc_addr   <= pc_addr;    -- Needed for exception.
        if not exception_addr then    -- qqq Good idea?
          reg_write <= '0';
          do_regw   := '0';
        end if;
      end if;

      if do_regw then
        reg_write <= '1';
        reg_wdata <= suit(unsigned(result), reg_wdata);
      end if;

      if reset then
        state   := INIT;
        pc_addr <= suit(boot_addr, pc_addr);
        addr    <= suit(boot_addr, addr);
      end if;
    end if;
  end process;


  -- Reads from reg happens before instruction execution (if side effects).
  -- Update to actual reg happens after intruction executes.
  -- Ie writes to insttret can be read back by next instruction, due to
  -- increment of insttret happens before write of new value.
  -- Strange...

  -- qqq Should non-existing CSR addresses cause fails?
  -- Opt: Write in next cycle? Separate read and write somehow.
  -- Opt: Decode addresses to shorter ones in decode!

  csr_access: process(clk)
    variable csr_data  : w32;
    variable csr_write : w32;

    -- qqq Implement 32 bit counters and irq on overflow?
    -- Use same adder for all or some?
    variable csr_mcycle      : u64 := (others => '0');   -- CPU cycles
    alias    csr_mcycle_h   is csr_mcycle(63 downto 32);
    alias    csr_mcycle_l   is csr_mcycle(31 downto 0);
    variable csr_minstret    : u64 := (others => '0');   -- Instructions retired (see above)
    alias    csr_minstret_h is csr_mcycle(63 downto 32);
    alias    csr_minstret_l is csr_mcycle(31 downto 0);

    variable mcycle_lowx   : unsigned(32 downto 0) := (others => '0');
    variable minstret_lowx : unsigned(32 downto 0) := (others => '0');

  --  also mhpmcounter3 to 31. May be hardwired to 0.
  --  also mhpmevent3 to 31. May be hardwired to 0.

  -- These are memory mapped
  --  signal csr_mtime    : u64 := (others => '0');   -- Wall clock, fixed rate.
  -- signal csr_mtimecmp : u64;

    variable csr_misa    : w32 := "01" & x"0" & 26x"0000100";  -- RISC-V32i

    -- Also available restricted as sstatus and ustatus (when available).
    variable csr_mstatus : w32 := (others => '0');
    alias    mstat_zero is csr_mstatus(31 downto 13);
    alias    mstat_sd   is csr_mstatus(31); -- 0 if no xs/fs
    alias    mstat_tsr  is csr_mstatus(22); -- 0 if no super mode
    alias    mstat_tw   is csr_mstatus(21); -- 0 if no super mode
    alias    mstat_tvm  is csr_mstatus(20); -- 0 if no super mode
    alias    mstat_mxr  is csr_mstatus(19); -- 0 if no super mode
    alias    mstat_sum  is csr_mstatus(18); -- 0 if no super mode
    alias    mstat_mprv is csr_mstatus(17); -- 0 if no user mode
    alias    mstat_xs   is csr_mstatus(16 downto 15); -- 0 if no ext with state
    alias    mstat_fs   is csr_mstatus(14 downto 13); -- 0 if no super/float
    -- The next two are WARL
    alias    mstat_mpp  is csr_mstatus(12 downto 11);  -- Previous privilege?
    alias    mstat_spp  is csr_mstatus(8);  -- Not implemented
    alias    mstat_mpie is csr_mstatus(7);
    alias    mstat_spie is csr_mstatus(5);  -- Not implemented
    alias    mstat_upie is csr_mstatus(4);  -- Not implemented
    alias    mstat_mie  is csr_mstatus(3);
    alias    mstat_sie  is csr_mstatus(1);  -- Not implemented
    alias    mstat_uie  is csr_mstatus(0);  -- Not implemented

    -- qqq More to do here!
    variable csr_mcause : w32;
    variable csr_mtval  : w32;
    variable csr_mepc   : w32;

    -- qqq These will likely need to be signals!

    -- Always direct mapped, for now (qqq).
    -- Fixed address, for now (qqq).
    --variable csr_mtvec : w32 := std_logic_vector(exc_addr);

    -- Restricted as sip/sie and uip/uie.
    -- Priority is external > timer > software
    variable csr_mip : w32;
    alias mip_xxip  is csr_mip(31 downto 16);  -- Impl specific
    alias mip_meip  is csr_mip(11);  -- External interrupt
    alias mip_seip  is csr_mip(9);   -- 0 if no super mode
    alias mip_ueip  is csr_mip(8);   -- 0 if no super mode
    alias mip_mtip  is csr_mip(7);   -- Timer interrupt
    alias mip_stip  is csr_mip(5);   -- 0 if no super mode
    alias mip_utip  is csr_mip(4);   -- 0 if no super mode
    alias mip_msip  is csr_mip(3);   -- Software interrupt
    alias mip_ssip  is csr_mip(1);   -- 0 if no super mode
    alias mip_usip  is csr_mip(0);   -- 0 if no super mode
    variable csr_mie : w32;
    alias mie_xxie  is csr_mie(31 downto 16);  -- Impl specific
    alias mie_meie  is csr_mie(11);  -- External interrupt
    alias mie_seie  is csr_mie(9);   -- 0 if no super mode
    alias mie_ueie  is csr_mie(8);   -- 0 if no super mode
    alias mie_mtie  is csr_mie(7);   -- Timer interrupt
    alias mie_stie  is csr_mie(5);   -- 0 if no super mode
    alias mie_utie  is csr_mie(4);   -- 0 if no super mode
    alias mie_msie  is csr_mie(3);   -- Software interrupt
    alias mie_ssie  is csr_mie(1);   -- 0 if no super mode
    alias mie_usie  is csr_mie(0);   -- 0 if no super mode

    variable csr_mscratch : w32 := (others => '0');
  begin
    if rising_edge(clk) then
      -- For testing
      ext_out <= suit(csr_mscratch, ext_out);

      csr_done <= '0';
      if csr_r or csr_w then
        csr_done <= '1';
      end if;

      case to_integer(csr_addr) is
      when addr_mvendorid | addr_marchid | addr_mimpid | addr_mhartid =>  -- Read only
        csr_data := (others => '0');          -- Not implemented!
      when addr_mcause =>    csr_data := suit(csr_mcause, csr_data);
      when addr_mtval =>     csr_data := suit(csr_mtval, csr_data);
      when addr_mepc =>      csr_data := suit(csr_mepc, csr_data);
      when addr_mtvec =>     csr_data := suit(csr_mtvec, csr_data);
      when addr_mcycle =>    csr_data := suit(csr_mcycle_l, csr_data);
      when addr_mcycleh =>   csr_data := suit(csr_mcycle_h, csr_data);
      when addr_minstret =>  csr_data := suit(csr_minstret_l, csr_data);
      when addr_minstreth => csr_data := suit(csr_minstret_h, csr_data);
      when addr_mstatus =>   csr_data := suit(csr_mstatus, csr_data);
      when addr_mip =>       csr_data := suit(csr_mip, csr_data);
      when addr_mie =>       csr_data := suit(csr_mie, csr_data);
      when addr_mscratch =>  csr_data := suit(csr_mscratch, csr_data);
      when others =>         csr_data := (others => '0');
      end case;

      -- Must update counters after reading, to avoid dependence.
      -- qqq Might not fulfill all requirementes for these!
      --     Perhaps delay readouts a cycle, or something?

      csr_mcycle_h   := csr_mcycle_h + mcycle_lowx(mcycle_lowx'high);
      csr_minstret_h := csr_minstret_h + minstret_lowx(minstret_lowx'high);

      mcycle_lowx  := suit(csr_mcycle_l, mcycle_lowx);
      mcycle_lowx  := mcycle_lowx + 1;
      csr_mcycle_l := suit(mcycle_lowx, csr_mcycle_l);
      if instr_retired then
        minstret_lowx  := suit(csr_minstret_l, minstret_lowx);
        minstret_lowx  := minstret_lowx + 1;
        csr_minstret_l := suit(minstret_lowx, csr_minstret_l);
      end if;


      if csr_r then
        -- Handle read side effects.
      end if;

      csr_read  <= csr_data;
      csr_write := (csr_data and not csr_clear) or csr_set;

      if csr_w then
        -- Handle write side effects.
        case to_integer(csr_addr) is
        -- qqq csr_mtvec would be nice to have writable!
        when addr_mcause =>    csr_mcause     := suit(csr_write, csr_mcause);
        when addr_mtval =>     csr_mtval      := suit(csr_write, csr_mtval);
        when addr_mepc =>      csr_mepc       := suit(csr_write, csr_mepc);
        when addr_mtvec =>     csr_mtvec      <= suit(csr_write, csr_mtvec);
        when addr_mcycle =>    csr_mcycle_l   := suit(csr_write, csr_mcycle_l);
        when addr_mcycleh =>   csr_mcycle_h   := suit(csr_write, csr_mcycle_h);
        when addr_minstret =>  csr_minstret_l := suit(csr_write, csr_minstret_l);
        when addr_minstreth => csr_minstret_h := suit(csr_write, csr_minstret_h);
        when addr_mstatus =>   csr_mstatus    := suit(csr_write, csr_mstatus);
        when addr_mip =>       csr_mip        := suit(csr_write, csr_mip);
        when addr_mie =>       csr_mie        := suit(csr_write, csr_mie);
        when addr_mscratch =>  csr_mscratch   := suit(csr_write, csr_mscratch);
        when others =>
          null;
        end case;
      end if;

      irq_active <= mip_meip and mie_meie;

      if irq_taken then
        mie_meie := '0';
      end if;

      irq_ack  <= '0';
      if irq_sync and not mip_meip then
        irq_ack  <= '1';
        mip_meip := '1';
      end if;

      -- Hard zeros
      mstat_zero := (others => '0');
      mstat_mpp  := "11";   -- Only machine mode possible (WARL)
      mstat_spp  := '0';    -- Or what? (WARL)
      mip_seip   := '0';  mip_ueip   := '0';
      mip_stip   := '0';  mip_utip   := '0';
      mip_ssip   := '0';  mip_usip   := '0';
      mie_seie   := '0';  mie_ueie   := '0';
      mie_stie   := '0';  mie_utie   := '0';
      mie_ssie   := '0';  mie_usie   := '0';
      csr_mepc(1 downto 0) := (others => '0');   -- Low two bits 0 for IALIGN=32.

      if reset then
        csr_mip    := (others => '0');
        csr_mie    := (others => '0');
        mstat_mprv := '0';
        csr_mcause := (others => '0');
        csr_mtvec  <= suit(exc_addr, csr_mtvec);
      end if;
    end if;
  end process;


-- synthesis translate_off
  opcodes: process(all)
  begin
    opname <= to_opname(to_integer(opcode), to_integer(funct3), to_integer(funct7), to_integer(funct12));
  end process;
-- synthesis translate_on

--  Platform-level interrupt controller (PLIC)

end architecture;
