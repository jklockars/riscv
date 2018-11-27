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
use std.textio.all;

library work;
use work.types.all;
use work.common.all;
use work.cpu_defs.all;


entity cpu_tb is
  generic (
    path      : string  := "c:\Users\johan\Sources\lattice\cpu\";
    boot_addr : u32     := x"8000_0000";
    exc_addr  : u32     := x"8000_0100";
    lattice   : boolean := true;
--    lattice   : boolean := false;
    ram_type  : integer := 0;
    test      : integer := 10
  );
end entity cpu_tb;

architecture test of cpu_tb is

  signal ext_addr : w32;
  signal ext_r    : w32 := (others => '0');
  signal ext_w    : w32;
  signal ext_be   : std_logic_vector(3 downto 0);

  signal ext_out  : w8;
  signal irq      : std_logic := '0';

  signal clk      : std_logic := '0';
  signal reset    : std_logic := '1';

  type mem_t  is array(integer range <>) of u32;
  signal memory: mem_t(96 to 127) := (others => (others => '0'));

  function instr_base(cat : integer; op : integer;
                      dst : integer; src1 : integer; src2 : integer) return w32 is
    variable instr : w32;
    alias width32 is instr(1 downto 0);
    alias opcode  is instr(6 downto 2);
    alias rd      is instr(11 downto 7);
    alias funct3  is instr(14 downto 12);
    alias rs1     is instr(19 downto 15);
    alias rs2     is instr(24 downto 20);
  begin
    width32 := suit(3,    width32);
    opcode  := suit(cat,  opcode);
    funct3  := suit(op,   funct3);
    rd      := suit(dst,  rd);
    rs1     := suit(src1, rs1);
    rs2     := suit(src2, rs2);

    return instr;
  end function;

  function instr_r(cat : integer; op : integer;
                   dst : integer; src1 : integer; src2 : integer) return w32 is
    variable instr : w32;
--    alias special is instr(30);
    alias funct7  is instr(31 downto 25);
  begin
    instr  := instr_base(cat, op, dst, src1, src2);
    funct7 := suit(0,    funct7);

    return instr;
  end function;

  function instr_r(cat : integer; op : integer; extra : integer;
                   dst : integer; src1 : integer; src2 : integer) return w32 is
  begin
    return x"0000_0000";
  end function;

  function instr_i(cat : integer; op : integer;
                   dst : integer; src1 : integer; im : integer) return w32 is
    variable instr : w32;
--    alias special is instr(30);
    alias imm12i  is instr(31 downto 20);
  begin
    instr  := instr_base(cat, op, dst, src1, 0);
    imm12i := suit(im,   imm12i);
    if to_integer(signed(imm12i)) /= im then
      -- Impossible to encode.
    end if;

    return instr;
  end function;

  function instr_s(cat : integer; op : integer;
                   src1 : integer; src2 : integer; im : integer) return w32 is
    variable instr : w32;
--    alias special is instr(30);
    alias rd      is instr(11 downto 7);
    alias funct7  is instr(31 downto 25);
    variable imm12 : std_logic_vector(11 downto 0);
  begin
    instr  := instr_base(cat, op, 0, src1, src2);
    imm12  := suit(im,   imm12);
    if to_integer(signed(imm12)) /= im then
      -- Impossible to encode.
    end if;
    funct7 := imm12(11 downto 5);
    rd     := imm12(4 downto 0);

    return instr;
  end function;

  function instr_b(cat : integer; op : integer;
                   src1 : integer; src2 : integer; im : integer) return w32 is
    variable instr : w32;
    alias rd      is instr(11 downto 7);
--    alias special is instr(30);
    alias funct7  is instr(31 downto 25);
    variable imm13 : std_logic_vector(12 downto 0);
  begin
    instr  := instr_base(cat, op, 0, src1, src2);
    imm13  := suit(im,   imm13);
    if to_integer(signed(imm13)) /= im or  imm13(0) = '1' then
      -- Impossible to encode.
    end if;
    funct7(funct7'high)                       := imm13(12);
    funct7(funct7'high - 1 downto funct7'low) := imm13(10 downto 5);
    rd(rd'high downto rd'low + 1)             := imm13(4 downto 1);
    rd(rd'low)                                := imm13(11);

    return instr;
  end function;

  function instr_u(cat : integer;
                   dst : integer; im : integer) return w32 is
    variable instr : w32;
--    alias special is instr(30);
    alias funct7  is instr(31 downto 25);
    alias imm20u  is instr(31 downto 12);
  begin
    instr  := instr_base(cat, 0, dst, 0, 0);
    imm20u := suit(im / 16#1000#, imm20u);
    if to_integer(unsigned(imm20u)) * 16#1000# /= im then
      -- Impossible to encode.
    end if;

    return instr;
  end function;

  function instr_j(cat : integer;
                   dst : integer; im : integer) return w32 is
    variable instr : w32;
--    alias special is instr(30);
    variable jump : w32;
  begin
    instr  := instr_base(cat, 0, dst, 0, 0);

    jump                := suit(im, jump);
    if to_integer(unsigned(jump)) /= im then
      -- Impossible to encode.
    end if;
    instr(31)           := jump(20);
    instr(30 downto 21) := jump(10 downto 1);
    instr(20)           := jump(11);
    instr(19 downto 12) := jump(19 downto 12);

    return instr;
  end function;

  function rom_value_1(addr : integer; cbase : integer := 0; dbase : integer := memory'low * 4) return w32 is
    variable incr   : integer := cbase;
    variable start  : integer;
    variable loop1  : integer;
    variable loop2  : integer;
    variable cont   : integer := cbase + 12;
  begin
    start := incr;
    if addr = incr then return instr_j(cat_jal, 5, cont - incr); end if;                 -- jal   d5, cont
    incr := incr + 4;
    if addr = incr then return x"04030201"; end if;
    incr := incr + 4;
    if addr = incr then return x"84838281"; end if;
    incr := incr + 4;
-- cont
    if addr = incr then return instr_i(cat_load, mem_b, 1, 5, 0); end if;                -- lb    d1, s5, 0
    incr := incr + 4;
    if addr = incr then return instr_i(cat_load, mem_b, 2, 5, 1); end if;                -- lb    d2, s5, + 1
    incr := incr + 4;
    if addr = incr then return instr_i(cat_load, mem_b, 3, 5, 6); end if;                -- lb    d3, s5, + 6
    incr := incr + 4;
    if addr = incr then return instr_i(cat_load, mem_b, 4, 5, 7); end if;                -- lb    d4, s5, + 7
    incr := incr + 4;
    if addr = incr then return instr_i(cat_load, mem_bu, 1, 5, 4); end if;               -- lbu   d1, s5, + 4
    incr := incr + 4;
    if addr = incr then return instr_i(cat_load, mem_bu, 2, 5, 5); end if;               -- lbu   d2, s5,  + 5
    incr := incr + 4;
    if addr = incr then return instr_i(cat_load, mem_bu, 3, 5, 2); end if;               -- lbu   d3, s5,  + 2
    incr := incr + 4;
    if addr = incr then return instr_i(cat_load, mem_bu, 4, 5, 3); end if;               -- lbu   d4, s5,  + 3
    incr := incr + 4;
    if addr = incr then return instr_i(cat_load, mem_h, 1, 5, 0); end if;                -- lh    d1, s5, + 0
    incr := incr + 4;
    if addr = incr then return instr_i(cat_load, mem_h, 2, 5, 6); end if;                -- lh    d2, s5, + 6
    incr := incr + 4;
    if addr = incr then return instr_i(cat_load, mem_hu, 1, 5, 4); end if;               -- lhu   d1, s5, + 4
    incr := incr + 4;
    if addr = incr then return instr_i(cat_load, mem_hu, 2, 5, 2); end if;               -- lhu   d2, s5, + 2
    incr := incr + 4;
    if addr = incr then return instr_i(cat_opi, op_add, 1, 0, 123); end if;              -- addi  d1, s0, 123
    incr := incr + 4;
    if addr = incr then return instr_i(cat_opi, op_sll, 2, 1, 2); end if;                -- slli  d2, s1, 2
    incr := incr + 4;
    if addr = incr then return instr_r(cat_op, op_slt, 3, 1, 2); end if;                 -- slt   d3, s1, s2
    incr := incr + 4;
    if addr = incr then return instr_r(cat_op, op_sltu, 4, 2, 1); end if;                -- slt   d4, s2, s1
    incr := incr + 4;
    if addr = incr then return instr_i(cat_opi, op_or, 6, 0, 1); end if;                 -- ori   d6, s0, 1
    incr := incr + 4;
    if addr = incr then return instr_r(cat_op, op_xor, 7, 2, 2); end if;                 -- xori  d7, s2, s2
    incr := incr + 4;
    loop1 := incr;
    if addr = incr then return instr_r(cat_op, op_add, 3, 3, 3); end if;                 -- add   d3, s3, s3
    incr := incr + 4;
    if addr = incr then return instr_i(cat_opi, op_add, 6, 6, -1); end if;               -- addi  d6, s6, -1
    incr := incr + 4;
    if addr = incr then return instr_b(cat_bxx, bxx_ne, 6, 7, loop1 - incr); end if;     -- bne   d6, d7, loop1
    incr := incr + 4;
    if addr = incr then return instr_u(cat_lui, 6, 16#1000#); end if;                    -- lui  d6, x1000
    incr := incr + 4;
    loop2 := incr;
    if addr = incr then return instr_i(cat_load, mem_b, 2, 6, 1); end if;                -- lb    d2, s6, x1001
    incr := incr + 4;
    if addr = incr then return instr_i(cat_opi, op_add, 1, 1, 60); end if;               -- addi  d1, d1, 60
    incr := incr + 4;
    if addr = incr then return instr_s(cat_store, mem_h, 6, 1, 0); end if;               -- sh    s1, s6, x1000
    incr := incr + 4;
    if addr = incr then return instr_i(cat_sys, sys_csrrw, 8, 1, addr_mscratch); end if; -- csrrw d8, s1, mscratch
    incr := incr + 4;
    if addr = incr then return instr_s(cat_store, mem_h, 6, 8, 2); end if;               -- sh    s8, s6, x1002
    incr := incr + 4;
    if addr = incr then return instr_j(cat_jal, 5, loop2 - incr); end if;                -- jal   d5, loop2
    incr := incr + 4;
--    if ext = addr then return

    return x"0000_0000";
  end function;

  -- Collatz
  function rom_value_2(addr : integer; cbase : integer := 0; dbase : integer := memory'low * 4) return w32 is
    constant xbase  : integer := cbase + 30 * 4;
    variable incr   : integer := cbase;
    variable start  : integer;
    variable loop1  : integer;
    variable even   : integer := 32;
  begin
    start := incr;
    if addr = incr then return instr_i(cat_opi, op_add, 3, 0, 0); end if;                -- addi  d3, s0, 0
    incr := incr + 4;
    if addr = incr then return instr_i(cat_opi, op_add, 1, 0, 1969); end if;             -- addi  d1, s0, 1969
    incr := incr + 4;
    loop1 := incr;
    if addr = incr then return instr_i(cat_opi, op_and, 2, 1, 1); end if;                -- andi  d2, s1, 1
    incr := incr + 4;
    if addr = incr then return instr_b(cat_bxx, bxx_eq, 2, 0, even - incr); end if;      -- beq   d2, s0, even
    incr := incr + 4;
    if addr = incr then return instr_r(cat_op, op_add, 2, 1, 1); end if;                 -- add  d2, s1, s1
    incr := incr + 4;
    if addr = incr then return instr_r(cat_op, op_add, 1, 2, 1); end if;                 -- add  d1, s2, s1
    incr := incr + 4;
    if addr = incr then return instr_i(cat_opi, op_add, 1, 1, 1); end if;                -- addi d1, d1, 1
    incr := incr + 4;
    if addr = incr then return instr_i(cat_op, op_add, 1, 1, 1); end if;                 -- add  d1, s1, s1
    incr := incr + 4;
-- even
    if addr = incr then return instr_i(cat_opi, op_srl, 1, 1, 1); end if;                -- srli  d1, s1, 1
    incr := incr + 4;
    if addr = incr then return instr_i(cat_opi, op_add, 3, 3, 1); end if;                -- addi  d3, s3, 1
    incr := incr + 4;
    if addr = incr then return instr_r(cat_opi, op_add, 2, 0, 1); end if;                -- addi  d2, s0, 1
    incr := incr + 4;
    if addr = incr then return instr_b(cat_bxx, bxx_ne, 2, 1, loop1 - incr); end if;     -- bne   d2, d1, loop
    incr := incr + 4;
    if addr = incr then return instr_i(cat_sys, sys_normal, 0, 0, sysn_ecall); end if;   -- ecall
    incr := incr + 4;
    if addr = incr then return instr_j(cat_jal, 4, start - incr); end if;                -- jal  d4, start - incr
    incr := incr + 4;

    incr := 16#100#;   -- qqq Same as mtvec!
    if addr = incr then return instr_i(cat_sys, sys_csrrw, 0, 1, addr_mscratch); end if; -- csrrw d0, s1, mscratch
    incr := incr + 4;
    if addr = incr then return instr_i(cat_sys, sys_csrrs, 1, 0, addr_mcause); end if;   -- csrrs d1, s0, mcause
    incr := incr + 4;
    if addr = incr then return instr_i(cat_sys, sys_csrrs, 1, 0, addr_mepc); end if;     -- csrrs d1, s0, mepc
    incr := incr + 4;
    if addr = incr then return instr_i(cat_opi, op_add, 1, 1, 4); end if;                -- addi  d1, s1, 4
    incr := incr + 4;
    if addr = incr then return instr_i(cat_sys, sys_csrrw, 0, 1, addr_mepc); end if;     -- csrrw d0, s1, mepc
    incr := incr + 4;
    if addr = incr then return instr_i(cat_sys, sys_csrrs, 1, 0, addr_mscratch); end if; -- csrrs d1, s0, mscratch
    incr := incr + 4;
    if addr = incr then return instr_i(cat_sys, sys_normal, 0, 0, sysn_mret); end if;  -- mret
    incr := incr + 4;

    return x"0000_0000";
  end function;

  function rom_value_3(addr : integer; cbase : integer := 0; dbase : integer := memory'low * 4) return w32 is
    variable incr   : integer := cbase;
    variable start  : integer;
    variable loop1  : integer;
    variable loop2  : integer;
    variable cont   : integer := cbase + 12;
  begin
    start := incr;
    if addr = incr then return instr_j(cat_jal, 5, cont - incr); end if;                 -- jal   d5, cont
    incr := incr + 4;
    if addr = incr then return x"04030201"; end if;
    incr := incr + 4;
    if addr = incr then return x"84838281"; end if;
    incr := incr + 4;
-- cont
    if addr = incr then return instr_i(cat_load, mem_w, 1, 5, 0); end if;                -- lh    d1, s5, + 0
    incr := incr + 4;
--    if addr = incr then return instr_i(cat_load, mem_h, 1, 5, 0); end if;                -- lh    d1, s5, + 0
--    incr := incr + 4;
--    if addr = incr then return instr_i(cat_load, mem_h, 2, 5, 6); end if;                -- lh    d2, s5, + 6
--    incr := incr + 4;
--    if addr = incr then return instr_i(cat_load, mem_hu, 1, 5, 4); end if;               -- lhu   d1, s5, + 4
--    incr := incr + 4;
--    if addr = incr then return instr_i(cat_load, mem_hu, 2, 5, 2); end if;               -- lhu   d2, s5, + 2
--    incr := incr + 4;
    if addr = incr then return instr_u(cat_lui, 6, 16#80002000#); end if;                    -- lui  d6, x2000
    incr := incr + 4;
    loop2 := incr;
    if addr = incr then return instr_s(cat_store, mem_h, 6, 1, 0); end if;               -- sh    s1, s6, x2000
    incr := incr + 4;
    if addr = incr then return instr_s(cat_store, mem_h, 6, 1, 2); end if;               -- sh    s1, s6, x2002
    incr := incr + 4;
    if addr = incr then return instr_s(cat_store, mem_h, 6, 1, 4); end if;               -- sh    s1, s6, x2004
    incr := incr + 4;
    if addr = incr then return instr_s(cat_store, mem_h, 6, 1, 5); end if;               -- sh    s1, s6, x2005
    incr := incr + 4;
    if addr = incr then return instr_s(cat_store, mem_h, 6, 1, 6); end if;               -- sh    s1, s6, x2006
    incr := incr + 4;
    if addr = incr then return instr_s(cat_store, mem_h, 6, 1, 7); end if;               -- sh    s1, s6, x2007
    incr := incr + 4;
    if addr = incr then return instr_s(cat_store, mem_w, 6, 1, 12); end if;              -- sw    s1, s6, x2012
    incr := incr + 4;
    if addr = incr then return instr_s(cat_store, mem_w, 6, 1, 13); end if;              -- sw    s1, s6, x2013
    incr := incr + 4;
    if addr = incr then return instr_s(cat_store, mem_w, 6, 1, 14); end if;              -- sw    s1, s6, x2014
    incr := incr + 4;
    if addr = incr then return instr_s(cat_store, mem_w, 6, 1, 15); end if;              -- sw    s1, s6, x2015
    incr := incr + 4;
    if addr = incr then return instr_i(cat_opi, op_add, 6, 6, 21); end if;                -- addi  d6, d6, 21
    incr := incr + 4;
    if addr = incr then return instr_j(cat_jal, 5, loop2 - incr); end if;                -- jal   d5, loop2
    incr := incr + 4;
--    if ext = addr then return

    return x"0000_0000";
  end function;

  function rom_value_4(addr : integer; cbase : integer := 0; dbase : integer := memory'low * 4) return w32 is
    variable incr   : integer := cbase;
    variable start  : integer;
    variable loop1  : integer;
    variable loop2  : integer;
    variable cont   : integer := cbase + 12;
  begin
    start := incr;
    if addr = incr then return instr_j(cat_jal, 5, cont - incr); end if;                 -- jal   d5, cont
    incr := incr + 4;
    if addr = incr then return x"04030201"; end if;
    incr := incr + 4;
    if addr = incr then return x"84838281"; end if;
    incr := incr + 4;
-- cont
    if addr = incr then return instr_i(cat_load, mem_h, 1, 5, 0); end if;                -- lh    d1, s5, + 0
    incr := incr + 4;
    if addr = incr then return instr_i(cat_load, mem_h, 1, 5, 1); end if;                -- lh    d1, s5, + 1
    incr := incr + 4;
    if addr = incr then return instr_i(cat_load, mem_h, 1, 5, 2); end if;                -- lh    d1, s5, + 2
    incr := incr + 4;
    if addr = incr then return instr_i(cat_load, mem_h, 1, 5, 3); end if;                -- lh    d1, s5, + 3
    incr := incr + 4;
    if addr = incr then return instr_i(cat_load, mem_w, 1, 5, 0); end if;                -- lh    d1, s5, + 0
    incr := incr + 4;
    if addr = incr then return instr_i(cat_load, mem_w, 1, 5, 1); end if;                -- lh    d1, s5, + 1
    incr := incr + 4;
    if addr = incr then return instr_i(cat_load, mem_w, 1, 5, 2); end if;                -- lh    d1, s5, + 2
    incr := incr + 4;
    if addr = incr then return instr_i(cat_load, mem_w, 1, 5, 3); end if;                -- lh    d1, s5, + 3
    incr := incr + 4;
    if addr = incr then return instr_i(cat_opi, op_add, 6, 6, 21); end if;               -- addi  d6, d6, 21
    incr := incr + 4;
    if addr = incr then return instr_j(cat_jal, 5, loop2 - incr); end if;                -- jal   d5, loop2
    incr := incr + 4;
--    if ext = addr then return

    return x"0000_0000";
  end function;

  function rom_value_5(addr : integer; cbase : integer := 0; dbase : integer := memory'low * 4) return w32 is
    variable incr   : integer := cbase;
    variable start  : integer;
    variable loop1  : integer;
    variable loop2  : integer;
    variable cont   : integer := cbase + 44;
  begin
    start := incr;
    if addr = incr then return instr_i(cat_opi, op_add, 1, 0, 0); end if;                -- addi  d1, s0, 0
    incr := incr + 4;
    if addr = incr then return instr_j(cat_jal, 5, cont - incr); end if;                 -- jal   d5, cont
    incr := incr + 4;
    if addr = incr then return instr_i(cat_sys, sys_csrrw, 0, 1, addr_mscratch); end if; -- csrrw d0, s1, mscratch
    incr := incr + 4;
    if addr = incr then return instr_i(cat_sys, sys_csrrs, 1, 0, addr_mepc); end if;     -- csrrs d1, s0, mepc
    incr := incr + 4;
--    if addr = incr then return instr_i(cat_opi, op_add, 1, 1, 4); end if;                -- addi  d1, s1, 4
--    incr := incr + 4;
    if addr = incr then return instr_i(cat_sys, sys_csrrw, 0, 1, addr_mepc); end if;     -- csrrw d0, s1, mepc
    incr := incr + 4;
    if addr = incr then return instr_i(cat_opi, op_add, 1, 0, 16#400#); end if;          -- addi  d1, s0, x400
    incr := incr + 4;
    if addr = incr then return instr_i(cat_op, op_add, 1, 1, 1); end if;                 -- add  d1, s1, s1
    incr := incr + 4;
    if addr = incr then return instr_i(cat_sys, sys_csrrc, 0, 1, addr_mip); end if;      -- csrrc d0, s1, mip
    incr := incr + 4;
    if addr = incr then return instr_i(cat_sys, sys_csrrs, 0, 1, addr_mie); end if;      -- csrrs d0, s1, mie
    incr := incr + 4;
    if addr = incr then return instr_i(cat_sys, sys_csrrs, 1, 0, addr_mscratch); end if; -- csrrs d1, s0, mscratch
    incr := incr + 4;
    if addr = incr then return instr_i(cat_sys, sys_normal, 0, 0, sysn_mret); end if;    -- mret
    incr := incr + 4;
-- cont
    if addr = incr then return instr_i(cat_sys, sys_csrrw, 0, 5, addr_mtvec); end if;    -- csrrw d0, s5, mtvec
    incr := incr + 4;
    if addr = incr then return instr_i(cat_opi, op_add, 2, 0, 16#400#); end if;          -- addi  d2, s0, x400
    incr := incr + 4;
    if addr = incr then return instr_i(cat_op, op_add, 2, 2, 2); end if;                 -- add  d2, s2, s2
    incr := incr + 4;
    if addr = incr then return instr_i(cat_sys, sys_csrrw, 0, 2, addr_mie); end if;      -- csrrw d0, s2, mie
    incr := incr + 4;
    if addr = incr then return instr_i(cat_opi, op_add, 1, 0, 0); end if;                -- addi  d1, s0, 0
    incr := incr + 4;
    loop1 := incr;
    if addr = incr then return instr_i(cat_opi, op_add, 1, 1, 1); end if;                -- addi  d1, s1, 1
    incr := incr + 4;
    if addr = incr then return instr_j(cat_jal, 5, loop1 - incr); end if;                -- jal   d5, loop1
    incr := incr + 4;
--    if ext = addr then return

    return x"0000_0000";
  end function;


  function rom_value(addr : integer; cbase : integer := 0; dbase : integer := memory'low * 4) return w32 is
  begin
    case test is
    when 1 => return rom_value_1(addr, cbase, dbase);
    when 2 => return rom_value_2(addr, cbase, dbase);
    when 3 => return rom_value_3(addr, cbase, dbase);
    when 4 => return rom_value_4(addr, cbase, dbase);
    when 5 => return rom_value_5(addr, cbase, dbase);
    when others => return x"0000_0000";
    end case;
  end function;


  signal irq_masked : std_logic := '0';

  signal ext_clk   : std_logic := '0';
  signal ext_rst   : std_logic := '1';
  signal ext_waddr : w32;
  signal ext_din   : w32;
  signal ext_we    : std_logic_vector(3 downto 0);
  signal ext_en    : std_logic;

begin

  ext_clk <= not ext_clk after 10 ns;

  clk   <= not clk after 5 ns;

  irq_masked <= irq when test = 5
           else '0';


  cpu: entity work.cpu
    generic map (
      boot_addr => boot_addr,
      exc_addr  => exc_addr,
      lattice   => lattice,
      ram_type  => ram_type,
      ram_depth => 32
    )
    port map (
      ext_addr => ext_addr,
      ext_r    => ext_r,
      ext_w    => ext_w,
      ext_be   => ext_be,

      ext_clk   => ext_clk,
      ext_rst   => ext_rst,
      ext_waddr => ext_waddr,
      ext_din   => ext_din,
      ext_we    => ext_we,
      ext_en    => ext_en,
-- valid?

      ext_out  => ext_out,

      irq      => irq_masked,

      clk      => clk,
      reset    => reset
    );


  interrupt: process
  begin
    wait for 341 ns;
    irq <= '1';
    wait for 3 ns;
    irq <= '0';
    wait for 500 ns;
    irq <= '1';
    wait for 12 ns;
    irq <= '0';
    wait;
  end process;


 ram_internal: if ram_type = 2 generate
  init: process
    procedure wait_for(n : integer) is
    begin
      for i in 1 to n loop
        wait until ext_clk = '1';
      end loop;
    end procedure;
  begin
    reset   <= '1';
    ext_rst <= '1';
    ext_we  <= (others => '0');
    ext_en  <= '0';
    wait_for(3);
    ext_rst <= '0';
    -- Program internal RAM.
    for i in 0 to memory'low - 1 loop
      ext_waddr <= suit(i * 4, ext_waddr);
      ext_din   <= rom_value(i * 4);
      ext_we    <= (others => '1');
      wait_for(1);
    end loop;
    ext_we <= (others => '0');
    wait_for(2);

    -- Let loose!
    reset  <= '0';
    wait;
  end process;
 end generate;


 ram_here: if ram_type = 990 generate
  ctrl_0: process
    procedure wait_for(n : integer) is
    begin
      for i in 1 to n loop
        wait until clk = '1';
      end loop;
    end procedure;
  begin
    reset <= '1';
    wait_for(5);
    reset <= '0';
    wait;
  end process;

  ram: process(clk)
    variable addr : integer;
    variable data : w32;
  begin
    if rising_edge(clk) then
      addr := to_integer(unsigned(ext_addr));
      if ext_be = "0000" then
        if addr < memory'low * 4 then
          if ext_addr(1 downto 0) /= "00" then
            -- Address error!
          end if;
          ext_r <= rom_value(addr);
        elsif addr <= memory'high * 4 then
          ext_r <= suit(memory(addr / 4), ext_r);
        else
          -- I/O
        end if;
      else
        if addr < memory'low * 4 then
          null;   -- Not allowed!
        elsif addr <= memory'high * 4 then
          data := suit(memory(addr / 4), data);
          for i in ext_be'range loop
            if ext_be(i) then
              data(i * 8 + 7 downto i * 8) := ext_w(i * 8 + 7 downto i * 8);
            end if;
          end loop;
          memory(addr / 4) <= suit(data, memory(memory'low));
        else
          -- I/O
        end if;
      end if;
    end if;
  end process;
 end generate;


 ram_here2: if ram_type = 0 generate
  ctrl_0: process
    procedure wait_for(n : integer) is
    begin
      for i in 1 to n loop
        wait until clk = '1';
      end loop;
    end procedure;
  begin
--    reset <= '1';
--    wait_for(5);
--    reset <= '0';
    wait;
  end process;

  ram: process
    subtype string16 is string(1 to 16);
    function str16(txt : string) return string16 is
      variable longer : string16 := (others => NUL);
    begin
      longer(16) := character'val(txt'length);
      longer(txt'range) := txt;
      return longer;
    end function;
    function str(txt : string16) return string is
      variable shorter : string(1 to character'pos(txt(16)));
    begin
      shorter := txt(shorter'range);

      return shorter;
    end function;
    constant code_offset   : integer := to_integer(boot_addr(boot_addr'high downto 2));
    constant data1k_addr   : u32 := boot_addr + 16#1000#;
    constant data2k_addr   : u32 := boot_addr + 16#2000#;
    constant data1k_offset : integer := to_integer(data1k_addr(data1k_addr'high downto 2));
    constant data2k_offset : integer := to_integer(data2k_addr(data2k_addr'high downto 2));
--    constant code_offset   : integer := 0;
--    constant data1k_offset : integer := 32 * 3;
--    constant data2k_offset : integer := 32 * 3;
    variable code_ram   : mem_t(0 to 1024);
    variable data_ram1k : mem_t(0 to 127);
    variable data_ram2k : mem_t(0 to 255);
    variable addr : integer;
    variable pos  : integer;
    variable data : w32;
    type     IntFile is file of integer;
    file     datafile : IntFile;
    variable filedata : integer;
    variable i : integer;
    variable sig_start : integer;
    variable sig_end : integer;
    variable outline : line;
    type tests_t is array (integer range <>) of string16;
    variable tests : tests_t(1 to 55) := (
      str16("ADD"), str16("ADDI"), str16("SUB"),
      str16("AND"), str16("ANDI"),
      str16("AUIPC"),
      str16("BEQ"), str16("BNE"),
      str16("BGE"), str16("BGEU"), str16("BLT"), str16("BLTU"),
      str16("CSRRC"), str16("CSRRCI"), str16("CSRRS"), str16("CSRRSI"), str16("CSRRW"), str16("CSRRWI"),
      str16("DELAY_SLOTS"),
      str16("EBREAK"), str16("ECALL"),
      str16("ENDIANESS"),
      str16("FENCE.I"),
      str16("IO"),
      str16("JAL"), str16("JALR"),
      str16("LB"), str16("LBU"), str16("LH"), str16("LHU"), str16("LW"),
      str16("LUI"),
      str16("MISALIGN_JMP"), str16("MISALIGN_LDST"),
      str16("NOP"),
      str16("OR"), str16("ORI"),
      str16("RF_size"), str16("RF_width"), str16("RF_x0"),
      str16("SB"), str16("SH"), str16("SW"),
      str16("SLL"), str16("SLLI"), str16("SRL"), str16("SRLI"), str16("SRA"), str16("SRAI"),
      str16("SLT"), str16("SLTI"), str16("SLTIU"), str16("SLTU"),
      str16("XOR"), str16("XORI")
    );
    variable test_no : integer;
  begin
   test_no := tests'low;
   do_tests: loop 
    reset <= '1';

    if test < 10 then
      populate: for i in code_ram'range loop
        data := suit(rom_value(i * 4 + code_offset * 4, code_offset * 4, data1k_offset * 4), data);
        if data = 32x"0" then
          exit populate;
        end if;
        code_ram(i) := suit(data, code_ram(0));
      end loop;
    else

      write(outline, "Testing " & str(tests(test_no)));
      writeline(output, outline);
      if str(tests(test_no)) /= "IO" then
        File_Open(datafile, path & "compliance\rv32i\I-" & str(tests(test_no)) & "-01.elf.cbin");
      else
        File_Open(datafile, path & "compliance\rv32i\I-" & str(tests(test_no)) & ".elf.cbin");
      end if;
      i := 0;
      while not EndFile(datafile) loop
        Read(datafile, filedata);
        data := suit(filedata, data);
        code_ram(i) := suit(data, code_ram(0));
        i := i + 1;
      end loop;
      File_Close(datafile);

      if str(tests(test_no)) /= "IO" then
        File_Open(datafile, path & "compliance\rv32i\I-" & str(tests(test_no)) & "-01.elf.dbin");
      else
        File_Open(datafile, path & "compliance\rv32i\I-" & str(tests(test_no)) & ".elf.dbin");
      end if;
      sig_start := -1;
      i := 0;
      while not EndFile(datafile) loop
        Read(datafile, filedata);
        data := suit(filedata, data);
        -- Signature starts at the last place where everything else is all 0xffffffff.
        if sig_start = -1 and data = x"ffff_ffff" then
          sig_start := i;
        elsif data /= x"ffff_ffff" then
          sig_start := -1;
        end if;
        data_ram2k(i) := suit(data, data_ram2k(0));
        sig_end := i;
        i := i + 1;
      end loop;
      File_Close(datafile);
    end if;

--    wait_for(5);
    wait until clk = '1';
    wait until clk = '1';
    wait until clk = '1';
    reset <= '0';

    forever: loop
      wait until clk = '1';

      if ext_addr(1 downto 0) /= "00" then
        -- Address error!
      end if;
      addr := to_integer(unsigned(ext_addr(ext_addr'high downto 2)));

      if addr >= data2k_offset then
        pos := addr - data2k_offset;
        if pos > data_ram2k'high then
          -- Address error!
        end if;
        data := suit(data_ram2k(pos), ext_r);
      elsif addr >= data1k_offset then
        pos := addr - data1k_offset;
        if pos > data_ram1k'high then
          -- Address error!
        end if;
        data := suit(data_ram1k(pos), ext_r);
      elsif addr >= code_offset then
        pos := addr - code_offset;
        if pos > code_ram'high then
          -- Address error!
        end if;
        data := suit(code_ram(pos), ext_r);
      else
        -- I/O
      end if;

      if ext_be = "0000" then
        ext_r <= suit(data, ext_r);
      else
        for i in ext_be'range loop
          if ext_be(i) then
            data(i * 8 + 7 downto i * 8) := ext_w(i * 8 + 7 downto i * 8);
          end if;
        end loop;

        if addr >= data2k_offset then
          data_ram2k(pos) := suit(data, data_ram2k(0));
        elsif addr >= data1k_offset then
--          data_ram1k(pos) := suit(data, data_ram1k(0));
          assert to_integer(unsigned(ext_w)) = 1
            report "Bad finish!"
            severity failure;
          i := sig_start;
          signature: loop
            if data_ram2k(i)(0) = 'X' then
              exit signature;
            end if;
            write(outline, to_hstring(data_ram2k(i + 3)) &
                           to_hstring(data_ram2k(i + 2)) &
                           to_hstring(data_ram2k(i + 1)) &
                           to_hstring(data_ram2k(i + 0)));
            writeline(output, outline);
            i := i + 4;
            if i > sig_end then
              exit signature;
            end if;
          end loop;
          exit forever;
        elsif addr >= code_offset then
--          null;   -- qqq Not allowed?
          code_ram(pos) := suit(data, code_ram(0));
        else
          -- I/O
        end if;
      end if;
    end loop;

    test_no := test_no + 1;
    if test_no > tests'high then
      exit do_tests;
    end if;
   end loop;
--   wait;
   assert false
     report "Done!"
     severity failure;
  end process;
 end generate;

end architecture;
