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

--  Xilinx True Dual Port RAM No Change Dual Clock
--  This code implements a parameterizable true dual port memory (both ports can read and write).
--  This is a no change RAM which retains the last read value on the output during writes
--  which is the most power efficient mode.
--  If a reset or enable is not necessary, it may be tied off or removed from the code.

-- In simple dual port, Port A is only write and Port B is only read.
--  Xilinx Simple Dual Port 2 Clock RAM with Byte-write
--  This code implements a parameterizable SDP dual clock memory.
--  If a reset or enable is not necessary, it may be tied off or removed from the code.
-- Note :
-- If the chosen width and depth values are low, Synthesis will infer Distributed RAM.
-- C_RAM_DEPTH should be a power of 2

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

library work;
use work.common.all;


entity dpram is
  generic (
    ram_depth       : integer := 0;                  -- Specify RAM depth (number of entries)
    ram_performance : string := "HIGH_PERFORMANCE";  -- Select "HIGH_PERFORMANCE" or "LOW_LATENCY" 
    init_file       : string := ""                   -- Specify name/location of RAM initialization file if using one (leave blank if not)
  );
  port (
    -- Port A
    clka   : in  std_logic;             -- Port A Clock
    rsta   : in  std_logic;             -- Port A Output reset (does not affect memory contents)
    addra  : in  std_logic_vector;      -- Port A address bus, width determined from RAM_DEPTH
    dina   : in  std_logic_vector;      -- Port A RAM input data
    wea    : in  std_logic_vector;      -- Port A Write enable
    ena    : in  std_logic;             -- Port A RAM Enable, for additional power savings, disable port when not in use
    regcea : in  std_logic;             -- Port A Output register enable
    douta  : out std_logic_vector;      -- Port A RAM output data

    -- Port B
    clkb   : in  std_logic;             -- Port B Clock
    rstb   : in  std_logic;             -- Port B Output reset (does not affect memory contents)
    addrb  : in  std_logic_vector;      -- Port B address bus, width determined from RAM_DEPTH
    dinb   : in  std_logic_vector;      -- Port B RAM input data
    web    : in  std_logic_vector;      -- Port B Write enable
    enb    : in  std_logic;             -- Port B RAM Enable, for additional power savings, disable port when not in use
    regceb : in  std_logic;             -- Port B Output register enable
    doutb  : out std_logic_vector       -- Port B RAM output data
  );
end dpram;

architecture rtl of dpram is

  --  The following function calculates the address width based on specified RAM depth
  function clogb2(depth : natural) return integer is
    variable temp    : integer := depth;
    variable ret_val : integer := 0;
  begin
    while temp > 1 loop
      ret_val := ret_val + 1;
      temp    := temp / 2;
    end loop;
    
    return ret_val;
  end function;

  function get_depth(depth : integer; addra_bits : integer; addrb_bits : integer) return integer is
    variable addr_bits : integer := addra_bits;
  begin
    if depth /= 0 then
      return 2 ** clogb2(depth);
    end if;

    if addrb_bits > addra_bits then
      addr_bits := addrb_bits;
    end if;

    return 2 ** addr_bits;
  end function;

  constant C_RAM_DEPTH : integer := get_depth(RAM_DEPTH, addra'length, addrb'length);
  constant C_RAM_WIDTH : integer := dina'length;  
  constant C_NB_COL    : integer := wea'length;               -- Specify number of colums (number of bytes)
  constant C_COL_WIDTH : integer := C_RAM_WIDTH / C_NB_COL;   -- Specify column width (byte width, typically 8 or 9)

--  assert C_NB_COL * C_COL_WIDTH = C_RAM_WIDTH;  
--  assert dina'length  = C_RAM_WIDTH;
--  assert dinb'length  = C_RAM_WIDTH;
--  assert douta'length = C_RAM_WIDTH;
--  assert doutb'length = C_RAM_WIDTH;




  signal douta_reg : std_logic_vector(douta'length - 1 downto 0) := (others => '0');           -- Port A RAM output data when RAM_PERFORMANCE = HIGH_PERFORMANCE
  signal doutb_reg : std_logic_vector(doutb'length - 1 downto 0) := (others => '0');

  type ram_type is array (C_RAM_DEPTH - 1 downto 0) of std_logic_vector (doutb_reg'range);    -- 2D Array Declaration for RAM signal

  signal ram_data_a : std_logic_vector(douta_reg'range);
  signal ram_data_b : std_logic_vector(doutb_reg'range);

-- The folowing code either initializes the memory values to a specified file or to all zeros to match hardware

  impure function initramfromfile (ramfilename : in string) return ram_type is
    file ramfile         : text is in ramfilename;
    variable ramfileline : line;
    variable ram_name    : ram_type;
    variable bitvec      : bit_vector(C_RAM_WIDTH - 1 downto 0);
  begin
    for i in ram_type'range loop
      readline(ramfile, ramfileline);
      read(ramfileline, bitvec);
      ram_name(i) := to_stdlogicvector(bitvec);
    end loop;

    return ram_name;
  end function;

  impure function init_from_file_or_zeroes(ramfile : string) return ram_type is
  begin
    if ramfile /= "" then
      return InitRamFromFile(ramfile);
    else
      return (others => (others => '0'));
    end if;
  end;

  -- Define RAM
--  signal ram_name : ram_type := init_from_file_or_zeroes(INIT_FILE);
  shared variable ram_name : ram_type := init_from_file_or_zeroes(INIT_FILE);

begin

  process(clka)
  begin
    if rising_edge(clka) then
      if is_1(ena) then
        ram_data_a <= ram_name(to_integer(unsigned(addra)));
      end if;    -- ?
        for i in  0 to C_NB_COL - 1 loop
          if is_1(wea(i)) then
            ram_name(to_integer(unsigned(addra)))(((i + 1) * C_COL_WIDTH) - 1 downto i * C_COL_WIDTH) := dina(((i + 1) * C_COL_WIDTH) - 1 downto i * C_COL_WIDTH);
          end if;
        end loop;
--      end if;
    end if;
  end process;

  process(clkb)
  begin
    if rising_edge(clkb) then
      if is_1(enb) then
        ram_data_b <= ram_name(to_integer(unsigned(addrb)));
      end if;   -- ?
        for i in  0 to C_NB_COL - 1 loop
          if is_1(web(i)) then
            ram_name(to_integer(unsigned(addrb)))(((i + 1) * C_COL_WIDTH) - 1 downto i * C_COL_WIDTH) := dinb(((i + 1) * C_COL_WIDTH) - 1 downto i * C_COL_WIDTH);
          end if;
        end loop;
--      end if;
    end if;
  end process;

  --  Following code generates LOW_LATENCY (no output register)
  --  Following is a 1 clock cycle read latency at the cost of a longer clock-to-out timing
  no_output_register : if RAM_PERFORMANCE = "LOW_LATENCY" generate
    douta <= ram_data_a;
    doutb <= ram_data_b;
  end generate;

  --  Following code generates HIGH_PERFORMANCE (use output register)
  --  Following is a 2 clock cycle read latency with improved clock-to-out timing
  output_register : if RAM_PERFORMANCE = "HIGH_PERFORMANCE"  generate
--    output_a : if DUAL_OUTPUT generate
      process(clka)
      begin
        if rising_edge(clka) then
          if is_1(rsta) then
            douta_reg <= (others => '0');
          elsif is_1(regcea) then
            douta_reg <= ram_data_a;
          end if;
        end if;
      end process;
--    end generate;

    process(clkb)
    begin
      if rising_edge(clkb) then
        if is_1(rstb) then
          doutb_reg <= (others => '0');
        elsif is_1(regceb) then
          doutb_reg <= ram_data_b;
        end if;
      end if;
    end process;

    douta <= douta_reg;
    doutb <= doutb_reg;
  end generate;

end architecture;

							
