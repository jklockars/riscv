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

entity cpu_top is
  generic (
    boot_addr : u32     := 32x"0";
    exc_addr  : u32     := 32x"100";
    ram_depth : integer := 1024;
    init_file : string  := ""
  );
  port (
    -- Dummy output to make sure everything is not optimized out.
    ext_out   : out w8;

    irq   : in  std_logic := '0';   -- qqq Synchronize this!

    clk   : in  std_logic;
    reset : in  std_logic
  );
end cpu_top;


architecture rtl of cpu_top is

  signal ext_addr  : w32;
  signal ext_r     : w32;
  signal ext_w     : w32;
  signal ext_be    : std_logic_vector(3 downto 0) := (others => '0');
  signal ext_clk   : std_logic;
  signal ext_rst   : std_logic;
  signal ext_waddr : w32;
  signal ext_din   : w32;
  signal ext_we    : std_logic_vector(3 downto 0);
  signal ext_en    : std_logic;
  signal ext_valid : std_logic                    := '1';

begin

  cpu_1: entity work.cpu
    generic map (
      boot_addr => boot_addr,
      exc_addr  => exc_addr,
      lattice   => true,
      ram_type  => 1,
      ram_depth => ram_depth,
      init_file => init_file)
    port map (
      ext_addr  => ext_addr,
      ext_r     => ext_r,
      ext_w     => ext_w,
      ext_be    => ext_be,
      ext_clk   => ext_clk,
      ext_rst   => ext_rst,
      ext_waddr => ext_waddr,
      ext_din   => ext_din,
      ext_we    => ext_we,
      ext_en    => ext_en,
      ext_valid => ext_valid,
      ext_out   => ext_out,
      irq       => irq,
      clk       => clk,
      reset     => reset
      );

end architecture;
