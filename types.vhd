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


package types is

  subtype u8  is unsigned(7 downto 0);
  subtype w8  is std_logic_vector(7 downto 0);
  subtype u32 is unsigned(31 downto 0);
  subtype w32 is std_logic_vector(31 downto 0);
  subtype u64 is unsigned(63 downto 0);

end package types;

package body types is

end package body types;
