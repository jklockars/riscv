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
use ieee.math_real.all;

library work;
use work.types.all;


package common is

  -- To std_ulogic
  function suit(constant src : std_ulogic;
                constant dst : std_ulogic) return std_ulogic;
  function suit(constant src : std_ulogic_vector;
                constant dst : std_ulogic) return std_ulogic;

  -- To std_ulogic_vector
  function suit(constant src : std_ulogic;
                constant dst : std_ulogic_vector) return std_ulogic_vector;
  function suit(constant src : std_ulogic_vector;
                constant dst : std_ulogic_vector) return std_ulogic_vector;
  function suit(constant src : signed;
                constant dst : std_ulogic_vector) return std_ulogic_vector;
  function suit(constant src : unsigned;
                constant dst : std_ulogic_vector) return std_ulogic_vector;
  function suit(constant src : integer;
                constant dst : std_ulogic_vector) return std_ulogic_vector;

  -- To unsigned
  function suit(constant src : std_ulogic_vector;
                constant dst : unsigned) return unsigned;
  function suit(constant src : signed;
                constant dst : unsigned) return unsigned;
  function suit(constant src : unsigned;
                constant dst : unsigned) return unsigned;
  function suit(constant src : integer;
                constant dst : unsigned) return unsigned;

  -- To signed
  function suit(constant src : std_ulogic_vector;
                constant dst : signed) return signed;
  function suit(constant src : signed;
                constant dst : signed) return signed;
  function suit(constant src : unsigned;
                constant dst : signed) return signed;
  function suit(constant src : integer;
                constant dst : signed) return signed;

  -- To integer
  function suit(constant src : std_ulogic_vector;
                constant dst : integer) return integer;


  function is_1(v : std_logic) return boolean;

end package common;

package body common is

  function is_1(v : std_logic) return boolean is
  begin
    return v = '1';
  end function;


  function suit(constant src : std_ulogic;
                constant dst : std_ulogic) return std_ulogic is
  begin
    return src;
  end function;


  function suit(constant src : std_ulogic_vector;
                constant dst : std_ulogic) return std_ulogic is
  begin
    return src(src'right);
  end function;


  function suit(constant src : std_ulogic;
                constant dst : std_ulogic_vector) return std_ulogic_vector is
    variable ret   : std_ulogic_vector(dst'range);
  begin
    ret            := (others => '0');
    ret(ret'right) := src;
    return ret;
  end function;


  function suit(constant src : std_ulogic_vector;
                constant dst : std_ulogic_vector) return std_ulogic_vector is
    variable src_n : std_ulogic_vector(src'length - 1 downto 0);
    variable dst_n : std_ulogic_vector(dst'length - 1 downto 0);
    variable ret   : std_ulogic_vector(dst'range);
  begin
    if src'length < dst'length then
      dst_n := (others => '0');
      dst_n(src'length - 1 downto 0) := src;
    else
      src_n := src;
      dst_n := src_n(dst_n'range);
    end if;
    ret := dst_n;
    return ret;
  end function;


  function suit(constant src : unsigned;
                constant dst : std_ulogic_vector) return std_ulogic_vector is
  begin
    return suit(std_ulogic_vector(src), dst);
  end function;


  function suit(constant src : signed;
                constant dst : std_ulogic_vector) return std_ulogic_vector is
    variable dst_n : std_ulogic_vector(dst'length - 1 downto 0);
    variable ret   : std_ulogic_vector(dst'range);
  begin
    if src'length < dst'length then
      dst_n := (others => src(src'left));
      dst_n(src'length - 1 downto 0) := std_ulogic_vector(src);
      ret := dst_n;
    else
      ret := suit(std_ulogic_vector(src), dst);
    end if;
    return ret;
  end function;


  function suit(constant src : integer;
                constant dst : std_ulogic_vector) return std_ulogic_vector is
    variable ret   : std_ulogic_vector(dst'range);
    variable src_n : std_ulogic_vector(31 downto 0);
    variable dst_n : std_ulogic_vector(dst'length - 1 downto 0);
  begin
    src_n := std_ulogic_vector(to_signed(src, src_n'length));
    if dst_n'length > src_n'length then
      dst_n := (others => '0');
      dst_n(src_n'range) := src_n;
    else
      dst_n := src_n(dst_n'range);
    end if;
    ret := dst_n;
    return ret;
  end function;


  function suit(constant src : std_ulogic_vector;
                constant dst : unsigned) return unsigned is
  begin
    return unsigned(suit(src, std_ulogic_vector(dst)));
  end function;


  function suit(constant src : signed;
                constant dst : unsigned) return unsigned is
  begin
-- synthesis translate_off
    for i in src'range loop
      if src(i) = 'U' then
        return (dst'range => 'U');
      end if;
      if src(i) = 'X' then
        return (dst'range => 'X');
      end if;
    end loop;
-- synthesis translate_on
--    assert src >= 0
--      report "Attempt to convert negative number to unsigned."
--      severity failure;
    return unsigned(suit(src, std_ulogic_vector(dst)));
  end function;


  function suit(constant src : unsigned;
                constant dst : unsigned) return unsigned is
  begin
    return unsigned(suit(src, std_ulogic_vector(dst)));
  end function;


  function suit(constant src : integer;
                constant dst : unsigned) return unsigned is
  begin
    assert src >= 0
      report "Attempt to convert negative number to unsigned."
      severity failure;
    return unsigned(suit(src, std_ulogic_vector(dst)));
  end function;


  function suit(constant src : std_ulogic_vector;
                constant dst : signed) return signed is
  begin
    return signed(suit(src, std_ulogic_vector(dst)));
  end function;


  function suit(constant src : signed;
                constant dst : signed) return signed is
    variable v : std_logic_vector(dst'length - 1 downto 0);
  begin
    if src'length >= dst'length then
      v := suit(src, v);
    else
      v := (others => src(src'high));
      v(src'length - 1 downto 0) := std_logic_vector(src);
    end if;
    return suit(v, dst);
  end function;


  function suit(constant src : unsigned;
                constant dst : signed) return signed is
  begin
    return signed(suit(src, std_ulogic_vector(dst)));
  end function;


  function suit(constant src : integer;
                constant dst : signed) return signed is
    variable ret : signed(dst'length - 1 downto 0);
  begin
    ret := signed(suit(src, std_ulogic_vector(ret)));
    if ret'length > 32 then
      ret(ret'high downto 32) := (others => ret(31));
    end if;
    return ret;
  end function;


  function suit(constant src : std_ulogic_vector;
                constant dst : integer) return integer is
    variable v : w32;
  begin
    assert src'length <= 32
      report "Attempt to convert too long vector to integer."
      severity failure;
    v := suit(src, v);
    return to_integer(signed(src));
  end function;

end package body common;
