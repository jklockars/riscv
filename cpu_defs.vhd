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


package cpu_defs is

  constant width_32  : integer := 3;  -- Bottom two bits of opcode word
  
  constant cat_load  : integer :=  0;
  constant cat_fence : integer :=  3;  -- No implementation
  constant cat_opi   : integer :=  4;
  constant cat_auipc : integer :=  5;
  constant cat_opiw  : integer :=  6;  -- 64 bit, illegal
  constant cat_illegal0 : integer :=  7;  -- Right?
  constant cat_store : integer :=  8;  -- TODO
  constant cat_op    : integer := 12;
  constant cat_lui   : integer := 13;
  constant cat_opw   : integer := 14;  -- 64 bit, illegal
  constant cat_illegal1 : integer := 15;  -- Right?
  constant cat_illegal2 : integer := 23;  -- Right?
  constant cat_bxx   : integer := 24;
  constant cat_jalr  : integer := 25;
  constant cat_jal   : integer := 27;
  constant cat_sys : integer := 28;  -- TODO
  constant cat_illegal3 : integer := 31;  -- Right?
  
  constant op_add  : integer := 0;  -- w iw 31-25 = 0
--  constant op_sub  : integer := 0;  -- w 31-25 = 32
  constant op_sll  : integer := 1;  -- w iw
  constant op_slt  : integer := 2;
  constant op_sltu : integer := 3;
  constant op_xor  : integer := 4;
  constant op_srl  : integer := 5;  -- w iw 31-25 = 0
--  constant op_sra  : integer := 5;  -- w iw 31-25 = 32
  constant op_or   : integer := 6;
  constant op_and  : integer := 7;

  constant bxx_eq  : integer := 0;
  constant bxx_ne  : integer := 1;
  constant bxx_lt  : integer := 4;
  constant bxx_ge  : integer := 5;
  constant bxx_ltu : integer := 6;
  constant bxx_geu : integer := 7;

  constant mem_b  : integer := 0;  -- store
  constant mem_h  : integer := 1;  -- store
  constant mem_w  : integer := 2;  -- store
  constant mem_d  : integer := 3;  -- store
  constant mem_bu : integer := 4;
  constant mem_hu : integer := 5;
  constant mem_wu : integer := 6;

  constant fence_d : integer := 0;  -- No implementation
  constant fence_i : integer := 1;  -- No implementation

  -- TODO?
  constant sys_normal : integer := 0;
  constant sys_csrrw  : integer := 1;
  constant sys_csrrs  : integer := 2;
  constant sys_csrrc  : integer := 3;
  constant sys_csrrwi : integer := 5;
  constant sys_csrrsi : integer := 6;
  constant sys_csrrci : integer := 7;

  -- TODO?
  constant sysn_ecall  : integer := 0;
  constant sysn_ebreak : integer := 1;
  constant sysn_uret   : integer := 2;        -- No implementation
  constant sysn_sret   : integer := 16#102#;  -- No implementation
  constant sysn_mret   : integer := 16#302#;
  constant sysn_dret   : integer := 16#7b2#;  -- No implementation
  constant sysn_sfen   : integer := 9;        -- qqq ???
  constant sysn_wfi    : integer := 16#105#;

  -- CSR
  -- Top two bits mean:
  -- 11 Read only
  -- Next two bits mean:
  -- 00 user, 01 system, 10 reserved, 11 machine

  -- Do no worry about user or system CSR for now.

  constant addr_mstatus    : integer := 16#300#;
  constant addr_misa       : integer := 16#301#;
  constant addr_medeleg    : integer := 16#302#;  -- Do not need to exist without u/s
  constant addr_mideleg    : integer := 16#303#;  -- Do not need to exist without u/s
  constant addr_mie        : integer := 16#304#;
  constant addr_mtvec      : integer := 16#305#;
  constant addr_mcounteren : integer := 16#306#;

  --x323 to x33f unused machine perf mon event sel  -- Allowed to be all 0

  constant addr_mscratch   : integer := 16#340#;
  constant addr_mepc       : integer := 16#341#;
  constant addr_mcause     : integer := 16#342#;
  constant addr_mtval      : integer := 16#343#;
  constant addr_mip        : integer := 16#344#;
  
  constant addr_pmpcfg0    : integer := 16#3a0#;
  constant addr_pmpcfg1    : integer := 16#3a1#;
  constant addr_pmpcfg2    : integer := 16#3a2#;
  constant addr_pmpcfg3    : integer := 16#3a3#;
  constant addr_pmpaddr0   : integer := 16#3b0#;
  constant addr_pmpaddr1   : integer := 16#3b1#;
  constant addr_pmpaddr2   : integer := 16#3b2#;
  constant addr_pmpaddr3   : integer := 16#3b3#;
  constant addr_pmpaddr4   : integer := 16#3b4#;
  constant addr_pmpaddr5   : integer := 16#3b5#;
  constant addr_pmpaddr6   : integer := 16#3b6#;
  constant addr_pmpaddr7   : integer := 16#3b7#;
  constant addr_pmpaddr8   : integer := 16#3b8#;
  constant addr_pmpaddr9   : integer := 16#3b9#;
  constant addr_pmpaddr10  : integer := 16#3ba#;
  constant addr_pmpaddr11  : integer := 16#3bb#;
  constant addr_pmpaddr12  : integer := 16#3bc#;
  constant addr_pmpaddr13  : integer := 16#3bd#;
  constant addr_pmpaddr14  : integer := 16#3be#;
  constant addr_pmpaddr15  : integer := 16#3bf#;
--  x7a0 tselect debug
--  x7a1 tdata1 debug
--  x7a2 tdata1 debug
--  x7a3 tdata1 debug
--  x7b0 dcsr debug
--  x7b1 dpc debug
--  x7b2 dscratch debug
  constant addr_mcycle     : integer := 16#b00#;
  constant addr_minstret   : integer := 16#b02#;
--xb03 to xb1f unused machine perofmrna monitor counter
  constant addr_mcycleh    : integer := 16#b80#;
  constant addr_minstreth  : integer := 16#b82#;

--xb83 to xb9f unused machine performance monitor counter

  constant addr_mvendorid  : integer := 16#f11#;  -- These are all read only (top two bits 11)
  constant addr_marchid    : integer := 16#f12#;
  constant addr_mimpid     : integer := 16#f13#;
  constant addr_mhartid    : integer := 16#f14#;

  -- These are memory mapped!
--  constant addr_time       : integer := ;
--  constant addr_time_h     : integer := ;

  -- Exceptions

  constant irq_u_soft         : integer := 0;
  constant irq_s_soft         : integer := 1;
  constant irq_m_soft         : integer := 3;
  constant irq_u_timer        : integer := 4;
  constant irq_s_timer        : integer := 5;
  constant irq_m_timer        : integer := 7;
  constant irq_u_external     : integer := 8;
  constant irq_s_external     : integer := 9;
  constant irq_m_external     : integer := 11;
  constant exc_i_addr_align   : integer := 0;
  constant exc_i_addr_access  : integer := 1;
  constant exc_illegal        : integer := 2;
  constant exc_breakpoint     : integer := 3;
  constant exc_l_addr_align   : integer := 4;
  constant exc_l_add_access   : integer := 5;
  constant exc_s_addr_align   : integer := 6;
  constant exc_s_add_access   : integer := 7;
  constant exc_u_ecall        : integer := 8;
  constant exc_s_ecall        : integer := 9;
  constant exc_m_ecall        : integer := 11;
  constant exc_i_page_fault   : integer := 12;
  constant exc_l_page_fault   : integer := 13;
  constant exc_s_page_fault   : integer := 15;

  constant exc_impossible    : integer := 0;
  constant exc_illegal_bxx   : integer := 2;
  constant exc_illegal_load  : integer := 2;
  constant exc_illegal_store : integer := 2;
  constant exc_illegal_cat   : integer := 2;
  constant exc_64_bit        : integer := 2;

  -- WIRI Reserved writes ignored, reads ignore values
  -- WPRI Reserved writes preserve values, reads ignore values
  -- WLRL Write/read only legal values
  -- WARL Write any values, reads legal values


  type op_t is (LB_op, LH_op, LW_op, LBU_op, LHU_op, Lxx_op,
                ADDI_op, SLLI_op, SLTI_op, SLTUI_op, XORI_op, SRLI_op, ORI_op, ANDI_op, OPIxx_op,
                AUIPC_op,
                SB_op, SH_op, SW_op, Sxx_op,
                ADD_op, SLL_op, SLT_op, SLTU_op, XOR_op, SRL_op, OR_op, AND_op, OPxx_op,
                LUI_op,
                BEQ_op, BNE_op, BLT_op, BGE_op, BLTU_op, BGEU_op, Bxx_op,
                JALR_op,
                JAL_op,
                ECALL_op, EBREAK_op, MRET_op, WFI_op, SYSNxx_op,
                CSRRW_op, CSRRS_op, CSRRC_op, CSRRWI_op, CSRRSI_op, CSRRCI_op, SYSxx_op,
                FENCE_op,
                xx_op);

  function to_opname(opcode : integer; funct3 : integer; funct7 : integer; funct12 : integer) return op_t;

end package cpu_defs;

package body cpu_defs is
  function to_opname(opcode : integer; funct3 : integer; funct7 : integer; funct12 : integer) return op_t is
  begin
    case opcode is
    when cat_load =>
      case funct3 is
      when mem_b  => return lb_op;
      when mem_h  => return lh_op;
      when mem_w  => return lw_op;
      when mem_bu => return lbu_op;
      when mem_hu => return lhu_op;
      when others => return lxx_op;
      end case;
    when cat_opi =>
      case funct3 is
      when op_add  => return addi_op;   -- sub?
      when op_sll  => return slli_op;
      when op_slt  => return slti_op;
      when op_sltu => return sltui_op;
      when op_xor  => return xori_op;
      when op_srl  => return srli_op;   -- sra?
      when op_or   => return ori_op;
      when op_and  => return andi_op;
      when others  => return opixx_op;
      end case;
    when cat_auipc => return auipc_op;
    when cat_store =>
      case funct3 is
      when mem_b  => return sb_op;
      when mem_h  => return sh_op;
      when mem_w  => return sw_op;
      when others => return sxx_op;
      end case;
    when cat_op =>
      case funct3 is
      when op_add  => return add_op;   -- do sub
      when op_sll  => return sll_op;
      when op_slt  => return slt_op;
      when op_sltu => return sltu_op;
      when op_xor  => return xor_op;
      when op_srl  => return srl_op;   -- do sra
      when op_or   => return or_op;
      when op_and  => return and_op;
      when others  => return opxx_op;
    end case;
    when cat_lui => return lui_op;
    when cat_bxx =>
      case funct3 is
      when bxx_eq  => return beq_op;
      when bxx_ne  => return bne_op;
      when bxx_lt  => return blt_op;
      when bxx_ge  => return bge_op;
      when bxx_ltu => return bltu_op;
      when bxx_geu => return bgeu_op;
      when others  => return bxx_op;
      end case;
    when cat_jalr => return jalr_op;
    when cat_jal =>  return jal_op;
    when cat_sys =>
      case funct3 is
      when sys_normal => return sysnxx_op;  -- Continue here!
        case funct12 is
        when sysn_ecall  => return ecall_op;
        when sysn_ebreak => return ebreak_op;
        when sysn_mret   => return mret_op;
        when sysn_wfi    => return wfi_op;
        when others      => return sysnxx_op;
        end case;
      when sys_csrrw  => return csrrw_op;
      when sys_csrrs  => return csrrs_op;
      when sys_csrrc  => return csrrc_op;
      when sys_csrrwi => return csrrwi_op;
      when sys_csrrsi => return csrrsi_op;
      when sys_csrrci => return csrrci_op;
      when others     => return sysxx_op;
      end case;
    when cat_fence    => return fence_op;
    when others => return xx_op;
    end case;
--  begin
--    case to_integer(opcode) is
--    when cat_load =>
--      case to_integer(funct3) is
--      when mem_b  => opname <= "lb";
--      when mem_h  => opname <= "lh";
--      when mem_w  => opname <= "lw";
--      when mem_bu => opname <= "lbu";
--      when mem_hu => opname <= "lhu";
--      when others => opname <= "unknown load";
--      end case;
--    when cat_opi =>
--      case to_integer(funct3) is
--      when op_add  => opname <= "addi";   -- sub?
--      when op_sll  => opname <= "slli";
--      when op_slt  => opname <= "slti";
--      when op_sltu => opname <= "sltui";
--      when op_xor  => opname <= "xori";
--      when op_srl  => opname <= "srli";   -- sra?
--      when op_or   => opname <= "ori";
--      when op_and  => opname <= "andi";
--      when others  => opname <= "unknown opi";
--      end case;
--    when cat_auipc => opname <= "auipc";
--    when cat_store =>
--      case to_integer(funct3) is
--      when mem_b  => opname <= "sb";
--      when mem_h  => opname <= "sh";
--      when mem_w  => opname <= "sw";
--      when others => opname <= "unknown store";
--      end case;
--    when cat_op =>
--      case to_integer(funct3) is
--      when op_add  => opname <= "add";   -- do sub
--      when op_sll  => opname <= "sll";
--      when op_slt  => opname <= "slt";
--      when op_sltu => opname <= "sltu";
--      when op_xor  => opname <= "xor";
--      when op_srl  => opname <= "srl";   -- do sra
--      when op_or   => opname <= "or";
--      when op_and  => opname <= "and";
--      when others  => opname <= "unknown op";
--    end case;
--    when cat_lui => opname <= "lui";
--    when cat_bxx =>
--      case to_integer(funct3) is
--      when bxx_eq  => opname <= "beq";
--      when bxx_ne  => opname <= "bne";
--      when bxx_lt  => opname <= "blt";
--      when bxx_gt  => opname <= "bgt";
--      when bxx_ge  => opname <= "bge";
--      when bxx_ltu => opname <= "bltu";
--      when bxx_geu => opname <= "bgeu";
--      when others  => opname <= "unknown bxx";
--      end case;
--    when cat_jalr => opname <= "jalr";
--    when cat_jal =>  opname <= "jal";
--    when cat_sys =>
--      case to_integer(funct3) is
--      when sys_normal => opname <= "unknown sys_normal";
--      when sys_csrrw  => opname <= "csrrw";
--      when sys_csrrs  => opname <= "csrrs";
--      when sys_csrrc  => opname <= "csrrc";
--      when sys_csrrwi => opname <= "csrrwi";
--      when sys_csrrsi => opname <= "csrrsi";
--      when sys_csrrci => opname <= "csrrci";
--      when others     => opname <= "unknown sys";
--      end case;
--    when others => opname <= "unknown instruction";
--    end case;
--  end process;
  end function;
end package body cpu_defs;
--      case to_integer(opcode) is
--      when cat_op =>
--        v1sel := REG1;
--        v2sel := REG2;
--      when cat_opi =>
--        v1sel := REG1;
--        v2sel := IMMEDIATE;
--        imm   := suit(imm32i, v2);
--      when cat_load =>
--        v1sel := REG1;
--        v2sel := IMMEDIATE;
--        imm   := suit(imm32i, v2);
--      when cat_store =>
--        v1sel := REG1;
--        v2sel := IMMEDIATE;
--        imm   := suit(imm32s, v2);
--      when cat_auipc =>
--        v1sel := REGPC;
--        v2sel := IMMEDIATE;
--        imm   := suit(imm32u, v2);
--      when cat_jal =>
--        v1sel := REGPC;
--        v2sel := IMMEDIATE;
--        imm   := suit(imm32j, v2);
--      when cat_jalr =>
--        v1sel := REGPC;
--        v2sel := IMMEDIATE;
--        imm   := suit(imm32i, v2);
--      when cat_bxx =>
--        if doing_branch or (funct3 = bxx_eq or funct3 = bxx_ne) then
--          v1sel := REGPC;
--          v2sel := IMMEDIATE;
--          imm   := suit(imm32b, v2);
--        else
--          v1sel := REG1;
--          v2sel := REG2;
--        end if;
--      when cat_sys =>         -- Correct for csrrw/rs/rc at least.
--        v1sel := REG1;
--        v2sel := IMMEDIATE;
--        imm   := suit(imm32i, imm);
--      when others =>             -- Does not matter!
--        v1sel := REG1;
--        v2sel := IMMEDIATE;
--        imm   := suit(imm32i, v2);
--      end case;
--
--
--      v1sel := REG1;
--      v2sel := IMMEDIATE;
--      imm   := suit(imm32i, v2);
--      case to_integer(opcode) is
--      when cat_op    => v2sel := REG2;
--      when cat_store => imm   := suit(imm32s, v2);
--      when cat_auipc => v1sel := REGPC;
--                        imm   := suit(imm32u, v2);
--      when cat_jal   => v1sel := REGPC;
--                        imm   := suit(imm32j, v2);
--      when cat_jalr => v1sel := REGPC;
--      when cat_bxx =>  if doing_branch or (funct3 = bxx_eq or funct3 = bxx_ne) then
--                         v1sel := REGPC;
--                         imm   := suit(imm32b, v2);
--                        else
--                          v2sel := REG2;
--                        end if;
--      when cat_sys =>         -- Correct for csrrw/rs/rc at least.
--      end case;

