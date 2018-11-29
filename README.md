# The "insert name here later" RISC-V32i 
This is an as yet unnamed implementation of RISC-V32i in VHDL, under the GPLv3.
It satisfies the standard compliance tests, tested using a build for riscvOVPsim.

## A breif history
I started working on this CPU on November 8, since a colleague mentioned that there was a competition going on. It sounded like fun, but I quickly realized that I would have to do this outside the actual competition since VHDL was not allowed...

Now it is November 26, and I really need some time off from this. Far too many evenings have been spent in front of the computer over these past three weeks (and I actually took today off from work to get the final compliance tests and interrupts working).

The current check-in is only the final result of this work. I will go back and put up the full day by day history of the code later. This is all really in need of a clean-up, but the deadline is in a couple of hours, and it is now 4am here...

## Design

The CPU is not yet pipelined, but I have tried to keep things structured in a way that should make it reasonably straight-forward to do later on.

An FSM runs that is more or less FETCH/DECODE/EXECUTE/WRITEBACK, with various extras states to handle more complex things. However, FETCH is normally overlapped with EXECUTE, and DECODE with WRITEBACK. 

* ADD/SUB, SLT, AND/OR/XOR, AUIPC, LUI - 2 cycles
* SLL/SRL/SRA - currently 2 + (n / 8) + (n % 8) cycles
* JAL/JALR - 2 cycles, but next instruction will take an extra to fetch
* BEQ/BNE - 2 cycles when not taken, else an extra to fetch next
* BLT/BGE - 2 cycles when not taken, else 3 and an extra to fetch next
* LOAD - 4 (assuming single cycle RAM) and an extra to fetch next
* STORE - 3 (assuming single cycle RAM) and an extra to fetch next
* CSR - 4

(I might have gotten a couple of things wrong above, but close to that. Should test.)

The code uses a single process (~600 "active" lines) for just about everything (the rest is ~300 "active" lines, including entity and signal declarations, but discounting constant declarations and utility functions). I have separated out RAM, interrupt synchronization, register file write (mainly to ensure the tools do not get confused regarding EBR/BRAM implementation), and CSR handling.

My code style may not be the most common, with heavy use of variables (neat, fast simulation, but not so good for debug on simple simulators) and relying on the tools. The synthesizers are really good these days, so one can keep the code quite readable and still get extremely good performance (see further down regarding the Xilinx build).

## Extra features

* Handling of unaligned load and store can be activated via a generic.  
  (I believed this was required, but then failed the compliance test...)  
  Tested, but not enough.

## Limitations

* So far, only "fake" single cycle access memory has been tested.
* There is not yet any support for the memory mapped timer.  
  (I completely forgot about it, and then time was up.)
* External interrupt handling is usable, but does not quite do what it should.  
  (I need to take another look at the spec).
* While counters for cycles and retired instructions are in there, they both still do the former.
* There is only machine mode  
  And the privileged stuff is not complete, although ECALL, MRET, CSR:s etc are implemented to what seems to be a useful degree.

## Builds, resources and performance

There are somewhat different implementations for Xilinx (tested with an UltraScale+ build using Vivado) and Lattice (tested with an UltraPlus build using Radiant). The main difference is that the Lattice register file uses clocked output, while the Xilinx one does not. This means that it is possible to move more logic to the decode stage on the Xilinx chip.

A normal build on Lattice currently uses ~3000 LUT:s, 750 registers and 13 EBR:s (that is with a 1024 deep 4 byte wide "RAM"). I have not yet gotten around to using an SRAM instead, I'm afraid. While I have not checked in detail, it seems like somewhere around 20 MHz ought to be possible with this configuration.

A normal UltraScale+ build uses ~250 CLB:s (~1500 LUT, ~700 reg, 1 BRAM).
The maximum number of logic levels varies depending on build options, but I have not seen above 10 (and that includes a bunch of carry chains), and most by far is 6 and below. The resulting maximum logic delay also depends, but is usually about 1.05 ns (the highest ones are from BRAM output).
Net delay is usually much worse, with the maximum on my latest build at just short of 1.8 ns.
Fortunately, things even out and it is usually no problem to build with a 450 MHz clock, unless using single cycle BRAM (which limits things to somewhere closer to 300 MHz). This remains true when using a BRAM in high performance mode as memory (but the current design does not actually work then ;-). Implementing a decent memory system is among the next things to do.

## Build/simulation instructions

Note that this requires VHDL 2008 to build, except for dpram.vhd where ModelSim actually complains but runs anyway.

Once I find the time, I will include more information and actual tool setup code for these things.

### Lattice UltraPlus
The top level is cpu_top.vhd.
Due to the limited number of pins available, this only has reset, clk and irq as input.
As output, all you get is the low 8 bits of the CSR mscratch register (for no particular reason).

You need to enable VHDL 2008, but then both synthesizers available in Radiant should work (I have recently been using Lattice LSE rather than Synplify).

This will give an implementation with a built in 4 kbyte (EBR) RAM. Without any contents, as is...

### Xilinx
Use cpu.vhd as top level.
Set all files except dpram.vhd as VHDL 2008, and ensure everything ends up in a library named work.
Besides the obvious reset, clk and irq (and silly ext_out as above), this has two different interfaces depending on configuration.
1. I have mostly been running with the "imaginary" single cycle RAM interface (ext_addr, ext_r, ext_w, ext_be), which is expected to deliver data in a single cycle when ext_be is all zero, otherwise write in a single cycle when some byte enables are set. There is some handling of ext_valid, but it has not actually been tested (and apparently ended up in the wrong place in the port).
2. Use an internal dual ported RAM. Then there is an interface to program it (ext_clk, ext_rst, ext_waddr, ext_din, ext_we, ext_en) from outside. It has been a while since I last tested this, unfortunately.

### Simulation
Use cpu_tb.vhd as top level.
This can configure the CPU to run in Lattice or Xilinx mode (see above), as well as to use different RAM interfaces and test benches.

There are a couple of built in tests (see rom_value_1-5), for testing various things.
1. My first test, with a mix of instructions. Later co-opted to also test loads.
2. A "more realistic" test, calculating the number of iterations required for a number to reach 1 when checking the Collatz conjecture. Also added in a first ECALL test, for good measure.
3. Misligned store test.
4. Misaligned load test.
5. External interrupt test. A counter loop that is interrupted.

By setting test=10, the full set of compliance test modules is read into the CPU one at a time and run to completion, dumping the verification data to the standard output. The code is supplied as binary files under the compliance/rv32i directory, and includes dumps of code and data segments from a riscvOVPsim build (that includes all the miscellaneous CPU check stuff that is done on boot, and it works well enough).

Soon I will add instructions for generating these binary dumps, as well as a comparison file for the test signatures.
