This is an as yet unnamed implementation of RISC-V32i in VHDL, under the GPLv3.
It satisfies the standard compliance tests, tested using a build for riscvOVPsim.

I started working on this CPU on November 8, since a colleague mentioned that there was a competition going on. It sounded like fun, but I quickly realized that I would have to do this outside the actual competition since VHDL was not allowed...

Now it is November 26, and I really need some time off from this. Far too many evenings have been spent in front of the computer over these past three weeks (and I actually took today off from work to get the final compliance tests and interrupts working).

The current check-in is only the final result of this work. I will go back and put up the full day by day history of the code later. This is all really in need of a clean-up, but the deadline is in a couple of hours, and it is now 4am here...

Design

The design is not yet pipelined, but I have tried to keep things structured in a way that should make it reasonably straight-forward to do later on.

An FSM runs that is more or less FETCH/DECODE/EXECUTE/WRITEBACK, with various extras states to handle more complex things. However, FETCH is normally overlapped with EXECUTE, and DECODE with WRITEBACK. This means that simple instructions, including some branches, run in two cycles.

The code uses a single process (~1k lines) for just about everything. I have separated out RAM, interrupt synchronization, register file write (mainly to ensure the tools do not get confused regarding EBR/BRAM implementation), and CSR handling.
My code style may not be the most common, with heavy use of variables (fast simulation, but not so good for debug on simple simulators) and relying on the tools. The synthesizers are really good these days, so one can keep the code quite readable and still get extremely good performance (see further down regarding the Xilinx build).

Extra features

Handling of unaligned load and store can be activated via a generic. Tested, but not enough.

Limitations

There is only master mode, but ECALL, MRET, CSR:s etc are implemented to what seems to be a useful degree. External interrupt is available, but the handling is not quite they way it is supposed to be yet.

Cycle count is in there, but retired instruction count is not correct.
There is not yet any support for the memory mapped timer.
So far, only "fake" single cycle access memory has been tested.

Builds, resources and performance

There are somewhat different implementations for Xilinx (tested with an UltraScale+ build using Vivado) and Lattice (tested with an UltraPlus build using Radiant).
The main difference is that the Lattice register file uses clocked output, while the Xilinx one does not. This means that it is possible to move more logic to the decode stage on the Xilinx chip.

A normal build on Lattice currently uses ~3000 LUT:s, 750 registers and 13 EBR:s (that is with a 1024 deep 4 byte wide "RAM"). I have not yet gotten around to using an SRAM instead, I'm afraid. While I have not check in detail, it seems like somewhere around 20 MHz ought to be possible.

Unfortunately, I have not had the time to build on Xilinx for a few days, and I do not remember what the resource usage was. It did build at somewhere between 450 and 500 MHz (only a couple of places with as much as 8-9 gate levels reported, and those included carry chains), however, using the code I had then. That was with an imaginary single cycle external RAM, but even using single cycle BRAM I got around 300 MHz. Implementing a decent memory system is among the next things to do.
