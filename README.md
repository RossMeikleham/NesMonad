NesMonad [![Build Status](https://travis-ci.org/RossMeikleham/NesMonad.svg?branch=master)](https://travis-ci.org/RossMeikleham/NesMonad)
============

Nintendo Entertainment System emulator in Haskell

Completed
========
- Implementation of almost all instructions in the 6502 CPU
- Debugger to trace instructions
- Correct cycle counting of instructions in the CPU

TODO
====
- Implementation of handling interrupts, and Kil instruction
- Correct loading of ROMS and startup initialization
- Implementation of PPU 
- Implementation of APU
- Implementation of Memory redirection to IO ports
- ROM mapper
- Keyboard input and graphics output
