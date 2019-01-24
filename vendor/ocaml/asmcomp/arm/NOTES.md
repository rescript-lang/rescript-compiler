# Supported platforms

A great many variants of the ARM 32-bit architecture:
* Architecture versions: v4, v5, v5te, v6, v6t2, v7.
  ARMv7 is the standard nowadays.
* Instruction encoding: classic ARM or Thumb or Thumb-2.
* Floating-point: software emulation, VFPv2, VFPv3-d16, VFP-v3.
* ABI: the standard EABI (with floats passed in integer registers)
  or the EABI-HF variant (with floats passed in VFP registers).

Debian architecture names: `armel` and `armhf`.

# Reference documents

* Instruction set architecture:
  _ARM Architecture Reference Manual, ARMv7-A and ARMv7-R edition_.
  Alternatively:
  _ARM Architecture Reference Manual, ARMv8_, restricted to the AArch32 subset.
* Application binary interface:
  _Procedure Call Standard for the ARM Architecture_
