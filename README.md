# rodos-disassembly
This is a work in progress disassembly of RODOS v2.19.
As of now this does compile to a binary exact copy of the official V2.19 rom.

**Tools needed**

linux: z80asm and z80dasm

**COMPILING**

Do this:
`z80asm RODOS-219-wip.asm -o RODOS-219-wip.rom`

**TODO**
* [ ] More documentation
* [ ] Determining what more the various calls actually do.
* [ ] Determine where the `|zap,ROMNUMBER,"Message/Command"` buffer overflow is happening.
* [ ] Fully document the work area (the only scan of the manual faded out partway through that section.)
