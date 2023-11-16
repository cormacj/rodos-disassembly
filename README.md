# rodos-disassembly
This is a work in progress disassembly of RODOS v2.19.
As of now this does compile to a binary exact copy of the official V2.19 rom.

This is not relocatable as yet. Any code that causes a shift (eg adding a NOP after RSX definitions)
causes the ROM to fail upload load.

**Tools needed**

linux: z80asm and z80dasm

**COMPILING**

Do this:
`z80asm RODOS-219-wip.asm -o RODOS-219-wip.rom`

**TODO**
* [ ] More documentation
* [ ] Determining what more the various calls actually do.
* [X] Determine where the `|zap,ROMNUMBER,"Message/Command"` buffer overflow is happening.
        This needed the buffer adjusting at two locations:
        F49F: ld de,0becah		;f49f	11 c0 be 	. . .
        F52E:	ld hl,0becah		;f52e	21 c0 be 	! . .
        Both these originally were using 0bec0h

* [ ] Fully document the work area (the only scan of the manual faded out partway through that section.)
