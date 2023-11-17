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
* [X] New Bug: zap/rom buffer is overwritten after 21 characters by 9 characters of Ascii(255)
        I set a limit of 21 characters for the |zap/|Roms message parameter
         
* [ ] Fully document the work area (the only scan of the manual faded out partway through that section.)

**DEBUG NOTES**
Test command:
|zap,12,"12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
Notes:
At 30 characters, we get a bunch of ASCII 255s in the buffer
At 60 characters, we hit the |CLI strings
At 70 characters, it zaps all roms

Solutions?
1. I need to add a check for length/max length
2. Find a better buffer space - most blank areas seem to non-useful. So #1 is best option.
