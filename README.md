# rodos-disassembly
This is a work in progress disassembly of RODOS v2.19.
As of now this does compile to a binary exact copy of the official V2.19 rom.

This is not relocatable as yet. Any code that causes a shift (eg adding a NOP after RSX definitions)
causes the ROM to fail upload load.

**Important Files**
rodos219.asm: This will compile to a binary match of the existing v2.19 rom. Comments are still a work in progress. Updated sometimes.
rodos220.asm: This is v2.19 with the zap buffer bug fixed.
l.z80: This is the work-in-progress code thats being documented.

**Tools needed**

linux: z80asm and z80dasm

**Supplemental tools**
`tools/generate_string_locations.sh`
This tool will search a ROM for strings and produce a blockfile for use with `z80dasm -b`.

The goal is to help tag these locations and stop z80dasm from confusing these as code.

Note that once it's finished, you'll have to edit the results and clean out anything thats not obviously a string.

The output file format is:
`lc0c3h: start 0xc0c3 end 0xc0cb type bytedata ;db "RODOS RO"`


Usage:
./generate_string_locations.sh RODOS219.ROM  >blockfile.txt

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
RODOS 2.13 works as documented, up to 32 chars. Will have to look at that.
Rodos 2.15 has the bug with things being overwritten at start.

Test command:
|zap,12,"12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
Notes:
At 30 characters, we get a bunch of ASCII 255s in the buffer
At 60 characters, we hit the |CLI strings
At 70 characters, it zaps all roms

Solutions?
1. I need to add a check for length/max length
2. Find a better buffer space - most blank areas seem to non-useful. So #1 is best option.
