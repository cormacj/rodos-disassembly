# rodos-2.20
This is a work in progress of updates based on the disassembly of RODOS v2.19.

**Important Files**

rodos220.asm: This is v2.19 with the zap buffer bug fixed.

**Tools needed**

linux: z80asm and z80dasm

**Supplemental tools**

`tools/generate_string_locations.sh`
This uses the linux string utility to search for potentially valid strings and generates output that can be included in the z80asm blockfile. Note: This output does have a lot of false positives and will need to be manually cleaned up.

The output file format is:
`lc0c3h: start 0xc0c3 end 0xc0cb type bytedata ;db "RODOS RO"`

Usage:
./generate_string_locations.sh RODOS219.ROM  >blockfile.txt


 `validate-size.sh`
 This validates the size of a file and makes sure that the 16,384 byte size for ROMs is met.

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

`|zap,12,"12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"`

Notes:

At 30 characters, we get a bunch of ASCII 255s in the buffer

At 60 characters, we hit the |CLI strings

At 70 characters, it zaps all roms

Solutions?
1. I need to add a check for length/max length
2. Find a better buffer space - most blank areas seem to non-useful. So #1 is best option.
