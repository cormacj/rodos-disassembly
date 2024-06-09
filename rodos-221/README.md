# rodos-2.21
This is a work in progress of bugfixes from RODOS v2.19.

**Important Files**

rodos221.asm: This is based on the relocatable v2.19 code.

**Tools needed**

linux: z80asm and z80dasm

**Supplemental tools**

`tools/generate_string_locations.sh`
This uses the linux string utility to search for potentially valid strings and generates output that can be included in the z80asm blockfile. Note: This output does have a lot of false positives and will need to be manually cleaned up.

The output file format is:
`lc0c3h: start 0xc0c3 end 0xc0cb type bytedata ;db "RODOS RO"`

Usage:
./generate_string_locations.sh RODOS219.ROM  >blockfile.txt

**COMPILING**
- make validate  - check to see if the working asm file compiles to a binary match with a known version of the rom
- make diffs - If you have a binary difference this uses a brief diff so you can see where the drift happened
- make rom - Makes a rom file and validates its size
- make fullcompare - If you have a binary difference this puts up a side-by-side diff so you can see where the drift happened
- make run - Build and run the rom in Caprice32
- make release - generate a zip file suitable for uploading

**TODO**
* [ ] More documentation
* [ ] Determining what more the various calls actually do.
* [ ] Fully document the work area (the only scan of the manual faded out partway through that section.)

Test command:

`|zap,12,"12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"`

**Root cause of corruption bug**
Most of the issue was caused by this code:
```
ld bc,0000ah		;d95e	01 0a 00 	. . .
ld hl,ld954h		;d961	21 54 d9 	! T .
ld de,0bec0h		;d964	11 c0 be 	. . .
ldir		;d967	ed b0 	. .
ret			;d969	c9
```
This copied the subroutine at ld954h to 0xbec0, the same 0xbec0 which was being used to store the command for use on reboot. This particular section of code is called a lot, so the chances of buffer corruption were pretty much 100%

This is the code that was copied to the buffer.
```
ld954h:
	push bc			;d954	c5 	.
	call KL_ROM_SELECT		;d955	cd 0f b9 	. . .
	ld a,(hl)			;d958	7e 	~
	call KL_ROM_DESELECT		;d959	cd 18 b9 	. . .
	pop bc			;d95c	c1 	.
	ret			;d95d	c9 	.
```
The last version where this code worked correctly was v2.13 when the code was relocated to 0be80h, avoiding the 0bec0h clash.

In v2.15 this was changed where the rom select code was now sharing the same buffer as the the |zap command data. I can only assume that this was either a bug, or adjusted to avoid a clash at 0xbe80.

My investigation of this issue showed that there was unused space at 0xbf20, so I changed the |zap/|rom command code to use that area rather than going for the more risky route of changing the rom select code that gets called a lot more.
