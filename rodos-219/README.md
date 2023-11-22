# rodos-disassembly
This is a work in progress disassembly of RODOS v2.19.
As of now this does compile to a binary exact copy of the official V2.19 rom.

This is not relocatable as yet. Any code that causes a shift (eg adding a NOP after RSX definitions)
causes the ROM to fail upload load.

**Important Files**
rodos219.asm: This will compile to a binary match of the existing v2.19 rom. Comments are still a work in progress. Updated sometimes.

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
* [ ] Fully document the work area (the only scan of the manual faded out partway through that section.)
