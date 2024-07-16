 # rodos-2.22
This version has been tested and all commands are working as documented.

Development work is tracked here: https://github.com/cormacj/rodos-disassembly

**New**

A new command has been added: `|RODOS.OFF`

`|RODOS.OFF,<option command>`: If a program needs all the memory and RODOS isn't giving it enough, then |RODOS.OFF will turn off the RODOS rom and reboot with the passed command. If no command is passed then it will use RUN"DISC on startup.

Examples:

`|RODOS.OFF` - reboot with RODOS turned off, and RUN"DISC on startup

`|RODOS.OFF,"RUN"+CHR$(34)+"GAME"` - reboot with RODOS turned off, and RUN"GAME on startup

`|RODOS.OFF,""` - reboot with RODOS turned off, and don't run anything on startup.

**Fixes**

Since V2.21:

    Fixed an issue where |point or |bget would randomly return 27 rather than the expected data.

Since V2.20:

    Reimplimented |OPT,10
    Corrected |OPT,11

**Important Files**

rodos222.asm: This is based on the relocatable v2.19 code, with the incremental changes since then.

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

There is a debug option available on building: for example `make debug=1 run`

**TODO**
* [ ] More documentation
* [ ] Determining what more the various calls actually do.
* [X] Fully document the work area (the only scan of the manual faded out partway through that section.)
