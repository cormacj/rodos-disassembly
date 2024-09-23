 # rodos-2.22
This version has been tested and all commands are working as documented.

**About**

Full details about the ROM and it's history can be found at [RODOS on the CPCWiki.eu website](https://www.cpcwiki.eu/index.php/RODOS)

The development work is tracked [here on github](https://github.com/cormacj/rodos-disassembly)

I'm considering this a major release, although I'm not jumping the versioning that much. This release fixes all the bugs I've been able to identify and find. It also includes one new quality-of-life `|RODOS.OFF` (documented below).

I'm also including my debug ROM build and testing code with this release.

**Future**

I'm not planning on introducing any more features, but if I find bugs (or someone reports one) I'll fix those. Please use the [issues tracker](https://github.com/cormacj/rodos-disassembly/issues) to report those.

I do plan on continuing to document the code when I get time.

**New**

A new command has been added: `|RODOS.OFF`

`|RODOS.OFF,<option command>`: If a program needs all the memory and RODOS isn't giving it enough, then |RODOS.OFF will turn off the RODOS rom and reboot with the passed command. If no command is passed then it will use RUN"DISC on startup.

Examples:

`|RODOS.OFF` - reboot with RODOS turned off, and RUN"DISC on startup

`|RODOS.OFF,"RUN"+CHR$(34)+"GAME"` - reboot with RODOS turned off, and RUN"GAME on startup

`|RODOS.OFF,""` - reboot with RODOS turned off, and don't run anything on startup.

**Fixes**

Since V2.21:

* A new command has been added: `|RODOS.OFF`

* Fixed an issue where |point or |bget would randomly return 27 rather than the expected data.

* Fixed: |BGET randomly returned a number of 27 instead of the actual data. The CAS IN CHAR patch code was reverted to the v2.01 code at the end. See https://github.com/cormacj/rodos-disassembly/issues/1
* Fixed: Limit the length of the |ROM and |ZAP startup string to 100 characters to avoid a buffer overflow. See: https://github.com/cormacj/rodos-disassembly/issues/7

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
