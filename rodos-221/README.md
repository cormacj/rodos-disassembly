# rodos-2.21
This is a work in progress of bugfixes from RODOS v2.19.
**Fixes**
Since V2.20:
    Corrected |OPT,10 and |OPT,11

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

There is a debug option available on building: for example `make debug=1 run`

**TODO**
* [ ] More documentation
* [ ] Determining what more the various calls actually do.
* [X] Fully document the work area (the only scan of the manual faded out partway through that section.)
