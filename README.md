# Current Release: rodos-2.22

This version has been fully tested and all commands are working as documented.

## About

Full details about the ROM and it's history can be found at [RODOS on the CPCWiki.eu website](https://www.cpcwiki.eu/index.php/RODOS)

The development work is tracked [here on github](https://github.com/cormacj/rodos-disassembly)

I'm considering this a major release, although I'm not jumping the versioning that much. This release fixes all the bugs I've been able to identify and find. It also includes one new quality-of-life command `|RODOS.OFF` (documented below).

I have also included my debug ROM build and testing code with this release.

## Future

I do plan on continuing to document the code when I get time.

I'm not planning on introducing any more features, but if I find bugs (or someone reports one) I'll fix those.

## Bug Reporting

Please use the [issues tracker](https://github.com/cormacj/rodos-disassembly/issues) to report those.

## New Commands

A new command has been added: `|RODOS.OFF`

`|RODOS.OFF,<option command>`: If a program needs all the memory and RODOS isn't giving it enough, then |RODOS.OFF will turn off the RODOS rom and reboot with the passed command. If no command is passed then it will use RUN"DISC on startup.

Examples:

`|RODOS.OFF` - reboot with RODOS turned off, and RUN"DISC on startup

`|RODOS.OFF,"RUN"+CHR$(34)+"GAME"` - reboot with RODOS turned off, and RUN"GAME on startup

`|RODOS.OFF,""` - reboot with RODOS turned off, and don't run anything on startup.

### Debug Only Commands

The following commands are only available when using the debug build of the rom:

`|WS`: Report the address of the ROM workspace.

Example: `addr%=0:|WS,@addr%:PRINT HEX$(addr%)`

`|MSG`: This will display all the error messages defined within the ROM. This is used to validate the tokens used to save space within the ROM

`|CLEAR.ERROR`: This command resets the RODOS error address located at &be6d

Example: `|CLEAR.ERROR:|LS,"abc":PRINT "Error code:";PEEK(&BE6D)`

This will return:

`Too many parameters`

`Error Code: 1`

## Fixes and changes

Please see the CHANGELOG file for full details of changes.

V2.22:
* A new command has been added: `|RODOS.OFF`
* Fixed: |POINT or |BGET randomly returned a number of 27 instead of the actual data. The CAS IN CHAR patch code was reverted to the v2.01 code. See https://github.com/cormacj/rodos-disassembly/issues/1
* Fixed: Limit the length of the |ROM and |ZAP startup string to 100 characters to avoid a buffer overflow. See: https://github.com/cormacj/rodos-disassembly/issues/7
* |TITLE produces a meaningful error message if the title string is too long.

## Important Files

rodos222.asm: This is based on the relocatable v2.19 code, with the incremental changes included since then.

### Tools needed

linux: z80asm and z80dasm

### Supplemental tools
`dum.py` - This produces a hex/ascii dump of the rom.

`validate-size.sh` - This file checks the size of the file passed as a parameter and makes sure it's exactly 16k. It throws an error if the file exceeds that size.

`z80make` - This script builds the RODOS rom and reports the available byte space left after compilation.

## COMPILING

#### Building Commands
- `make rom` - Makes a rom file and validates its size
- `make run` - Build and run the rom in Caprice32
- `make runa` - Build and run the rom in arnold
- `make clean` - remove temporary files, built ROMs and releases.
- `make release` - generate zips file suitable for uploading

#### Debugging Commands
- `make validate` - check to see if the working asm file compiles to a binary match with a known version of the rom
- `make diffs` - If you have a binary difference this uses a brief diff so you can see where the drift happened
- `make fullcompare` - If you have a binary difference this puts up a full side-by-side diff so you can see where the drift happened

### Enabling debug mode
Add `debug=1` to the make command to build the ROM with debug option included.

Example: `make run debug=1`

### Updating `make diffs`

`make diffs` compares a build of the latest code against a rom called `Known-Good-RODOS222.ROM`. Copy a new build to that filename and `make diffs` and `make fullcompare` will use that new version as its baseline.
