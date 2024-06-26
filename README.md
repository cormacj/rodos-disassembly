# rodos-disassembly

**About RODOS**

This project is a disassembly of RODOS v2.19.

RODOS was a high-capacity disc operating system released by Romantic Robot for Amstrad CPC computers. The main aim was to allow you to make use of the extra capacity on 3.5in discs. It shipped on a single 16k sideways ROM.

Unlike other similar DOSses (ParaDOS, MS800, S-DOS, ROMDOS), RODOS aimed to do significantly more than provide new, high-capacity formats. Among its features were:

* Long filenames (no restriction to 8.3 format)
* Hierarchical subdirectories
* 'Autorun' facility (automatically loads DISC., DISC.BAS or DISC.BIN if present)

**Details about this project**

There are several folders here:
* rodos-219 - This is a faithful disassembly and documentation of RODOS v2.19. This was the last known official release of RODOS
* rodos-220 - This is a version that fixes a buffer collision bug that caused a couple of commands (|ZAP, |ROMS) to not fully work.
* rodos-221 - This version fixed two bugs with |OPT (specifically |OPT 10 and |OPT 11)
* rodos-222 - This is the current work-in-progress version. A bugfix for a |BGET bug has been applied.
* disasm - This folder contains scripts and definitions to disassemble a ROM.
* doc - This contains a text version of the manual and some documentation on internals.


**Tools needed**

This project is done on Linux. I've not looked at tools for other environments.

linux: z80asm and z80dasm

z80asm:
```
Z80 assembler version 1.8
Copyright (C) 2002-2007 Bas Wijnen <shevek@fmf.nl>.
Copyright (C) 2005 Jan Wilmans <jw@dds.nl>.
This program comes with ABSOLUTELY NO WARRANTY.
You may distribute copies of the program under the terms
of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The complete text of the GPL can be found in
/usr/share/common-licenses/GPL.
```

z80dasm:
```
z80dasm, Z80 assembly language generating disassembler 1.1.6
Copyright (C) 1994-2007 Jan Panteltje <panteltje@yahoo.com>
Copyright (C) 2007-2019 Tomaz Solc <tomaz.solc@tablix.org>

This is free software. You may redistribute copies of it under the terms of
the GNU General Public License <http://www.gnu.org/licenses/gpl.html>.
There is NO WARRANTY, to the extent permitted by law.
```
** Configuration notes **

Makefile:
```
make validate  - check to see if the working asm file compiles to a binary match with RODOS219
make diffs - If you have a binary difference this uses a brief diff so you can see where the
             drift happened
make fullcompare - If you have a binary difference this puts up a side-by-side diff so you can
                   see where the drift happened
make run - Build and run the rom in Caprice32
make release - generate a zip file suitable for uploading
make: 'help' is up to date.
````

I also use Caprice32 to validate and test the builds.

This is available at: https://github.com/ColinPitrat/caprice32

I have Caprice32 configured to run in ~/Amstrad and to load a.bin as a ROM and a blank Amstrad .dsk file for `make run`
