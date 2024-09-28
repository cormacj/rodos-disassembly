# rodos-disassembly

## About This Project

In 1988 I owned a copy of RODOS v2.17 and liked it a lot, but did find that it was a little buggy.

In 2023 I started using RODOS again and found that the released versions had several bugs, for example, `|OPT,10,1` was completely broken.

I started working on disassembling RODOS v2.19, with the goal to be able to the code to a point where I could make changes and have it compile correctly. Most of the code was simple to decipher, however the ROM had a few gotchas due to how to patched into the AMSDOS calls and its use of RST 20 to make calls.

Once this was figured out, I had code that compiled to a binary match of v2.19, and this allowed me to produce fixes for the broken commands.

This repo now has several versions.

**Older releases should be considered locked and changes won't be accepted for these**.

The versions are:
### rodos-219

This is the code complete version of the last known release.

### rodos-220

This release fixed the |zap,rom,"string" corruption bug.

### rodos-221

This release corrected `|OPT,10` and `|OPT,11`

### rodos-222

This release

## About RODOS

RODOS was a high-capacity disc operating system released by Romantic Robot, whose main aim was to allow you to make use of the extra capacity on 3.5in discs. It shipped on a single 16k sideways ROM.

Unlike other similar DOSses (ParaDOS, MS800, S-DOS, ROMDOS), RODOS aimed to do significantly more than provide new, high-capacity formats. Among its features were:

* Long filenames (no restriction to 8.3 format)
* Hierarchical subdirectories
* Autorun facility (automatically loads DISC., DISC.BAS or DISC.BIN if present)

Unlike ParaDOS and ROMDOS, RODOS was not a modified version of the original AMSDOS code: its disc access routines were all new.

However, despite its advanced features, RODOS never gained much of a foothold. This is commonly attributed to the fact that it took up substantially more RAM workspace than AMSDOS, thereby lowering HIMEM and making it incompatible with much software.

## History

Romantic Robot was one of the longest-established CPC software companies, founded by Alexander Goldscheider. Their best-known product was the Multiface II, but the company also produced some serious software and even games. Romantic Robot produced Multifaces for the Spectrum, Amiga and ST, as well as the CPC. The company also sold classical music CDs.

Matthew Edwards, programmer of the Romantic Robot game Wriggler, spent a couple years perfecting Rodos - an impressive feat which he managed single handedly.

The RODOS system is best thought of as two seperate entities within a single 16k ROM. On one side you have RODOS- a powerful disk operating system which runs hand in glove with Amsdos - and on the other RECS (Rom Extended Command System), which gives extra operating system commands. RODOS and RRECS provide the user with extra bar commands Both systems work with or without the other.

The first release of RODOS was in 1987, with v2.01. The last known official release was v2.19.

RODOS had several releases, and the archived versions are available at the [CPC Wiki RODOS Page](https://www.cpcwiki.eu/index.php/RODOS)

* v2.01 - This appears to have been the first commercial release, dated 1987.
* v2.11
* v2.12
* v2.13
* v2.15 - This version appears to have been a milestone release. The manual makes many references to V2.15 and above.
* v2.17
* v2.19

**About This Project**

Since then, I've produced the following releases:

***RODOS 2.20 - November, 2023 - New Patched Version***

-  Fixed a bug where |zap,12,"print"+chr$(34)+"Rom 12 turned off"+chr$(13) would not work as documented. This was being overwritten by relocated code, meaning on reboot it was displaying garbage. I've moved the command buffer for |zap and |rom from 0xbec0 to 0xbf20 to correct this bug. A bonus result due to this move is that  the possible command length has changed from 31 (in V2.13) to 100 now.

***RODOS 2.21 - June 2024 - Bugs fixes***
- Put the |OPT,10 code back in that was lost in v2.17.
- Corrected |OPT,11 so that it uses the correct drive parameter

***RODOS 2.22 - Current development***
- |BGET randomly returned a number of 27 instead of the actual data. This was traced to how the ROM patched the CAS_IN_CHAR call. The CAS IN CHAR patch code was reverted to the v2.01 code. See https://github.com/cormacj/rodos-disassembly/issues/1
- Added code to enforce a limit on the length of the |ROM and |ZAP startup string to 100 characters to avoid a buffer overflow.  See: https://github.com/cormacj/rodos-disassembly/issues/7

V2.22 includes validation scripts to validate that the rom is working as documented.

**Details about this project**

There are several folders here:
* rodos-219 - This is a faithful disassembly and documentation of RODOS v2.19. This was the last known official release of RODOS
* rodos-220 - This is a version that fixes a buffer collision bug that caused a couple of commands (|ZAP, |ROMS) to not fully work.
* rodos-221 - This version fixed two bugs with |OPT (specifically |OPT 10 and |OPT 11)
* rodos-222 - This is the current work-in-progress version. A bugfix for a |BGET bug has been applied.
* disasm - This folder contains scripts and definitions to disassemble a ROM.
* doc - This contains a text version of the manual and some documentation on internals.

**Configuration notes**

This relates only to v2.22 development
Makefile:
```
make validate  - check to see if the working asm file compiles correctly and is a valid size.
make diffs - If you have a binary difference this uses a brief diff so you can see where the drift happened
make rom - generate a rodos222.rom file and validate it
make fullcompare - If you have a binary difference this puts up a side-by-side diff so you can see where the drift happened
make run - Build and run the rom in Caprice32
make runa - Build and run the rom in Arnold
make release - generate a zip file suitable for uploading
make: 'help' is up to date.
```


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
I also use Caprice32 to validate and test the builds.

This is available at: https://github.com/ColinPitrat/caprice32

I have Caprice32 configured to run in ~/Amstrad and to load a.bin as a ROM and a blank Amstrad .dsk file for `make run`

**Licence**

I don't own this code,so I've not attached any license to this.
