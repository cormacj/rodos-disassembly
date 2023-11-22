# rodos-disassembly

This project is a disassembly of RODOS v2.19.

RODOS was a high-capacity disc operating system released by Romantic Robot for Amstrad CPC computers. The main aim was to allow you to make use of the extra capacity on 3.5in discs. It shipped on a single 16k sideways ROM.

Unlike other similar DOSses (ParaDOS, MS800, S-DOS, ROMDOS), RODOS aimed to do significantly more than provide new, high-capacity formats. Among its features were:

* Long filenames (no restriction to 8.3 format)
* Hierarchical subdirectories
* 'Autorun' facility (automatically loads DISC., DISC.BAS or DISC.BIN if present)

The rodos-219 folder is a faithful disassembly and documentation of RODOS v2.19.

The rodos-220 folder is a version that fixes a buffer collision bug that caused a couple of commands to not fully work.

The disasm folder contains scripts and definitions to disassemble a ROM.
