MAKEFLAGS += --silent

version=222

releasefiles=README.md KNOWN_BUGS CHANGELOG rodos$(version).rom
debugfiles=README.md KNOWN_BUGS TESTING.md CHANGELOG rodos$(version)-debug.rom *.dsk
sourcefiles=CHANGELOG debug_off.asm debug_on.asm dum.py KNOWN_BUGS Makefile opt-validate.bas README.md rodos222.asm TESTING.md THANKS validate.bas validate-size.sh z80make

#This is the known good baseline rom for checking against changes.
KNOWN_GOOD=Known-Good-RODOS222.ROM

WORKING_DIR=Release
DEBUG_DIR=$(WORKING_DIR)/Debug
SOURCE_DIR=$(WORKING_DIR)/Source
RELEASE_DIR=$(WORKING_DIR)/Release

BUILDFILE=rodos-$(version).rom
ifndef debug
rodos=debug_off.asm rodos${version}.asm
else
rodos=debug_on.asm rodos${version}.asm
BUILDFILE=rodos-$(version)-debug.ROM
endif

help:
	$(info make validate  - check to see if the working asm file compiles to a binary match with RODOS219)
	$(info make diffs - If you have a binary difference this uses a brief diff so you can see where the drift happened)
	$(info make rom - generate a rodos$(version).rom file and validate it)
	$(info make fullcompare - If you have a binary difference this puts up a side-by-side diff so you can see where the drift happened)
	$(info make run - Build and run the rom in Caprice32)
	$(info make runa - Build and run the rom in arnold)
	$(info make release - generate a zip file suitable for uploading)

validate:
		./z80make -v -o $(BUILDFILE) ${rodos} && ./validate-size.sh $(BUILDFILE)

rom:
		./z80make -v -o $(BUILDFILE) ${rodos}  && ./validate-size.sh $(BUILDFILE)

fullcompare:
		./z80make ${rodos} && ./dum.py $(BUILDFILE) -o 0xc000 >new.txt && ./dum.py $(KNOWN_GOOD) -o 0c0000 >original.txt && diff -W165 -y original.txt new.txt |less

diffs:
		./z80make -o $(BUILDFILE) ${rodos} && ./dum.py $(BUILDFILE) -o 0xc000 >new.txt && ./dum.py $(KNOWN_GOOD) -o 0xc000 >original.txt && diff -s original.txt new.txt

run:
		./z80make -o $(BUILDFILE) ${rodos} && cp $(BUILDFILE) ~/Amstrad/ROM/ && cap32 ~/Amstrad/blankdisc.dsk ~/Amstrad/rodosval.dsk

runa:
		./z80make -o $(BUILDFILE) ${rodos} && cp $(BUILDFILE) ~/Amstrad/ROM/ && arnold --diska ~/Amstrad/blankdisc.dsk --diskb ~/Amstrad/rodosval.dsk

release:
		mkdir -pv $(RELEASE_DIR)
		mkdir -pv $(DEBUG_DIR)
		mkdir -pv $(SOURCE_DIR)
		# cp *.md Release/
		# cp Rodos\ validation\ code.dsk Release/testing
		echo "----Compiling----"
		./z80make -v -o rodos$(version).rom debug_off.asm rodos${version}.asm && ./validate-size.sh rodos$(version).rom
		./z80make -v -o rodos$(version)-debug.rom debug_on.asm rodos${version}.asm && ./validate-size.sh rodos$(version)-debug.rom
		# zip -v -j Release/rodos$(version).zip ${releasefiles} && echo "Release made!"
		#  && zip -v -j Release/rodos$(version)-debug.zip ${testingfiles} && echo "Debug Release made!"
		for u in $(debugfiles); do cp "$$u" $(DEBUG_DIR); done
		for u in $(releasefiles); do cp "$$u" $(RELEASE_DIR); done
		for u in $(sourcefiles); do  cp "$$u" $(SOURCE_DIR); done

clean:
		rm -f $(BUILDFILE) rodos*.rom new.txt original.txt tmp*
		rm -rf $(DEBUG_DIR)
		rm -rf $(SOURCE_DIR)
		rm -rf $(RELEASE_DIR)
