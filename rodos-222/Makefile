version=222

help:
	$(info make validate  - check to see if the working asm file compiles to a binary match with RODOS219)
	$(info make diffs - If you have a binary difference this uses a brief diff so you can see where the drift happened)
	$(info make rom - generate a rodos$(version).rom file and validate it)
	$(info make fullcompare - If you have a binary difference this puts up a side-by-side diff so you can see where the drift happened)
	$(info make run - Build and run the rom in Caprice32)
	$(info make release - generate a zip file suitable for uploading)

releasefiles=README.md KNOWN_BUGS TESTING.md CHANGELOG rodos$(version).rom
testingfiles=README.md KNOWN_BUGS TESTING.md CHANGELOG rodos$(version)-debug.rom *.dsk
sourcefiles=rodos222.asm debug_on.asm debug_off.asm Makefile validate-size.sh z80make

RELEASE_DIR=Release
TESTING_DIR=$(RELEASE_DIR)/Testing
DEBUG_DIR=$(RELEASE_DIR)/Debug
SOURCE_DIR=$(RELEASE_DIR)/Source

ifndef debug
rodos=debug_off.asm rodos${version}.asm
else
rodos=debug_on.asm rodos${version}.asm
endif

validate:
		./z80make -v ${rodos} && ./validate-size.sh a.bin

rom:
		./z80make -v ${rodos} -o rodos$(version).rom && ./validate-size.sh rodos$(version).rom

fullcompare:
		./z80make ${rodos} && ./dum.py a.bin -o 0xc000 >new.txt && ./dum.py rodos$(version).rom -o 0c0000 >original.txt && diff -W165 -y original.txt new.txt |less

diffs:
		./z80make ${rodos} && ./dum.py a.bin -o 0xc000 >new.txt && ./dum.py rodos$(version).rom -o 0xc000 >original.txt && diff -s original.txt new.txt

run:
		./z80make ${rodos} && cp a.bin ~/Amstrad/ROM/ && cap32 ~/Amstrad/blankdisc.dsk ~/Amstrad/rodosval.dsk

runa:
		./z80make ${rodos} && cp a.bin ~/Amstrad/ROM/ && arnold --diska ~/Amstrad/blankdisc.dsk --diskb ~/Amstrad/rodosval.dsk

release:
		rm -rf $(RELEASE_DIR)
		mkdir -pv $(RELEASE_DIR)
		mkdir -pv $(DEBUG_DIR)
		mkdir -pv $(SOURCE_DIR)
		for u in $(testingfiles); do echo $$u; cp $$u $(RELEASE_DIR); done
		# cp *.md Release/
		# cp Rodos\ validation\ code.dsk Release/testing
		./z80make -v -o Release/rodos$(version).rom debug_off.asm rodos${version}.asm && ./validate-size.sh Release/rodos$(version).rom && zip -v -j Release/rodos$(version).zip ${releasefiles} && echo "Release made!"
		# ./z80make -v -o Release/rodos$(version)-debug.rom debug_on.asm rodos${version}.asm && ./validate-size.sh Release/rodos$(version)-debug.rom && zip -v -j Release/rodos$(version)-debug.zip ${testingfiles} && echo "Debug Release made!"

clean:
		rm -f a.bin rodos*.rom new.txt original.txt tmp*
		rm -rf $(RELEASE_DIR)
