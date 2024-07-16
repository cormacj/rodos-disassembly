version=222

help:
	$(info make validate  - check to see if the working asm file compiles to a binary match with RODOS219)
	$(info make diffs - If you have a binary difference this uses a brief diff so you can see where the drift happened)
	$(info make rom - generate a rodos$(version).rom file and validate it)
	$(info make fullcompare - If you have a binary difference this puts up a side-by-side diff so you can see where the drift happened)
	$(info make run - Build and run the rom in Caprice32)
	$(info make release - generate a zip file suitable for uploading)

releasefiles=README.md KNOWN_BUGS CHANGELOG Release/rodos$(version).rom Release/rodos$(version)-debug.rom

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
		#rm -v Release/* &&
		./z80make -v -o Release/rodos$(version).rom debug_off.asm rodos${version}.asm && ./validate-size.sh Release/rodos$(version).rom && zip -v -j Release/rodos$(version).zip ${releasefiles} && echo "Release made!"
		./z80make -v -o Release/rodos$(version)-debug.rom debug_on.asm rodos${version}.asm && ./validate-size.sh Release/rodos$(version)-debug.rom && zip -v -j Release/rodos$(version).zip ${releasefiles} && echo "Debug Release made!"

clean:
		rm -f a.bin new.txt original.txt tmp*
