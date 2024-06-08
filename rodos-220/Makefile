version=220

help:
	$(info make validate  - check to see if the working asm file compiles to a binary match with RODOS219)
	$(info make diffs - If you have a binary difference this uses a brief diff so you can see where the drift happened)
	$(info make rom - generate a rodos$(version).rom file and validate it)
	$(info make fullcompare - If you have a binary difference this puts up a side-by-side diff so you can see where the drift happened)
	$(info make run - Build and run the rom in Caprice32)
	$(info make release - generate a zip file suitable for uploading)

validate:
		z80asm -v rodos$(version).asm && ./validate-size.sh a.bin

rom:
		z80asm -v rodos$(version).asm -o rodos$(version).rom && ./validate-size.sh rodos$(version).rom

fullcompare:
		z80asm rodos$(version).asm && ./dum.py a.bin -o 0xc000 >new.txt && ./dum.py rodos$(version).rom -o 0c0000 >original.txt && diff -W165 -y original.txt new.txt |less

diffs:
		z80asm rodos$(version).asm && ./dum.py a.bin -o 0xc000 >new.txt && ./dum.py rodos$(version).rom -o 0xc000 >original.txt && diff -s original.txt new.txt

run:
		z80asm rodos$(version).asm && cp a.bin ~/Amstrad/ROM/ && cap32 ~/Amstrad/blankdisc.dsk

release:
		#rm -v Release/* &&
		z80asm -v -o Release/rodos$(version).rom rodos$(version).asm && ./validate-size.sh Release/rodos$(version).rom && zip -v -j Release/rodos$(version).zip CHANGELOG Release/rodos$(version).rom && echo "Release made!"

clean:
		rm a.bin new.txt original.txt
