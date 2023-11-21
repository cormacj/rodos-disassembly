help:
	$(info make validate  - check to see if the working asm file compiles to a binary match with RODOS219)
	$(info make diffs - If you have a binary difference this uses a brief diff so you can see where the drift happened)
	$(info make fullcompare - If you have a binary difference this puts up a side-by-side diff so you can see where the drift happened)

validate:
		#z80asm -v rodos220.asm && diff RODOS219.ROM a.bin && echo "\nRODOS219 and compile match"
		z80asm -v rodos220.asm && ./validate-size.sh a.bin

fullcompare:
		z80asm rodos220.asm && ~/Amstrad/utils/dum.py a.bin -o 0xc000 >new.txt && diff -y original.txt new.txt |less

diffs:
		z80asm rodos220.asm && ~/Amstrad/utils/dum.py a.bin -o 0xc000 >new.txt && diff original.txt new.txt

run:
		z80asm rodos220.asm && cp a.bin ~/Amstrad/ROM/ && cap32 ~/Amstrad/blankdisc.dsk

release:
		rm -v Release/* && z80asm -v -o Release/rodos220.rom rodos220.asm && ./validate-size.sh Release/rodos220.rom && zip -v -j Release/rodos220.zip CHANGELOG Release/rodos220.rom && echo "Release made!"

clean:
		rm a.bin
