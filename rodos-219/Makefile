help:
	$(info make validate  - check to see if the working asm file compiles to a binary match with RODOS219)
	$(info make diffs - If you have a binary difference this uses a brief diff so you can see where the drift happened)
	$(info make fullcompare - If you have a binary difference this puts up a side-by-side diff so you can see where the drift happened)
	$(info make run - Build and run the rom in Caprice32)
	$(info make release - generate a zip file suitable for uploading)

validate:
		z80asm -v rodos219.asm && ./validate-size.sh a.bin

fullcompare:
		z80asm rodos219.asm && ./dum.py a.bin -o 0xc000 >new.txt && ./dum.py RODOS219.ROM -o 0c000 >original.txt && diff -W165 -y original.txt new.txt |less

diffs:
		z80asm rodos219.asm && ./dum.py a.bin -o 0xc000 >new.txt && ./dum.py RODOS219.ROM -o 0xc000 >original.txt && diff -s original.txt new.txt

run:
		z80asm rodos219.asm && cp a.bin ~/Amstrad/ROM/ && cap32 ~/Amstrad/blankdisc.dsk

release:
		rm -v Release/* && z80asm -v -o Release/rodos219.rom rodos219.asm && ./validate-size.sh Release/rodos219.rom && zip -v -j Release/rodos219.zip CHANGELOG Release/rodos.rom && echo "Release made!"

clean:
		rm a.bin new.txt original.txt
