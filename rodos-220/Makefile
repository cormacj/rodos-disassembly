help:
	$(info make validate  - check to see if the working asm file compiles to a binary match with RODOS219)
	$(info make diffs - If you have a binary difference this uses a brief diff so you can see where the drift happened)
	$(info make fullcompare - If you have a binary difference this puts up a side-by-side diff so you can see where the drift happened)
	$(info make run - Build and run the rom in Caprice32)
	$(info make release - generate a zip file suitable for uploading)

validate:
		z80asm -v rodos220.asm && ./validate-size.sh a.bin

fullcompare:
		z80asm rodos220.asm && ./dum.py a.bin -o 0xc000 >new.txt && ./dum.py RODOS219.ROM -o 0c0000 >original.txt && diff -W150 -y original.txt new.txt |less

diffs:
		z80asm rodos220.asm && ./dum.py a.bin -o 0xc000 >new.txt && ./dum.py RODOS219.ROM -o 0xc000 >original.txt && diff -s original.txt new.txt

run:
		z80asm rodos220.asm && cp a.bin ~/Amstrad/ROM/ && cap32 ~/Amstrad/blankdisc.dsk

release:
		#rm -v Release/* && 
		z80asm -v -o Release/rodos220.rom rodos220.asm && ./validate-size.sh Release/rodos220.rom && zip -v -j Release/rodos220.zip CHANGELOG Release/rodos220.rom && echo "Release made!"

clean:
		rm a.bin new.txt original.txt
