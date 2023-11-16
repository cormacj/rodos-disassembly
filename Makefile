help:
	$(info make validate  - check to see if the working asm file compiles to a binary match with RODOS219)
	$(info make diffs - If you have a binary difference this uses a brief diff so you can see where the drift happened)
	$(info make fullcompare - If you have a binary difference this puts up a side-by-side diff so you can see where the drift happened)

validate:
		z80asm -v l.z80 && diff RODOS219.ROM a.bin && echo "\nRODOS219 and compile match"

fullcompare:
		z80asm l.z80 && ~/Amstrad/utils/dum.py a.bin -o 0xc000 >new.txt && diff -y original.txt new.txt |less

diffs:
		z80asm l.z80 && ~/Amstrad/utils/dum.py a.bin -o 0xc000 >new.txt && diff original.txt new.txt

run:
		z80asm l.z80 && cp a.bin ~/Amstrad/ROM/ && cap32 ~/Amstrad/blankdisc.dsk
# edit : main.o kbd.o command.o display.o \
#        insert.o search.o files.o utils.o
#         cc -o edit main.o kbd.o command.o display.o \
#                    insert.o search.o files.o utils.o
#
# clean :
#         rm edit main.o kbd.o command.o display.o \
#            insert.o search.o files.o utils.o
