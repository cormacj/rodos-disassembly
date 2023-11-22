#z80dasm -g 0xc000 -S fwsyms.txt -s syms.txt -r default -l -t RODOS219.ROM
z80dasm -b blockfile.txt -g 0xc000 -S Firmware_labels.txt -s syms.txt -r default -l -t RODOS219.ROM -v >l.z80


