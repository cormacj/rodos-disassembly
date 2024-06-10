AUTO
WSRODOSROMNUMBER=         0 :REM The rom slot number of the RODOS rom
WSCPMROMNUMBER=           1 :REM The rom slot number of the CPM (aka AMSDOS) rom (normally slot 7) plus &30.
                                    :REM If the rom is missing or non standard this value is &3F
WSCURRENTDRIVELETTER=     3:REM Current Drive Letter
WSDRIVENUMBER=             4:REM Current Drive Number (mostly used when patching (See Appendix F))
WSLOADINGMESSAGES=         8:REM Loading messages (|OPT,1,x)
WSDISKERRORRETRYCOUNT=   9:REM Disk read error retry count (|OPT,5,x)
WSEXPANSIONRAMCOUNT=      11:REM Expansion ram count (in 16k blocks)
WSSTARTPRBUFFBANK=        12:REM Printer Buffer Bank
WSSCREENOUTPUTVALUE=      17:REM Sets normal screen output according to n, where n is the sum of the following:
                                    :REM     +1 - screen output to current open file.
                                    :REM     +2 - screen output to printer.
                                    :REM     +4 - screen echo disabled.
WSPREVIOUSDRIVELETTER=    19:REM Previous value of WSCURRENTDRIVELETTER
WSOVERWRITEFILE=           22:REM Overwrite file= 0=ask 1=overwrite 2=create backup
WSINPUTBUFFADDRFILE=     23:REM Address of input buffer (file header)
WSINPUTBUFFADDRSECTOR=   25:REM Address of input buffer (sector block)
WSOUTPUTBUFFADDRHEADER=  27:REM Address of output buffer (header)
WSOUTPUTBUFFADDRSECTOR=  29:REM Address of output buffer (sector block)
WSORIGINALKLFINDCOMMAND= 51:REM Original contents of KLFINDCOMMAND location. (KLFINDCOMMAND was patched by RODOS)
WSUSERNUMBER=              54:REM Current User Number
WSCASESENSITIVITY=         65:REM Case sensitivity of filenames. (1=Off, 0=On)
WSCDHOMEDRIVENUMBER=     66:REM Home drive number for |CD
WSCDHOMEDRIVELETTER=     67:REM Home drive letter for |CD
WSCDHOMETRACK=            68:REM Home track for |CD
WSCDHOMESECTOR=           69:REM Home sector for |CD
WSKMTESTKEYVALUE=        70:REM HL Value from KMTESTKEY on boot
WSRODOSUSERNUMBERLOW=    78:REM RODOS User number (0 to 255). Rodos supports 0-65535 but only 0-255 are supported
WSRODOSUSERNUMBERHIGH=   79:REM RODOS User number (current reserved, set to 255). Rodos supports 0-65535 but only 0-255 are supported
WSEXTRADRIVEPORTLOW=     88:REM Extra external disk drives port number (0-65535) low byte
WSEXTRADRIVEPORTHIGH=    89:REM Extra external disk drives port number (0-65535) high byte
WSORIGINALTXTOUTPUT=      90:REM Original contents of TXTOUTUT (&bb5a-&bb5c)
|load,"wspace.bin",&4000
work%=0:call &4000,@work%
DEF FNw$(opt,ws)="|opt "+str$(opt)+": "+str$(peek(work%+ws))
DEF FNp1(addr)=(peek(addr+1)*256)+peek(addr)
DEF FNc(ws)=peek(work%+ws)
Print "Now validating |OPT"
|opt,1,0:b=FNc(WSLOADINGMESSAGES):|opt,1,255:a=FNc(WSLOADINGMESSAGES):if a=b then "Error: Print OPT 1 didn't change"
|opt,2,0:b=FNc(WSCASESENSITIVITY):|opt,2,255:a=FNc(WSCASESENSITIVITY):if a=b then "Error: Print OPT 2 didn't change"
print "|opt 3 called":|opt,3,255
|opt,4,0:b=FNc(WSOVERWRITEFILE):|opt,4,2:a=FNc(WSOVERWRITEFILE):if a=b then print "Error: OPT 4 didn't change"
|opt,5,16:b=FNc(WSDISKERRORRETRYCOUNT):|opt,5,12:a=FNc(WSDISKERRORRETRYCOUNT):if a=b then print "Error: OPT 5 didn't change"
|opt,6,0:b=peek(&be78):|opt,6,255:a=peek(&be78):if a=b then print "Error: OPT 6 didn't change"
|opt,7,30:b=peek(&be44):|opt,7,80:a=peek(&be44):if a=b then print "Error: OPT 7 didn't change"
|opt,8,50*7:b=FNp1(&be46):|opt,8,60*7:a=FNp1(&be46):if a=b then print "Error: OPT 8 didn't change"
|opt,9,12:b=peek(&be4a):|opt,9,18:a=peek(&be4a):if a=b then print "Error: OPT 9 didn't change"
print "|opt 10 is broken":rem |opt,10,1
|opt,10,1:b=peek()  
|OPT,11,5:b=peek(&be4b):|opt,11,1:a=peek(&be4b):if a=b then print "Error: OPT 11 didn't change"
rem Note that opt 12 has a default of &faff
|opt,12,&fafe:b=FNp1(work%+WSEXTRADRIVEPORTLOW):|opt,12,&faff:b=FNp1(work%+WSEXTRADRIVEPORTLOW):if a=b then print "Error: OPT 12 didn't change"
Print "Not sure |Opt 13 updates anything":|opt,13,0
|opt,14,10:b=peek(&be49):|opt,14,18:a=peek(&be49):if a=b then print "Error: OPT 14 didn't change"
print "|Opt done!"
print "Hit any key to continue..."
a$="":while a$="":a$=inkey$:wend
|cls
Print "Formatting C"
|format,2,8
|a:|CAT:|dir
|b:|cat:|dir
|c:|cat:|dir
|cls
print "Making a 'validate' folder on C:"
|mkdir,"C:validate"
|C
|cd,"validate"
print "Copying files to C:"
|copy,"C:","A:*.*"
|c
openout "testfile.dat"
for a=0 to 255
|bput,a
Next
closeout
openin "testfile.dat"
for l=0 to 255
|point,l
a%=0:|bget,@a%
print  "Byte ";l;" of the file is",a%
Next
closein
|info,"testfile.dat"
|dump,"testfile.dat"
|copy,"testfile.dat.bak","testfile.dat"
|copy,"testfile.dat.1","testfile.dat"
|dir,"testfile.dat.bak"
|eb
|dir,"testfile.dat.bak"
|era,"testfile.dat"
|rm,"testfile.dat.1"
|mkdir,"B:linktest":|c:|link,"B:linktest":|dir:|info,"*.*"
openout "text.txt":print #9,"This is a test file":print "|dir":closeout:|list,"text.txt"
rem |load,"text.txt",&4000
rem |load,"text.txt",1
rem |load,"text.txt",&4000,4
|title,"test disk":|dir
|access,"text.txt",45
|alias,"H","help 7":|H
a%=0:|askram,@a%:print a%,"RAM found"
a$="H":|do,@a$
|exec,"text.txt"
a%=&1234:|poke,2,&100,@a%
a%=0:|peek,2,&100,a%:print hex$(a%)
|save,"save.bin",1,0,&1000
|spool,"spoolfile":list:|spool
rem print "Formatting B"
rem |format,2,9
rem print "Formatting A:"
rem |format,2,0
