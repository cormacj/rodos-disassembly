AUTO
|cls
|opt,1,255
Print "Formatting C"
|format,2,8
|dir,"C:"
Print "Copying files from A: to C:"
|copy,"C:","A:*.*"
Print "Copying files from B: to C:"
|copy,"C:","B:*.*"
|c:|cat
print "Hit any key...":call &bb18
|cls
print "Making a 'validate' folder on C:"
|mkdir,"C:validate"
|C
|cd,"validate"
print "Copying files to C:"
|copy,"C:","A:*.*"
|c
print "Writing files to testfile.dat using |BPUT..."
openout "testfile.dat"
for a=0 to 255
|bput,a
Next
print "Done"
closeout
print "Checking testfile.dat..."
openin "testfile.dat"
for l=255 to 0 step -1
|point,l
a%=0:|bget,@a%
print  "Byte ";
print using "####";l;
print " of the file is: ";
print using "####";a%;
print chr$(13);
if a%<>l then print:print "Error: |BGET failed - I asked for data. I expected ";pnt;" but got ";a%
Next
closein
|spool,"pntval"
openin "testfile.dat"
|random
for l=0 to 255
pnt=(rnd*256) mod 256
|point,pnt
a%=0:|bget,@a%
print  "Byte ";
print using "####";pnt;
print " of the file is: ";
print using "####";a%;
print chr$(13);
if a%<>pnt then print:print "Error: |point failed - point ";pnt;" got ";a%
rem t = TIME: WHILE TIME < t + (300 * 1):WEND
Next
closein
|spool
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
openout "text.txt":print #9,"Print"+chr$(34)+"This is a test file"+chr$(34):print "|dir":closeout:|list,"text.txt"
|exec,"text.txt"
rem |load,"text.txt",&4000
rem |load,"text.txt",1
rem |load,"text.txt",&4000,4
|title,"test disk":|dir
|access,"text.txt",45
|alias,"H","help 7":|H
a%=0:|askram,@a%:print a%,"RAM found"
a$="H":|do,@a$
a%=&1234:|poke,2,&100,@a%
a%=0:|peek,2,&100,a%:print hex$(a%)
|save,"save.bin",1,0,&1000
|spool,"spoolfile":list:|spool
rem print "Formatting B"
rem |format,2,9
rem print "Formatting A:"
rem |format,2,0
