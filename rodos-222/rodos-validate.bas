AUTO
rem a=0 enable pauses, a=1 is automatic
a=1
dim rtest$(15)
rem define a checkmark
symbol 240,3,6,12,204,216,240,96
print chr$(240)
pass$=chr$(9)+chr$(240)
fail$=chr$(9)+"X"
|cls
|opt,1,255
Print "Formatting C"
|format,2,8
rtest$(0)="|FORMAT tests:"+pass$
|dir,"C:"
Print "Copying files from A: to C:"
|copy,"C:","A:*.*"
Print "Copying files from B: to C:"
|copy,"C:","B:*.*"
|c:|cat
if a=0 then print "Hit any key...":call &bb18
|cls
print "Making a 'validate' folder on C:"
|a:|mkdir,"c:mkdir-while-on-a"
|c:|mkdir,"C:validate"
|C
|cd,"validate"
print "Copying files to C:"
|copy,"C:","B:*.*"
rtest$(1)="|COPY tests:"+pass$
if a=0 then Print "Press any key...":while inkey$="":wend
|cls
print "Writing files to testfile.dat using |BPUT..."
openout "testfile.dat"
for a=0 to 255
|bput,a
Next
closeout
print "Done"
rtest$(2)="|BPUT test:"+pass$
if a=0 then Print "Press any key...":while inkey$="":wend
|cls
print "Getting file size using |point..."
openin "testfile.dat"
a%=0:|point,65535:|point,65535,@a%
closein
if a%=256 then rtest$(3)="|POINT filesize test:"+pass$ else rtest$(3)="|POINT filesize test:"+fail$
print "File size is ";a%;" bytes"
filesize=a%
if a=0 then Print "Press any key...":while inkey$="":wend
|cls
print "This next test will read past the end of file and should report a negative error number"
print "Checking testfile.dat forwards using |BGET only..."
rtest$(5)="|BGET read tests:"+pass$
openin "testfile.dat"
for l=0 to filesize+1
if l>filesize then print "Reading past end of file. Expect a negative error code:"
a%=0:|bget,@a%
if l>filesize and a%<0 then rtest$(4)="|BGET end of file test:"+pass$ else rtest$(4)="|BGET end of file test:"+fail$
next
for l=0 to filesize
if l>filesize then print "Reading past end of file. Expect a negative error code:"
a%=0:|bget,@a%
print  "Byte ";
print using "####";l;
print " of the file is: ";
print using "####";a%;
print chr$(13);
if a%<>l then print:print "Error: |BGET failed - I expected ";l;" but got ";a%:rtest$(5)="|BGET read tests:"+fail$
Next
closein
print "Done"
if a=0 then Print "Press any key...":while inkey$="":wend
|cls
print "Checking testfile.dat backwards using |POINT and |GET..."
openin "testfile.dat"
for l=filesize to 0 step -1
|point,l
a%=0:|bget,@a%
print  "Byte ";
print using "####";l;
print " of the file is: ";
print using "####";a%;
print chr$(13);
if a%<>l then print:print "Error: |BGET failed - I expected ";l;" but got ";a%:rtest$(5)="|BGET read tests:"+fail$
Next
closein
print "Done"
Print "Press any key...":while inkey$="":wend
|cls
print "Checking testfile.dat randomly (256 times)..."
openin "testfile.dat"
|random
for l=0 to 256
pnt=(rnd*256) mod 256
|point,pnt
a%=0:|bget,@a%
print  "Byte ";
print using "####";pnt;
print " of the file is: ";
print using "####";a%;
print chr$(13);
if a%<>pnt then print:print "Error: |point failed - point ";pnt;" got ";a%:rtest$(5)="|BGET read tests:"+fail$
rem t = TIME: WHILE TIME < t + (300 * 1):WEND
Next
closein
print "Done"
if a=0 then Print "Press any key...":while inkey$="":wend
|cls
for a=0 to 15:print rtest$(a):Next
end
print "Testing |Spool"
|spool
|spool,"spoolfile"
|info,"testfile.dat"
|dump,"testfile.dat"
|spool
print "Done"
Print "Press any key to print the spoolfile...":while inkey$="":wend
|cls
|list,"spoolfile"
Print "Press any key...":while inkey$="":wend
end
|copy,"testfile.dat.bak","testfile.dat"
|copy,"testfile.dat.1","testfile.dat"
|dir,"testfile.dat.bak"
cat
Print "Erasing backup files"
|eb
cat
Print "Press any key...":while inkey$="":wend
end
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
