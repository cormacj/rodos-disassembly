AUTO
rem a=0 enable pauses, a=1 is automatic
a=1
e=&BE6D:rem this is where RODOS stores error codes if it generates one
Def FNe=peek(e)
dim rtest$(30)
rnum=0
rem define a checkmark
symbol 240,3,6,12,204,216,240,96
pass$=chr$(9)+chr$(240)
fail$=chr$(9)+"X"
|clear.error
openout "exectest"
print #9,"|cls"
print #9,"|opt,1,255"
print #9,"|opt,4,1"
print #9,"|opt,6,255"
closeout
|exec,"exectest"
if fne<>255 then rtest$(rnum)="|EXEC tests reported error code "+str$(Fne)+fail$ else rtest$(rnum)="|EXEC tests passed: "+pass$
print rtest$(rnum):call &bb18
|cls
rem ---- test 0 -----
rem do this before formatting C:
rnum=rnum+1
|cls
|clear.error
Print "POKE test..."
tmpstr$="RODOS was a high-capacity disc operating system released by Romantic Robot, whose main aim was to allow you to make use of the extra capacity on 3.5in discs. It shipped on a single 16k sideways ROM."
strspot=1
strlen=len(tmpstr$)
for a=0 to &3fff
mydata=asc(mid$(tmpstr$,(a mod strlen)+1,1))
print "&";hex$(a);" - ";chr$(mydata);chr$(13);
|poke,1,a,mydata
Next
if fne<>255 then rtest$(rnum)="|POKE tests reported error code "+str$(Fne)+fail$ else rtest$(rnum)="|POKE tests passed: "+pass$
if a=0 then Print "Press any key...":while inkey$="":wend
rem ---- test 0.5 -----
Print "Saving bank 1, using |SAVE..."
rnum=rnum+1
|clear.error
|save,"bank1.txt",1
print "Save Complete"
if fne<>255 then rtest$(rnum)="|SAVE test reported error code "+str$(Fne)+fail$ else rtest$(rnum)="|SAVE tests passed: "+pass$
if a=0 then Print "Press any key...":while inkey$="":wend
rem ---- test 1 -----
rnum=rnum+1
|clear.error
Print "Formatting C"
|format,2,8
rtest$(rnum)="|FORMAT tests:"+pass$
|dir,"C:"
rem Print "Copying files from A: to C:"
rem |copy,"C:","A:*.*"
Print "Copying files from B: to C:"
|copy,"C:","B:*.*"
if fne<>255 then rtest$(rnum)="|FORMAT and |COPY tests reported error code "+str$(Fne)+fail$ else rtest$(rnum)="|FORMAT and |COPY tests passed: "+pass$
|c:|cat
if a=0 then print "Hit any key...":call &bb18
rem ---- test 2 -----
rnum=rnum+1
|cls
|clear.error
print "Making a 'validate' folder on C:"
|a:|mkdir,"c:mkdir-while-on-a"
|c:|mkdir,"C:validate"
if fne<>255 then rtest$(rnum)="|MKDIR tests reported error code "+str$(Fne)+fail$ else rtest$(rnum)="|MKDIR tests passed: "+pass$
|c:|cd,"validate"
print "Copying files to C:"
|copy,"C:","B:*.*"
rtest$(rnum)="|COPY tests:"+pass$
if a=0 then Print "Press any key...":while inkey$="":wend
rem ---- test 3 -----
rnum=rnum+1
|cls
|clear.error
print "Writing files to testfile.dat using |BPUT..."
openout "testfile.dat"
for a=0 to 255
|bput,a
Next
closeout
print "Done"
if fne<>255 then rtest$(rnum)="|BPUT tests reported error code "+str$(Fne)+fail$ else rtest$(rnum)="|BPUT tests passed: "+pass$
if a=0 then Print "Press any key...":while inkey$="":wend
rem ---- test 4 -----
rnum=rnum+1
|cls
print "Getting file size using |point..."
openin "testfile.dat"
a%=0:|point,65535:|point,65535,@a%
closein
if a%=256 then rtest$(rnum)="|POINT filesize test:"+pass$ else rtest$(rnum)="|POINT filesize test:"+fail$+" (Error code"+str$(Fne)+")"
print "File size is ";a%;" bytes"
filesize=a%-1
if a=0 then Print "Press any key...":while inkey$="":wend
rem ---- test 5 -----
|cls
rnum=rnum+1
|clear.error
print "This next test will read past the end of file and should report a negative error number"
print "Checking testfile.dat forwards using |BGET only..."
rtest$(rnum)="|BGET read tests:"+pass$
openin "testfile.dat"
for l=0 to filesize+1
if l>filesize then print "Reading past end of file. Expect a negative error code:"
a%=0:|bget,@a%
if  a%<0 then rtest$(rnum)="|BGET end of file test:"+pass$ else rtest$(rnum)="|BGET end of file test:"+fail$+"(Error code"+str$(Fne)+")"
next
closein
if a=0 then Print "Press any key...":while inkey$="":wend
rem ---- test 6 -----
|cls
rnum=rnum+1
|clear.error
rtest$(rnum)="|BGET read passed:"+pass$
openin "testfile.dat"
for l=0 to filesize
a%=0:|bget,@a%
print  "Byte ";
print using "####";l;
print " of the file is: ";
print using "####";a%;
print chr$(13);
if a%<>l then print:print "Error: |BGET failed - I expected ";l;" but got ";a%:rtest$(rnum)="|BGET read tests (Error code:"+str$(Fne)+"):"+fail$
Next
closein
rem ---- test 7 -----
rnum=rnum+1
|clear.error
rtest$(rnum)="|BGET with |POINT backwards test:"+pass$
print "Done"
print "Checking testfile.dat backwards using |POINT and |GET..."
openin "testfile.dat"
for l=filesize-1 to 0 step -1
|point,l
a%=0:|bget,@a%
print  "Byte ";
print using "####";l;
print " of the file is: ";
print using "####";a%;
print chr$(13);
if a%<>l then print:print "Error: |BGET  with |POINT backwards test failed - I expected ";l;" but got ";a%:rtest$(rnum)="Error: |BGET with |POINT backwards test:"+fail$
Next
closein
print "Done"
if a=0 then Print "Press any key...":while inkey$="":wend
rem ---- test 8 -----
|cls
rnum=rnum+1
|clear.error
rtest$(rnum)="|BGET+|POINT random read tests:"+pass$
print "Checking testfile.dat randomly (1024 times)..."
openin "testfile.dat"
|random
for l=0 to 1024
pnt=(rnd*256) mod 256
|point,pnt
a%=0:|bget,@a%
print  "Byte ";
print using "####";pnt;
print " of the file is: ";
print using "####";a%;
print chr$(13);
if a%<>pnt then print:print "Error: |point failed - point ";pnt;" got ";a%:rtest$(rnum)="|BGET+|POINT random read tests:"+fail$+"(Error code"+str$(Fne)+")"
Next
closein
print "Done"
if a=0 then Print "Press any key...":while inkey$="":wend
rem ---- test 9 -----
|cls
rnum=rnum+1
|clear.error
Print "Title test..."
|title,"Title test"
if fne<>255 then rtest$(rnum)="|TITLE tests reported error code "+str$(Fne)+fail$ else rtest$(rnum)="|TITLE tests passed: "+pass$
if a=0 then Print "Press any key...":while inkey$="":wend
rem ---- test 10 -----
|cls
rnum=rnum+1
|clear.error
Print "Info test..."
|info,"testfile.dat"
if fne<>255 then rtest$(rnum)="|INFO tests reported error code "+str$(Fne)+fail$ else rtest$(rnum)="|INFO tests passed: "+pass$
if a=0 then Print "Press any key...":while inkey$="":wend
rem ---- test 11 -----
|cls
|clear.error
Print "Spool test..."
|spool,"spoolfile.txt"
|ls
|spool
if fne<>255 then rtest$(rnum)="|SPOOL tests reported error code "+str$(Fne)+fail$ else rtest$(rnum)="|SPOOL tests passed: "+pass$
if a=0 then Print "Press any key...":while inkey$="":wend
rem ---- test 12 -----
|cls
rnum=rnum+1
|clear.error
Print "TDUMP test..."
|list,"spoolfile.txt"
|tdump
if fne<>255 then rtest$(rnum)="|TDUMP tests reported error code "+str$(Fne)+fail$ else rtest$(rnum)="|TDUMP tests passed: "+pass$
if a=0 then Print "Press any key...":while inkey$="":wend

rem ---- display test results-----
|cls
for a=0 to rnum:print rtest$(a):Next
end
