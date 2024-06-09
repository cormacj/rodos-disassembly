AUTO
|opt,1,255
|opt,2,255:|opt,2,0
|opt,3,255
|opt,4,2
|opt,5,12
|opt,6,255
|opt,7,50
|opt,8,50*7
|opt,9,12
|opt,10,1
|opt,11,1
|opt,12,100
|opt,13,0
|opt,14,15
|cls
Print "Formatting C"
|format,2,8
print "Formatting B"
|format,2,9
|a:|CAT:|dir
|b:|cat:|dir
|c:|cat:|dir
|cls
print "Formatting C:"
|format,2,8
print "Making a 'validate' folder on C:"
|mkdir,"C:validate"
|C
|cd,"validate"
print "Copying files to C:"
|copy,"C:","A:*.*"
|c
print "Formatting A:"
|format,2,0
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
|load,"text.txt",&4000
|load,"text.txt",1
|load,"text.txt",&4000,4
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
