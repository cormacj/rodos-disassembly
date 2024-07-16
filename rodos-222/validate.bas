10 REM a=0 enable pauses, a=1 is automatic
20 a=1
30 e=&BE6D:REM this is where RODOS stores error codes if it generates one
40 DEF FNe=PEEK(e)
50 DIM rtest$(30)
60 rnum=0
70 REM define a checkmark
80 SYMBOL 240,3,6,12,204,216,240,96
90 pass$=CHR$(9)+CHR$(240)
100 fail$=CHR$(9)+"X"
110 |CLEAR.ERROR
120 OPENOUT "exectest"
130 PRINT #9,"|cls"
140 PRINT #9,"|opt,1,255"
150 PRINT #9,"|opt,4,1"
160 PRINT #9,"|opt,6,255"
170 CLOSEOUT
180 |EXEC,"exectest"
190 IF FNe<>255 THEN rtest$(rnum)="|EXEC tests reported error code "+STR$(FNe)+fail$ ELSE rtest$(rnum)="|EXEC tests passed: "+pass$
200 PRINT rtest$(rnum)
210 |CLS
220 REM ---- test 0 -----
230 REM do this before formatting C:
240 rnum=rnum+1
250 |CLS
260 |CLEAR.ERROR
270 PRINT "POKE test..."
280 tmpstr$="RODOS was a high-capacity disc operating system released by Romantic Robot, whose main aim was to allow you to make use of the extra capacity on 3.5in discs. It shipped on a single 16k sideways ROM."
290 strspot=1
300 strlen=LEN(tmpstr$)
310 FOR a=0 TO &3FFF
320 mydata=ASC(MID$(tmpstr$,(a MOD strlen)+1,1))
330 PRINT "&";HEX$(a);" - ";CHR$(mydata);CHR$(13);
340 |POKE,1,a,mydata
350 NEXT
360 IF FNe<>255 THEN rtest$(rnum)="|POKE tests reported error code "+STR$(FNe)+fail$ ELSE rtest$(rnum)="|POKE tests passed: "+pass$
370 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
380 REM ---- test 0.5 -----
390 PRINT "Saving bank 1, using |SAVE..."
400 rnum=rnum+1
410 |CLEAR.ERROR
420 |SAVE,"bank1.txt",1
430 PRINT "Save Complete"
440 IF FNe<>255 THEN rtest$(rnum)="|SAVE test reported error code "+STR$(FNe)+fail$ ELSE rtest$(rnum)="|SAVE tests passed: "+pass$
450 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
460 REM ---- test 1 -----
470 rnum=rnum+1
480 |CLEAR.ERROR
490 PRINT "Formatting C"
500 |FORMAT,2,8
510 rtest$(rnum)="|FORMAT tests:"+pass$
520 |DIR,"C:"
530 REM Print "Copying files from A: to C:"
540 REM |copy,"C:","A:*.*"
550 PRINT "Copying files from B: to C:"
560 |COPY,"C:","A:*.*"
570 IF FNe<>255 THEN rtest$(rnum)="|FORMAT and |COPY tests reported error code "+STR$(FNe)+fail$ ELSE rtest$(rnum)="|FORMAT and |COPY tests passed: "+pass$
580 |C:|CAT
590 IF a=0 THEN PRINT "Hit any key...":CALL &BB18
600 REM ---- test 2 -----
610 rnum=rnum+1
620 |CLS
630 |CLEAR.ERROR
640 PRINT "Making a 'validate' folder on C:"
650 |A:|MKDIR,"c:mkdir-while-on-a"
660 |C:|MKDIR,"C:validate"
670 IF FNe<>255 THEN rtest$(rnum)="|MKDIR tests reported error code "+STR$(FNe)+fail$ ELSE rtest$(rnum)="|MKDIR tests passed: "+pass$
680 |C:|CD,"validate"
690 rnum=rnum+1
700 PRINT "Copying files to C:"
710 |COPY,"C:","A:*.*"
720 rtest$(rnum)="|COPY tests:"+pass$
730 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
740 REM ---- test 3 -----
750 rnum=rnum+1
760 |CLS
770 |CLEAR.ERROR
780 PRINT "Writing files to testfile.dat using |BPUT..."
790 OPENOUT "testfile.dat"
800 FOR a=0 TO 255
810 |BPUT,a
820 NEXT
830 CLOSEOUT
840 PRINT "Done"
850 IF FNe<>255 THEN rtest$(rnum)="|BPUT tests reported error code "+STR$(FNe)+fail$ ELSE rtest$(rnum)="|BPUT tests passed: "+pass$
860 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
870 REM ---- test 4 -----
880 rnum=rnum+1
890 |CLS
900 PRINT "Getting file size using |point..."
910 OPENIN "testfile.dat"
920 a%=0:|POINT,65535:|POINT,65535,@a%
930 CLOSEIN
940 IF a%=256 THEN rtest$(rnum)="|POINT filesize test:"+pass$ ELSE rtest$(rnum)="|POINT filesize test:"+fail$+" (Error code"+STR$(FNe)+")"
950 PRINT "File size is ";a%;" bytes"
960 filesize=a%-1
970 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
980 REM ---- test 5 -----
990 |CLS
1000 rnum=rnum+1
1010 |CLEAR.ERROR
1020 PRINT "This next test will read past the end of file and should report a negative error number"
1030 PRINT "Checking testfile.dat forwards using |BGET only..."
1040 rtest$(rnum)="|BGET read tests:"+pass$
1050 OPENIN "testfile.dat"
1060 FOR l=0 TO filesize+1
1070 IF l>filesize THEN PRINT "Reading past end of file. Expect a negative error code:"
1080 a%=0:|BGET,@a%
1090 IF  a%<0 THEN rtest$(rnum)="|BGET end of file test:"+pass$ ELSE rtest$(rnum)="|BGET end of file test:"+fail$+"(Error code"+STR$(FNe)+")"
1100 NEXT
1110 CLOSEIN
1120 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
1130 REM ---- test 6 -----
1140 |CLS
1150 rnum=rnum+1
1160 |CLEAR.ERROR
1170 rtest$(rnum)="|BGET read passed:"+pass$
1180 OPENIN "testfile.dat"
1190 FOR l=0 TO filesize
1200 a%=0:|BGET,@a%
1210 PRINT  "Byte ";
1220 PRINT USING "####";l;
1230 PRINT " of the file is: ";
1240 PRINT USING "####";a%;
1250 PRINT CHR$(13);
1260 IF a%<>l THEN PRINT:PRINT "Error: |BGET failed - I expected ";l;" but got ";a%:rtest$(rnum)="|BGET read tests (Error code:"+STR$(FNe)+"):"+fail$
1270 NEXT
1280 CLOSEIN
1290 REM ---- test 7 -----
1300 rnum=rnum+1
1310 |CLEAR.ERROR
1320 rtest$(rnum)="|BGET with |POINT backwards test:"+pass$
1330 PRINT "Done"
1340 PRINT "Checking testfile.dat backwards using |POINT and |GET..."
1350 OPENIN "testfile.dat"
1360 FOR l=filesize-1 TO 0 STEP -1
1370 |POINT,l
1380 a%=0:|BGET,@a%
1390 PRINT  "Byte ";
1400 PRINT USING "####";l;
1410 PRINT " of the file is: ";
1420 PRINT USING "####";a%;
1430 PRINT CHR$(13);
1440 IF a%<>l THEN PRINT:PRINT "Error: |BGET  with |POINT backwards test failed - I expected ";l;" but got ";a%:rtest$(rnum)="Error: |BGET with |POINT backwards test:"+fail$
1450 NEXT
1460 CLOSEIN
1470 PRINT "Done"
1480 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
1490 REM ---- test 8 -----
1500 |CLS
1510 rnum=rnum+1
1520 |CLEAR.ERROR
1530 rtest$(rnum)="|BGET+|POINT random read tests:"+pass$
1540 PRINT "Checking testfile.dat randomly (1024 times)..."
1550 OPENIN "testfile.dat"
1560 |RANDOM
1570 FOR l=0 TO 1024
1580 pnt=(RND*256) MOD 256
1590 |POINT,pnt
1600 a%=0:|BGET,@a%
1610 PRINT  "Byte ";
1620 PRINT USING "####";pnt;
1630 PRINT " of the file is: ";
1640 PRINT USING "####";a%;
1650 PRINT CHR$(13);
1660 IF a%<>pnt THEN PRINT:PRINT "Error: |point failed - point ";pnt;" got ";a%:rtest$(rnum)="|BGET+|POINT random read tests:"+fail$+"(Error code"+STR$(FNe)+")"
1670 NEXT
1680 CLOSEIN
1690 PRINT "Done"
1700 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
1710 REM ---- test 9 -----
1720 |CLS
1730 rnum=rnum+1
1740 |CLEAR.ERROR
1750 PRINT "Title test..."
1760 |TITLE,"Title test"
1770 IF FNe<>255 THEN rtest$(rnum)="|TITLE tests reported error code "+STR$(FNe)+fail$ ELSE rtest$(rnum)="|TITLE tests passed: "+pass$
1780 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
1790 REM ---- test 10 -----
1800 |CLS
1810 rnum=rnum+1
1820 |CLEAR.ERROR
1830 PRINT "Info test..."
1840 |INFO,"testfile.dat"
1850 IF FNe<>255 THEN rtest$(rnum)="|INFO tests reported error code "+STR$(FNe)+fail$ ELSE rtest$(rnum)="|INFO tests passed: "+pass$
1860 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
1870 REM ---- test 11 -----
1880 |CLS
1890 |CLEAR.ERROR
1900 PRINT "Spool test..."
1910 |SPOOL,"spoolfile.txt"
1920 |LS
1930 |SPOOL
1940 IF FNe<>255 THEN rtest$(rnum)="|SPOOL tests reported error code "+STR$(FNe)+fail$ ELSE rtest$(rnum)="|SPOOL tests passed: "+pass$
1950 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
1960 REM ---- test 12 -----
1970 |CLS
1980 rnum=rnum+1
1990 |CLEAR.ERROR
2000 PRINT "TDUMP test..."
2010 |LIST,"spoolfile.txt"
2020 |TDUMP
2030 IF FNe<>255 THEN rtest$(rnum)="|TDUMP tests reported error code "+STR$(FNe)+fail$ ELSE rtest$(rnum)="|TDUMP tests passed: "+pass$
2040 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
2050 REM ---- display test results-----
2060 |CLS
2070 FOR a=0 TO rnum:PRINT rtest$(a):NEXT
2080 END
