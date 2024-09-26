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
540 REM |opy,"C:","A:*.*" "Copying files from B: to C:"
560 |COPY,"C:","B:*.*"
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
690 PRINT "Copying files to C:"
700 |COPY,"C:","B:*.*"
710 rtest$(rnum)="|COPY tests:"+pass$
720 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
730 REM ---- test 3 -----
740 rnum=rnum+1
750 |CLS
760 |CLEAR.ERROR
770 PRINT "Writing files to testfile.dat using |BPUT..."
780 OPENOUT "testfile.dat"
790 FOR a=0 TO 255
800 |BPUT,a
810 NEXT
820 CLOSEOUT
830 PRINT "Done"
840 IF FNe<>255 THEN rtest$(rnum)="|BPUT tests reported error code "+STR$(FNe)+fail$ ELSE rtest$(rnum)="|BPUT tests passed: "+pass$
850 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
860 REM ---- test 4 -----
870 rnum=rnum+1
880 |CLS
890 PRINT "Getting file size using |point..."
900 OPENIN "testfile.dat"
910 a%=0:|POINT,65535:|POINT,65535,@a%
920 CLOSEIN
930 IF a%=256 THEN rtest$(rnum)="|POINT filesize test:"+pass$ ELSE rtest$(rnum)="|POINT filesize test:"+fail$+" (Error code"+STR$(FNe)+")"
940 PRINT "File size is ";a%;" bytes"
950 filesize=a%-1
960 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
970 REM ---- test 5 -----
980 |CLS
990 rnum=rnum+1
1000 |CLEAR.ERROR
1010 PRINT "This next test will read past the end of file and should report a negative error number"
1020 PRINT "Checking testfile.dat forwards using |BGET only..."
1030 rtest$(rnum)="|BGET read tests:"+pass$
1040 OPENIN "testfile.dat"
1050 FOR l=0 TO filesize+1
1060 IF l>filesize THEN PRINT "Reading past end of file. Expect a negative error code:"
1070 a%=0:|BGET,@a%
1080 IF  a%<0 THEN rtest$(rnum)="|BGET end of file test:"+pass$ ELSE rtest$(rnum)="|BGET end of file test:"+fail$+"(Error code"+STR$(FNe)+")"
1090 NEXT
1100 CLOSEIN
1110 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
1120 REM ---- test 6 -----
1130 |CLS
1140 rnum=rnum+1
1150 |CLEAR.ERROR
1160 rtest$(rnum)="|BGET read passed:"+pass$
1170 OPENIN "testfile.dat"
1180 FOR l=0 TO filesize
1190 a%=0:|BGET,@a%
1200 PRINT  "Byte ";
1210 PRINT USING "####";l;
1220 PRINT " of the file is: ";
1230 PRINT USING "####";a%;
1240 PRINT CHR$(13);
1250 IF a%<>l THEN PRINT:PRINT "Error: |BGET failed - I expected ";l;" but got ";a%:rtest$(rnum)="|BGET read tests (Error code:"+STR$(FNe)+"):"+fail$
1260 NEXT
1270 CLOSEIN
1280 REM ---- test 7 -----
1290 rnum=rnum+1
1300 |CLEAR.ERROR
1310 rtest$(rnum)="|BGET with |POINT backwards test:"+pass$
1320 PRINT "Done"
1330 PRINT "Checking testfile.dat backwards using |POINT and |GET..."
1340 OPENIN "testfile.dat"
1350 FOR l=filesize-1 TO 0 STEP -1
1360 |POINT,l
1370 a%=0:|BGET,@a%
1380 PRINT  "Byte ";
1390 PRINT USING "####";l;
1400 PRINT " of the file is: ";
1410 PRINT USING "####";a%;
1420 PRINT CHR$(13);
1430 IF a%<>l THEN PRINT:PRINT "Error: |BGET  with |POINT backwards test failed - I expected ";l;" but got ";a%:rtest$(rnum)="Error: |BGET with |POINT backwards test:"+fail$
1440 NEXT
1450 CLOSEIN
1460 PRINT "Done"
1470 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
1480 REM ---- test 8 -----
1490 |CLS
1500 rnum=rnum+1
1510 |CLEAR.ERROR
1520 rtest$(rnum)="|BGET+|POINT random read tests:"+pass$
1530 PRINT "Checking testfile.dat randomly (1024 times)..."
1540 OPENIN "testfile.dat"
1550 |RANDOM
1560 FOR l=0 TO 1024
1570 pnt=(RND*256) MOD 256
1580 |POINT,pnt
1590 a%=0:|BGET,@a%
1600 PRINT  "Byte ";
1610 PRINT USING "####";pnt;
1620 PRINT " of the file is: ";
1630 PRINT USING "####";a%;
1640 PRINT CHR$(13);
1650 IF a%<>pnt THEN PRINT:PRINT "Error: |point failed - point ";pnt;" got ";a%:rtest$(rnum)="|BGET+|POINT random read tests:"+fail$+"(Error code"+STR$(FNe)+")"
1660 NEXT
1670 CLOSEIN
1680 PRINT "Done"
1690 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
1700 REM ---- test 9 -----
1710 |CLS
1720 rnum=rnum+1
1730 |CLEAR.ERROR
1740 PRINT "Title test..."
1750 |TITLE,"Title test"
1760 IF FNe<>255 THEN rtest$(rnum)="|TITLE tests reported error code "+STR$(FNe)+fail$ ELSE rtest$(rnum)="|TITLE tests passed: "+pass$
1770 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
1780 REM ---- test 10 -----
1790 |CLS
1800 rnum=rnum+1
1810 |CLEAR.ERROR
1820 PRINT "Info test..."
1830 |INFO,"testfile.dat"
1840 IF FNe<>255 THEN rtest$(rnum)="|INFO tests reported error code "+STR$(FNe)+fail$ ELSE rtest$(rnum)="|INFO tests passed: "+pass$
1850 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
1860 REM ---- test 11 -----
1870 |CLS
1880 |CLEAR.ERROR
1890 PRINT "Spool test..."
1900 |SPOOL,"spoolfile.txt"
1910 |LS
1920 |SPOOL
1930 IF FNe<>255 THEN rtest$(rnum)="|SPOOL tests reported error code "+STR$(FNe)+fail$ ELSE rtest$(rnum)="|SPOOL tests passed: "+pass$
1940 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
1950 REM ---- test 12 -----
1960 |CLS
1970 rnum=rnum+1
1980 |CLEAR.ERROR
1990 PRINT "TDUMP test..."
2000 |LIST,"spoolfile.txt"
2010 |TDUMP
2020 IF FNe<>255 THEN rtest$(rnum)="|TDUMP tests reported error code "+STR$(FNe)+fail$ ELSE rtest$(rnum)="|TDUMP tests passed: "+pass$
2030 IF a=0 THEN PRINT "Press any key...":WHILE INKEY$="":WEND
2050 REM ---- display test results-----
2060 |CLS
2070 FOR a=0 TO rnum:PRINT rtest$(a):NEXT
2080 END

