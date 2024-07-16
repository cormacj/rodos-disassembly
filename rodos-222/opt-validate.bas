10 REM |OPT Validator/Verifier
20 |CLS:PRINT "|OPT Validator/Verifier"
30 a=1
40 DIM rtest$(15)
50 REM define a checkmark
60 SYMBOL 240,3,6,12,204,216,240,96
70 pass$=" - Passed! "+CHR$(240)
80 fail$=" - Failed! "+"X"
90 WSRODOSROMNUMBER=         0 :REM The rom slot number of the RODOS rom
100 WSCPMROMNUMBER=           1 :REM The rom slot number of the CPM (aka AMSDOS) rom (normally slot 7) plus &30.
110                                     :REM If the rom is missing or non standard this value is &3F
120 WSCURRENTDRIVELETTER=     3:REM Current Drive Letter
130 WSDRIVENUMBER=             4:REM Current Drive Number (mostly used when patching (See Appendix F))
140 WSLOADINGMESSAGES=         8:REM Loading messages (|OPT,1,x)
150 WSDISKERRORRETRYCOUNT=   9:REM Disk read error retry count (|OPT,5,x)
160 WSEXPANSIONRAMCOUNT=      11:REM Expansion ram count (in 16k blocks)
170 WSSTARTPRBUFFBANK=        12:REM Printer Buffer Bank
180 WSSCREENOUTPUTVALUE=      17:REM Sets normal screen output according to n, where n is the sum of the following:
190                                     :REM     +1 - screen output to current open file.
200                                     :REM     +2 - screen output to printer.
210                                     :REM     +4 - screen echo disabled.
220 WSPREVIOUSDRIVELETTER=    19:REM Previous value of WSCURRENTDRIVELETTER
230 WSOVERWRITEFILE=           22:REM Overwrite file= 0=ask 1=overwrite 2=create backup
240 WSINPUTBUFFADDRFILE=     23:REM Address of input buffer (file header)
250 WSINPUTBUFFADDRSECTOR=   25:REM Address of input buffer (sector block)
260 WSOUTPUTBUFFADDRHEADER=  27:REM Address of output buffer (header)
270 WSOUTPUTBUFFADDRSECTOR=  29:REM Address of output buffer (sector block)
280 WSORIGINALKLFINDCOMMAND= 51:REM Original contents of KLFINDCOMMAND location. (KLFINDCOMMAND was patched by RODOS)
290 WSUSERNUMBER=              54:REM Current User Number
300 WSCASESENSITIVITY=         65:REM Case sensitivity of filenames. (1=Off, 0=On)
310 WSCDHOMEDRIVENUMBER=     66:REM Home drive number for |CD
320 WSCDHOMEDRIVELETTER=     67:REM Home drive letter for |CD
330 WSCDHOMETRACK=            68:REM Home track for |CD
340 WSCDHOMESECTOR=           69:REM Home sector for |CD
350 WSKMTESTKEYVALUE=        70:REM HL Value from KMTESTKEY on boot
360 WSRODOSUSERNUMBERLOW=    78:REM RODOS User number (0 to 255). Rodos supports 0-65535 but only 0-255 are supported
370 WSRODOSUSERNUMBERHIGH=   79:REM RODOS User number (current reserved, set to 255). Rodos supports 0-65535 but only 0-255 are supp
380 WSEXTRADRIVEPORTLOW=     88:REM Extra external disk drives port number (0-65535) low byte
390 WSEXTRADRIVEPORTHIGH=    89:REM Extra external disk drives port number (0-65535) high byte
400 WSORIGINALTXTOUTPUT=      90:REM Original contents of TXTOUTUT (&bb5a-&bb5c)
410 ON ERROR GOTO 690
420 work%=0:|WS,@work%
430 DEF FNw$(opt,ws)="|opt "+STR$(opt)+": "+STR$(PEEK(work%+ws))
440 DEF FNp1(addr)=(PEEK(addr+1)*256)+PEEK(addr)
450 DEF FNc(ws)=PEEK(work%+ws)
460 FOR a=1 TO 14:rtest$(a)="|OPT "+STR$(a)+pass$
470 NEXT
480 PRINT "Now validating |OPT"
490 |OPT,1,0:b=FNc(WSLOADINGMESSAGES):|OPT,1,255:a=FNc(WSLOADINGMESSAGES):IF a=b THEN "Error: Print OPT 1 didn't change":rtest$(1)="|OPT 1"+fail$
500 |OPT,2,0:b=FNc(WSCASESENSITIVITY):|OPT,2,255:a=FNc(WSCASESENSITIVITY):IF a=b THEN "Error: Print OPT 2 didn't change":rtest$(2)="|OPT 2"+fail$
510 REM in the next section CAS NOISY on a 6128 is &b118. On a 464 is &b800
520 |OPT,3,255:b=PEEK(&B118):|OPT,3,0:a=PEEK(&B118):IF a=b THEN PRINT "Error: Print OPT 3 didn't change":rtest$(3)="|OPT 3"+fail$
530 |OPT,4,0:b=FNc(WSOVERWRITEFILE):|OPT,4,2:a=FNc(WSOVERWRITEFILE):IF a=b THEN PRINT "Error: OPT 4 didn't change":rtest$(4)="|OPT 4"+fail$
540 |OPT,5,16:b=FNc(WSDISKERRORRETRYCOUNT):|OPT,5,12:a=FNc(WSDISKERRORRETRYCOUNT):IF a=b THEN PRINT "Error: OPT 5 didn't change":rtest$(5)="|OPT 5"+fail$
550 |OPT,6,0:b=PEEK(&BE78):|OPT,6,255:a=PEEK(&BE78):IF a=b THEN PRINT "Error: OPT 6 didn't change":rtest$(6)="|OPT 6"+fail$
560 |OPT,7,30:b=PEEK(&BE44):|OPT,7,80:a=PEEK(&BE44):IF a=b THEN PRINT "Error: OPT 7 didn't change":rtest$(7)="|OPT 7"+fail$
570 |OPT,8,50*7:b=FNp1(&BE46):|OPT,8,60*7:a=FNp1(&BE46):IF a=b THEN PRINT "Error: OPT 8 didn't change":rtest$(8)="|OPT 8"+fail$
580 |OPT,9,12:b=PEEK(&BE4A):|OPT,9,18:a=PEEK(&BE4A):IF a=b THEN PRINT "Error: OPT 9 didn't change":rtest$(9)="|OPT 9"+fail$
590 |OPT,10,5:b=PEEK(&BE48):|OPT,10,1:a=PEEK(&BE48):IF a=b THEN PRINT "Error: OPT 10 didn't change":rtest$(10)="|OPT 10"+fail$
600 |OPT,11,5:b=PEEK(&BE4B):|OPT,11,1:a=PEEK(&BE4B):IF a=b THEN PRINT "Error: OPT 11 didn't change":rtest$(11)="|OPT 11"+fail$
610 REM Note that opt 12 has a default of &faff
620 |OPT,12,&FAFE:b=FNp1(work%+WSEXTRADRIVEPORTLOW):|OPT,12,&FAFF:b=FNp1(work%+WSEXTRADRIVEPORTLOW):IF a=b THEN PRINT "Error: OPT 12 didn't change":rtest$(12)="|OPT 12"+fail$
640 |OPT,13,0:b=FNc(WSCASESENSITIVITY):|OPT,13,255:b=FNc(WSCASESENSITIVITY):IF a=b THEN PRINT "Error: OPT 13 didn't change":rtest$(13)="|OPT 13"+fail$
650 |OPT,14,10:b=PEEK(&BE49):|OPT,14,18:a=PEEK(&BE49):IF a=b THEN PRINT "Error: OPT 14 didn't change":rtest$(14)="|OPT 14"+fail$
660 PRINT "|Opt done!"
670 FOR a=1 TO 14:PRINT rtest$(a):NEXT
680 END
690 PRINT "RODOS ROM is not in debug mode!"
700 END
