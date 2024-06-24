1 REM |OPT Validator/Verifier
5 |CLS:PRINT "|OPT Validator/Verifier" 
10 WSRODOSROMNUMBER=         0 :REM The rom slot number of the RODOS rom
20 WSCPMROMNUMBER=           1 :REM The rom slot number of the CPM (aka AMSDOS) rom (normally slot 7) plus &30.
30                                     :REM If the rom is missing or non standard this value is &3F
40 WSCURRENTDRIVELETTER=     3:REM Current Drive Letter
50 WSDRIVENUMBER=             4:REM Current Drive Number (mostly used when patching (See Appendix F))
60 WSLOADINGMESSAGES=         8:REM Loading messages (|OPT,1,x)
70 WSDISKERRORRETRYCOUNT=   9:REM Disk read error retry count (|OPT,5,x)
80 WSEXPANSIONRAMCOUNT=      11:REM Expansion ram count (in 16k blocks)
90 WSSTARTPRBUFFBANK=        12:REM Printer Buffer Bank
100 WSSCREENOUTPUTVALUE=      17:REM Sets normal screen output according to n, where n is the sum of the following:
110                                     :REM     +1 - screen output to current open file.
120                                     :REM     +2 - screen output to printer.
130                                     :REM     +4 - screen echo disabled.
140 WSPREVIOUSDRIVELETTER=    19:REM Previous value of WSCURRENTDRIVELETTER
150 WSOVERWRITEFILE=           22:REM Overwrite file= 0=ask 1=overwrite 2=create backup
160 WSINPUTBUFFADDRFILE=     23:REM Address of input buffer (file header)
170 WSINPUTBUFFADDRSECTOR=   25:REM Address of input buffer (sector block)
180 WSOUTPUTBUFFADDRHEADER=  27:REM Address of output buffer (header)
190 WSOUTPUTBUFFADDRSECTOR=  29:REM Address of output buffer (sector block)
200 WSORIGINALKLFINDCOMMAND= 51:REM Original contents of KLFINDCOMMAND location. (KLFINDCOMMAND was patched by RODOS)
210 WSUSERNUMBER=              54:REM Current User Number
220 WSCASESENSITIVITY=         65:REM Case sensitivity of filenames. (1=Off, 0=On)
230 WSCDHOMEDRIVENUMBER=     66:REM Home drive number for |CD
240 WSCDHOMEDRIVELETTER=     67:REM Home drive letter for |CD
250 WSCDHOMETRACK=            68:REM Home track for |CD
260 WSCDHOMESECTOR=           69:REM Home sector for |CD
270 WSKMTESTKEYVALUE=        70:REM HL Value from KMTESTKEY on boot
280 WSRODOSUSERNUMBERLOW=    78:REM RODOS User number (0 to 255). Rodos supports 0-65535 but only 0-255 are supported
290 WSRODOSUSERNUMBERHIGH=   79:REM RODOS User number (current reserved, set to 255). Rodos supports 0-65535 but only 0-255 are supp
orted
300 WSEXTRADRIVEPORTLOW=     88:REM Extra external disk drives port number (0-65535) low byte
310 WSEXTRADRIVEPORTHIGH=    89:REM Extra external disk drives port number (0-65535) high byte
320 WSORIGINALTXTOUTPUT=      90:REM Original contents of TXTOUTUT (&bb5a-&bb5c)
330 ON ERROR GOTO 1000
340 work%=0:|WS,@work%
350 DEF FNw$(opt,ws)="|opt "+STR$(opt)+": "+STR$(PEEK(work%+ws))
360 DEF FNp1(addr)=(PEEK(addr+1)*256)+PEEK(addr)
370 DEF FNc(ws)=PEEK(work%+ws)
380 PRINT "Now validating |OPT"
390 |OPT,1,0:b=FNc(WSLOADINGMESSAGES):|OPT,1,255:a=FNc(WSLOADINGMESSAGES):IF a=b THEN "Error: Print OPT 1 didn't change"
400 |OPT,2,0:b=FNc(WSCASESENSITIVITY):|OPT,2,255:a=FNc(WSCASESENSITIVITY):IF a=b THEN "Error: Print OPT 2 didn't change"
410 PRINT "|opt 3 called":|OPT,3,255
420 |OPT,4,0:b=FNc(WSOVERWRITEFILE):|OPT,4,2:a=FNc(WSOVERWRITEFILE):IF a=b THEN PRINT "Error: OPT 4 didn't change"
430 |OPT,5,16:b=FNc(WSDISKERRORRETRYCOUNT):|OPT,5,12:a=FNc(WSDISKERRORRETRYCOUNT):IF a=b THEN PRINT "Error: OPT 5 didn't change"
440 |OPT,6,0:b=PEEK(&BE78):|OPT,6,255:a=PEEK(&BE78):IF a=b THEN PRINT "Error: OPT 6 didn't change"
450 |OPT,7,30:b=PEEK(&BE44):|OPT,7,80:a=PEEK(&BE44):IF a=b THEN PRINT "Error: OPT 7 didn't change"
460 |OPT,8,50*7:b=FNp1(&BE46):|OPT,8,60*7:a=FNp1(&BE46):IF a=b THEN PRINT "Error: OPT 8 didn't change"
470 |OPT,9,12:b=PEEK(&BE4A):|OPT,9,18:a=PEEK(&BE4A):IF a=b THEN PRINT "Error: OPT 9 didn't change"
490 |OPT,10,5:b=PEEK(&BE48):|OPT,10,1:a=PEEK(&BE48):IF a=b THEN PRINT "Error: OPT 10 didn't change"
500 |OPT,11,5:b=PEEK(&BE4B):|OPT,11,1:a=PEEK(&BE4B):IF a=b THEN PRINT "Error: OPT 11 didn't change"
510 REM Note that opt 12 has a default of &faff
520 |OPT,12,&FAFE:b=FNp1(work%+WSEXTRADRIVEPORTLOW):|OPT,12,&FAFF:b=FNp1(work%+WSEXTRADRIVEPORTLOW):IF a=b THEN PRINT "Error: OPT 12
 didn't change"
530 PRINT "Not sure |Opt 13 updates anything":|OPT,13,0
540 |OPT,14,10:b=PEEK(&BE49):|OPT,14,18:a=PEEK(&BE49):IF a=b THEN PRINT "Error: OPT 14 didn't change"
550 PRINT "|Opt done!"
560 END
1000 PRINT "RODOS ROM is not in debug mode!"
1010 END
