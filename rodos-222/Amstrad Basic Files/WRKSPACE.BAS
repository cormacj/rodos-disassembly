10 REM Code to find changes in the workspace after a command. 
20 REM in this case, we'll track what changes when the user number changes
30 rodos%=0:|WS,@rodos%
40 PRINT "Workspace is at &";HEX$(rodos%)
50 DIM a(&100)
60 DIM b(&100)
70 |USER,0
80 FOR l=0 TO &100:d=PEEK(rodos%+l):a(l)=d:NEXT
90 |USER,2
100 FOR l=0 TO &100:d=PEEK(rodos%+l):b(l)=d:NEXT  
110 FOR l=0 TO &100:IF a(l)<>b(l) THEN PRINT l;"changed from ";a(l);" to ";b(l)
120 NEXT

