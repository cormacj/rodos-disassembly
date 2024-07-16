# Testing notes

As of v2.22 RODOS is being packaged with a debug version of the ROM, and an
Amstrad .DSK file containing code to validate and verify the ROM.

The debug version of the ROM adds two commands not normally in the normal ROM:
* `|WS` - Reports the RODOS rom workspace address. Usage: a%=0:|WS,@a%:print hex$(a%)
* `CLEAR.ERROR` - This resets the RODOS error location (&BE6D). The test suite reads this after commands to determine the PASS/FAIL results of a command.

**The Test Environment**

Several roms have commands that conflict with RODOS, for example MAXAM 1.5 also has a `|SAVE` command. This means that it's best to the the test suite with RODOS being the only ROM enabled on the system.

The recommended configuration is:
RODOS-debug version in any rom slots between 1 to 6
rodosval.dsk in Drive A:
Any disk in Drive B:

There are two test suites:
1. **OPT-VAL.BAS** - This validates that `|OPT` is working as expected. It works by setting a `|OPT` value, recording the value at the documented location, then updating the same `|OPT` value again and ensuring that the value changed.  
2. **VALIDATE.BAS** - This tests functionality on some of the more complex areas of RODOS. As of V2.22 the following commands are tested:

    |EXEC
    |POKE
    |SAVE
    |MKDIR
    |FORMAT and |COPY
    |COPY
    |BPUT
    |POINT filesize test
    |BGET end of file test
    |BGET read
    |BGET with |POINT backwards read
    |BGET+|POINT random read tests
    |TITLE
    |SPOOL
    |TDUMP
