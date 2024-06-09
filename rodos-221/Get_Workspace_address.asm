nolist

;Use the custom RSX to get the workspace address
;
;Use:
; work%=0
; call &4000,@work%
;
org &4000
kl_find_command    equ &bcd4
;first get variable address
ld e,(ix):ld d,(ix+1)
push de ;save it for later

;Now find our custom RSX
ld hl, cmd_name ;pointer to command name
call kl_find_command ;ask kernel where it is
jr nc, error_routine ;command not found error
ld (cmd_far_address+0), hl ;store address
ld a,c
ld (cmd_far_address+2),a ;store rom number

;having found the far address of the routine it can now be called.
ld a, 0 ;0 parameters
ld ix, param_block ;address of parameter block
rst 3 ;far call
defw cmd_far_address ; pointer to far address

;Now pass it back to paramter variable
push hl:pop de ;RSX passes workspace in HL, transfer to DE
pop hl ;Now get variable addess
ld (hl),e:inc hl:ld (hl),d ;pass workspace loc back to variable

error_routine:
ret

param_block defw status ;first parameter is status
defw char ;second parameter is character
;
status: defw &0000
char: defw &0000
list ;so I know where to save to
cmd_name: defb &92 ;^R+&80
cmd_far_address: defs 3 ;3 byte area for storing far address and rom number
