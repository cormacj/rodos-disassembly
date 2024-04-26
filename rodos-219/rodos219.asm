; z80dasm 1.1.6
; command line: z80dasm -b blockfile.txt -g 0xc000 -S Firmware_labels.txt -s syms.txt -r default -l -t -v RODOS219.ROM

org	0c000h
POST_BOOT_MSG: equ 0xbec1  ;This location is overwritten by the relocation code in ROM_SELECT_DESELECT_RELOCATED - fixed in v2.20
;This is where the |zap and |rom store the command data.
;The first 10 characters are stomped on by what looks like calls
ROM_SELECT_DESELECT_RELOCATED:equ 0xbec0

;RODOS error message store
DISK_ERROR_MESSAGE_FLAG: equ 0xbe78

;Standard CPC BIOS call locations
RESET_ENTRY_RST_0:	equ 0x0000
LOW_JUMP_RST_1:	equ 0x0008
KL_LOW_PCHL:	equ 0x000b
PCBC_INSTRUCTION:	equ 0x000e
SIDE_CALL_RST_2:	equ 0x0010
KL_FAR_PCHL:	equ 0x001b
RAM_LAM:	equ 0x0020
KL_ROM_SELECT:	equ 0xb90f
KL_CURR_SELECTION:	equ 0xb912
KL_PROBE_ROM:	equ 0xb915
KL_ROM_DESELECT:	equ 0xb918
KL_LDIR:	equ 0xb91b
KM_WAIT_CHAR:	equ 0xbb06
KM_READ_CHAR:	equ 0xbb09
KM_SET_EXPAND:	equ 0xbb0f
KM_WAIT_KEY:	equ 0xbb18
KM_TEST_KEY:	equ 0xbb1e
KM_SET_SHIFT:	equ 0xbb2d
KM_BREAK_EVENT:	equ 0xbb4b
TXT_OUTPUT:	equ 0xbb5a
TXT_RD_CHAR:	equ 0xbb60
TXT_GET_WINDOW:	equ 0xbb69
TXT_SET_CURSOR:	equ 0xbb75
TXT_GET_CURSOR:	equ 0xbb78
SCR_GET_MODE:	equ 0xbc11
CAS_NOISY:	equ 0xbc6b
CAS_START_MOTOR:	equ 0xbc6e
CAS_STOP_MOTOR:	equ 0xbc71
CAS_IN_OPEN:	equ 0xbc77
CAS_IN_CLOSE:	equ 0xbc7a
CAS_IN_ABANDON:	equ 0xbc7d
CAS_IN_CHAR:	equ 0xbc80
CAS_IN_DIRECT:	equ 0xbc83
CAS_OUT_OPEN:	equ 0xbc8c
CAS_OUT_CLOSE:	equ 0xbc8f
CAS_OUT_ABANDON:	equ 0xbc92
CAS_OUT_CHAR:	equ 0xbc95
CAS_OUT_DIRECT:	equ 0xbc98
CAS_CATALOG:	equ 0xbc9b
KL_CHOKE_OFF:	equ 0xbcc8
KL_ROM_WALK:	equ 0xbccb
KL_INIT_BACK:	equ 0xbcce
KL_LOG_EXT:	equ 0xbcd1
KL_FIND_COMMAND:	equ 0xbcd4
KL_ADD_FRAME_FLY:	equ 0xbcda
KL_DEL_TICKER:	equ 0xbcec
KL_INIT_EVENT:	equ 0xbcef
MC_START_PROGRAM:	equ 0xbd16
MC_PRINT_CHAR:	equ 0xbd2b
MC_BUSY_PRINTER:	equ 0xbd2e
MC_SEND_PRINTER:	equ 0xbd31
JUMP_RESTORE:	equ 0xbd37
RSX_MKDIR:	equ 0xdf64

;Workspace offset definitions
;This typically shows up in code as something like "ld (iy+WS_CURRENT_DRIVE_LETTER),a" or "ld a,(iy+WS_CURRENT_DRIVE_LETTER)"
;In this case 003h is actually "Current Drive Letter" so I'm creating some EQUs for readability
WS_CURRENT_DRIVE_LETTER: equ 003h ;Current Drive Letter
WS_DRIVE_NUMBER: 				 equ 004h ;Current Drive Number (mostly used when patching (See Appendix F))
; BLOCK 'ROM_TYPE' (start 0xc000 end 0xc001)
ROM_TYPE:
; The ROM type can be one of the following values:
; a. External Foreground &00
; b. Background &01
; c. Extension Foreground &02
; d. Internal (i.e. BASIC) &80

	;Define this as a background ROM
	defb 001h		;c000	01 	.

; BLOCK 'ROM_VERSION' (start 0xc001 end 0xc004)
ROM_VERSION:
;Aka v2.19
	defb 002h		;c001	02 	.
	defb 001h		;c002	01 	.
	defb 009h		;c003	09 	.


; BLOCK 'COMMAND_TABLE' (start 0xc004 end 0xc006)
COMMAND_TABLE:
	;This is a vector to the address of where the RSX names are defined.
	;defw 0c0c0h		;c004	c0 c0 	. .
	defw RSX_COMMANDS_start
	;This is the location of the commands

RSX_JUMPS:
;all the jumps - must be in the same order as the command names.
	jp ROM_INIT		;c006	c3 cb c1 	. . .
	jp RSX_CLS		;c009	c3 42 d4 	. B .
	jp RSX_DISK		;c00c	c3 26 de 	. & .
	jp RSX_DISK_IN		;c00f	c3 29 de 	. ) .
	jp RSX_DISK_OUT		;c012	c3 47 de 	. G .
	jp RSX_DISK		;c015	c3 26 de 	. & .
	jp RSX_DISK_IN		;c018	c3 29 de 	. ) .
	jp RSX_DISK_OUT		;c01b	c3 47 de 	. G .
	jp RSX_A		;c01e	c3 bf d7 	. . .
	jp RSX_B		;c021	c3 c3 d7 	. . .
	jp RSX_DRIVE		;c024	c3 cf d7 	. . .
	jp RSX_USER		;c027	c3 4d d5 	. M .
	jp RSX_DIR		;c02a	c3 ce d4 	. . .
	jp RSX_ERA		;c02d	c3 59 d6 	. Y .
	jp RSX_REN		;c030	c3 8f d5 	. . .
	jp RSX_FORMAT		;c033	c3 61 db 	. a .
	jp RSX_MKDIR		;c036	c3 64 df 	. d .
	jp RSX_CD		;c039	c3 df df 	. . .
	jp RSX_CAT		;c03c	c3 8f cf 	. . .
	jp RSX_TITLE		;c03f	c3 36 d3 	. 6 .
	jp RSX_RANDOM		;c042	c3 89 cb 	. . .
	jp RSX_POINT		;c045	c3 d6 cb 	. . .
	jp CAS_START_MOTOR		;c048	c3 6e bc 	. n .
	jp CAS_STOP_MOTOR		;c04b	c3 71 bc 	. q .
	jp RSX_OPT		;c04e	c3 74 d3 	. t .
	jp RSX_BPUT		;c051	c3 80 cf 	. . .
	jp RSX_BGET		;c054	c3 51 cf 	. Q .
	jp RSX_FS		;c057	c3 9b d0 	. . .
	jp RSX_SAVE		;c05a	c3 98 f9 	. . .
	jp RSX_LOAD		;c05d	c3 51 f9 	. Q .
	jp RSX_EXEC		;c060	c3 f7 cc 	. . .
	jp RSX_READSECT		;c063	c3 49 d4 	. I .
	jp RSX_WRITESECT		;c066	c3 5d d4 	. ] .
	jp RSX_LINK		;c069	c3 6c e0 	. l .
	jp RSX_RMDIR		;c06c	c3 d2 e0 	. . .
	jp RSX_RMDIR		;c06f	c3 d2 e0 	. . .
	jp RSX_ERA		;c072	c3 59 d6 	. Y .
	jp RSX_EB		;c075	c3 49 d8 	. I .
	jp RSX_LS		;c03c	c3 ce cc 	. . .
	jp RSX_MKDIR		;c07b	c3 64 df 	. d .
	jp RSX_C		;c07e	c3 c7 d7 	. . .
	jp RSX_INFO		;c081	c3 5d d1 	. ] .
	jp RSX_LIST		;c084	c3 27 d2 	. ' .
	jp RSX_DUMP		;c087	c3 4d d2 	. M .
	jp RSX_ZAP		;c08a	c3 11 f4 	. . .
	jp RSX_ROMS		;c08d	c3 d4 f3 	. . .
	jp RSX_CLI		;c090	c3 fc f1 	. . .
	jp RSX_TDUMP		;c093	c3 92 dd 	. . .
	jp RSX_SPOOL		;c096	c3 20 df 	.   .
	jp RSX_PRINT		;c099	c3 40 df 	. @ .
	jp RSX_PRBUFF		;c09c	c3 ce f5 	. . .
	jp RSX_ALIAS		;c09f	c3 76 c6 	. v .
	jp RSX_ASKRAM		;c0a2	c3 25 f9 	. % .
	jp RSX_POKE		;c0a5	c3 f9 f9 	. . .
	jp RSX_PEEK		;c0a8	c3 04 fa 	. . .
	jp RSX_HELP		;c0ab	c3 f5 c4 	. . .
	jp RSX_CLI		;c0ae	c3 fc f1 	. . .
	jp RSX_ACCESS		;c0b1	c3 36 cd 	. 6 .
	jp RSX_COPY		;c0b4	c3 bd cd 	. . .
  ;See page 27 of the RODOS Manul for more details about how to use these.
	jp RSX_HIDDEN_04		;c0b7	c3 cb c7 	. . .
	jp RSX_HIDDEN_05		;c0ba	c3 aa c7 	. . .
	jp RSX_HIDDEN_06		;c0bd	c3 06 c9 	. . .
RSX_COMMANDS:

; BLOCK 'RSX_COMMANDS' (start 0xc0c0 end 0xc1ca)
RSX_COMMANDS_start:
	;Each RSX name must end with the last letter + 0x80h
	;Therefore |CLS is defined here as defb 'CL','S'+0x80 and due to how this
	;was disassembled becomes defb "CL",0d3h

	defb "RODOS RO"
lc0c8h: ;I'm not sure why this label is pointed at the last byte of RODOS ROM, unless its the start of the command table-1
	defb 0cdh ;RODOS ROM
	;RSX Command table definitions (Command name, with last letter+128)
	defb "CL", 'S' + 0x80 ;0d3h ;CLS
	defb "DIS", 'C' + 0x80 ; DISC
	defb "DISC.I", 'N' + 0x80 ;0xce ; DISC.IN
	defb 'DISC.OU', 'T' + 0x80 ;0xd4 ; DISC.OUT
	defb 'DIS', 'K' + 0x80 ;0xcb ; DISK
	defb 'DISK.I', 'N' + 0x80 ;0xce ; DISK.IN
	defb 'DISK.OU', 'T' + 0x80 ;0xd4 ; DISK.OUT
	defb  0xc1 ; A - Z80asm has a bug where 'A' + 0x80 won't work
	defb  0xc2 ; B
	defb 'DRIV', 'E' + 0x80 ;0xc5 ; DRIVE
	defb 'USE', 'R' + 0x80 ; 0xd2 ; USER
	defb 'DI', 'R' + 0x80 ;0xd2 ; DIR
	defb 'ER', 'A' + 0x80; 0xc1 ; ERA
	defb 'RE', 'N' + 0x80 ;0xce ; REN
	defb 'FORMA', 'T' + 0x80 ;0xd4 ; FORMAT
	defb 'MKDI', 'R' + 0x80 ;0xd2 ; MKDIR
	defb 'C', 'D' + 0x80 ;0xc4 ; CD
	defb 'CA', 'T' + 0x80 ;0xd4 ; CAT
	defb 'TITL', 'E' + 0x80 ;0xc5 ; TITLE
	defb 'RANDO', 'M' + 0x80 ;0xcd ; RANDOM
	defb 'POIN', 'T' + 0x80 ;0xd4 ; POINT
	defb 'MOTOR.O', 'N' + 0x80 ;0xce ; MOTOR.ON
	defb 'MOTOR.OF', 'F'  + 0x80 ;0xc6 ; MOTOR.OFF
	defb 'OP', 'T' + 0x80 ;0xd4 ; OPT
	defb 'BPU', 'T' + 0x80 ;0xd4 ; BPUT
	defb 'BGE', 'T' + 0x80 ;0xd4 ; BGET
	defb 'F', 'S' + 0x80 ;0xd3 ; FS
	defb 'SAV', 'E' + 0x80 ;0xc5 ; SAVE
	defb 'LOA', 'D' + 0x80 ;0xc4 ; LOAD
	defb 'EXE', 'C' + 0x80 ;0xc3 ; EXEC
	defb 'READSEC', 'T' + 0x80 ;0xd4 ; READSECT
	defb 'WRITESEC', 'T' + 0x80 ;0xd4 ; WRITESECT
	defb 'LIN', 'K' + 0x80 ;0xcb ; LINK
	defb 'RMDI', 'R' + 0x80 ;0xd2 ; RMDIR
	defb 'ERADI', 'R' + 0x80 ;0xd2 ; ERADIR
	defb 'R', 'M' + 0x80 ;0xcd ; RM
	defb 'E', 'B' + 0x80 ;,0xc2 ; EB
	defb 'L', 'S' + 0x80 ;,0xd3 ; LS
	defb 'M', 'D' + 0x80 ;0xc4 ; MD
	defb 0xc3 ; C
	defb 'INF', 'O' + 0x80 ;0xcf ; INFO
	defb 'LIS', 'T' + 0x80 ;0xd4 ; LIST
	defb 'DUM', 'P' + 0x80 ;0xd0 ; DUMP
	defb 'ZA', 'P' + 0x80 ;0xd0 ; ZAP
	defb 'ROM', 'S' + 0x80 ;0xd3 ; ROMS
	defb 'CL', 'I' + 0x80 ;0xc9 ; CLI
	defb 'TDUM', 'P' + 0x80 ;0xd0 ; TDUMP
	defb 'SPOO', 'L' + 0x80 ;0xcc ; SPOOL
	defb 'PRIN', 'T' + 0x80 ;0xd4 ; PRINT
	defb 'PRBUF', 'F' + 0x80 ;0xc6 ; PRBUFF
	defb 'ALIA', 'S' + 0x80 ;0xd3 ; ALIAS
	defb 'ASKRA', 'M' + 0x80 ;0xcd ; ASKRAM
	defb 'POK', 'E' + 0x80 ;0xc5 ; POKE
	defb 'PEE', 'K' + 0x80 ;0xcb ; PEEK
	defb 'HEL', 'P' + 0x80 ;0xd0 ; HELP
	defb 'D', 'O' + 0x80 ;0xcf ; DO
	defb 'ACCES', 'S' + 0x80 ;0xd3 ; ACCESS
	defb 'COP', 'Y' + 0x80 ;0xd9 ; COPY
	;See page 27 of the RODOS Manul for more details about how to use these next commands
	defb 084h		;Hidden command 4 aka ^D
	defb 085h		;Hidden command 5 aka ^E
	defb 086h		;Hidden command 6 aka ^F
RSX_COMMANDS_end:
  ;The end of RSX definitions must end with zero.
	defb 0
;=======================================================================
ROM_INIT:
;=======================================================================
;This is the start of the ROM initialise routines.

	call sub_c1f9h		;c1cb	cd f9 c1 	. . .
	call KL_CURR_SELECTION		;c1ce	cd 12 b9 	. . .

	; 006   &B912   KL CURR SELECTION
	;       Action: Gets the ROM select address of the current ROM
	;       Entry:  No entry conditions
	;       Exit:   A contains the ROM select  address  of the current ROM,
	;               and all other registers are preserved

	push af			;c1d1	f5 	.
	push hl			;c1d2	e5 	.
	ld a,03dh		;c1d3	3e 3d 	> =
  ;The 03dh here is the D key. Pressing D disables the ROM on boot.
	call CHECK_FOR_KEY_PRESSED		;c1d5	cd 59 c2 	. Y .
  ;That is the keyboard test thing. Looks like it bypassed the usual rom call.
	pop hl			;c1d8	e1 	.
	jr nz,PRINT_RODOS_OFF		;c1d9	20 59 	  Y
lc1dbh:
	pop af			;c1db	f1 	.
	push de			;c1dc	d5 	.
	push bc			;c1dd	c5 	.
	push ix		;c1de	dd e5 	. .
	ld de,00aech		;c1e0	11 ec 0a 	. . .
	and a			;c1e3	a7 	.
	sbc hl,de		;c1e4	ed 52 	. R
	push hl			;c1e6	e5 	.
	inc hl			;c1e7	23 	#
	push hl			;c1e8	e5 	.
	pop iy		;c1e9	fd e1 	. .
	ld (iy+000h),a		;c1eb	fd 77 00 	. w .
	call sub_c2cch		;c1ee	cd cc c2 	. . .
	pop hl			;c1f1	e1 	.
	pop ix		;c1f2	dd e1 	. .
	pop bc			;c1f4	c1 	.
	pop de			;c1f5	d1 	.
	jp lda0ah		;c1f6	c3 0a da 	. . .
sub_c1f9h:
	push hl			;c1f9	e5 	.
	push bc			;c1fa	c5 	.
	push de			;c1fb	d5 	.
	call sub_RELOCATE_ROM_SELECT_DESELECT		;c1fc	cd 5e d9 	. ^ .
	call KL_CURR_SELECTION		;c1ff	cd 12 b9 	. . .
	; 006   &B912   KL CURR SELECTION
	;       Action: Gets the ROM select address of the current ROM
	;       Entry:  No entry conditions
	;       Exit:   A contains the ROM select  address  of the current ROM,
	;               and all other registers are preserved

	xor 008h		;c202	ee 08 	. .
	ld c,a			;c204	4f 	O
	ld hl,RSX_COMMANDS		;c205	21 c0 c0 	! . .
	call ROM_SELECT_DESELECT_RELOCATED		;c208	cd c0 be 	. . .
	cp 052h		;c20b	fe 52 	. R
	jr nz,lc21dh		;c20d	20 0e 	  .
	ld hl,lc0c8h		;c20f	21 c8 c0 	! . .
	call ROM_SELECT_DESELECT_RELOCATED		;c212	cd c0 be 	. . .
	cp 0cdh		;c215	fe cd 	. .
	jr nz,lc21dh		;c217	20 04 	  .
	pop de			;c219	d1 	.
	pop bc			;c21a	c1 	.
	pop hl			;c21b	e1 	.
	ret			;c21c	c9 	.
lc21dh:
	pop de			;c21d	d1 	.
	call DETERMINE_BASIC_VERSION		;c21e	cd d8 f4 	. . .
	pop bc			;c221	c1 	.
	pop hl			;c222	e1 	.
	ret nz			;c223	c0 	.
	push bc			;c224	c5 	.
	ld c,00eh		;c225	0e 0e 	. .
lc227h:
	push bc			;c227	c5 	.
	call KL_INIT_BACK		;c228	cd ce bc 	. . .
	pop bc			;c22b	c1 	.
	dec c			;c22c	0d 	.
	ld a,c			;c22d	79 	y
	cp 007h		;c22e	fe 07 	. .
	jr nz,lc227h		;c230	20 f5 	  .
	pop bc			;c232	c1 	.
	ret			;c233	c9 	.
PRINT_RODOS_OFF:
	push hl			;c234	e5 	.
	push bc			;c235	c5 	.
	call DETERMINE_BASIC_VERSION		;c236	cd d8 f4 	. . .
	pop bc			;c239	c1 	.
	ld a,h			;c23a	7c 	|
	pop hl			;c23b	e1 	.
	and a			;c23c	a7 	.
	jr z,lc1dbh		;c23d	28 9c 	( .
	push hl			;c23f	e5 	.
	ld hl,RODOS_OFF_MSG		;c240	21 4b c2 	! K .
	call DISPLAY_MSG		;c243	cd 6a d9 	. j .
	pop hl			;c246	e1 	.
	pop af			;c247	f1 	.
	jp lda0fh		;c248	c3 0f da 	. . .
RODOS_OFF_MSG:
	defb '* RODOS OFF *',05ch

CHECK_FOR_KEY_PRESSED:
;Entry: A=Amstrad hardware key number
;I'm assuming that this replicates the KM_READ_CHAR bios call
;
; {
;   undefined2 in_AF;
;   byte bVar1;
;
;   FUN_ram_c290();
;   bVar1 = (byte)((ushort)in_AF >> 8);
;   DAT_ram_bf06 = (bVar1 & 7) * '\b' + 'F';
;   SUB_ram_bf05 = 0xcb;
;   DAT_ram_bf07 = 0xc9;
;   func_0xbf05(&DAT_ram_bee0 + ((char)bVar1 >> 3 & 0xf),param_1);
;   return;
; }

	push af			;c259	f5 	.
	call sub_c290h		;c25a	cd 90 c2 	. . .
	pop af			;c25d	f1 	.
	ld hl,0bee0h		;c25e	21 e0 be 	! . .
	ld c,a			;c261	4f 	O
	sra a		;c262	cb 2f 	. /
	sra a		;c264	cb 2f 	. /
	sra a		;c266	cb 2f 	. /
	and 00fh		;c268	e6 0f 	. .
	push de			;c26a	d5 	.
	ld e,a			;c26b	5f 	_
	ld d,000h		;c26c	16 00 	. .
	add hl,de			;c26e	19 	.
	pop de			;c26f	d1 	.
	ld a,c			;c270	79 	y
	sla a		;c271	cb 27 	. '
	sla a		;c273	cb 27 	. '
	sla a		;c275	cb 27 	. '
	and 038h		;c277	e6 38 	. 8
	add a,046h		;c279	c6 46 	. F
;OK.... this next bit is stowing code
;but its wierd. it builds bf05 as:
;bf05: &CB,A calculated above,&C9
;CB is a bit operation.
;C9 - ret
	ld (0bf06h),a		;c27b	32 06 bf 	2 . .
	ld a,0cbh		;c27e	3e cb 	> .
	ld (0bf05h),a		;c280	32 05 bf 	2 . .
	ld a,0c9h		;c283	3e c9 	> .
	ld (0bf07h),a		;c285	32 07 bf 	2 . .
	call 0bf05h		;c288	cd 05 bf 	. . .
	jp z,lda0ah		;c28b	ca 0a da 	. . .
	xor a			;c28e	af 	.
	ret			;c28f	c9 	.
sub_c290h:
	ld hl,0bee0h		;c290	21 e0 be 	! . .
	di			;c293	f3 	.
	push bc			;c294	c5 	.
	ld bc,0f40eh		;c295	01 0e f4 	. . .
	out (c),c		;c298	ed 49 	. I
	ld b,0f6h		;c29a	06 f6 	. .
	in a,(c)		;c29c	ed 78 	. x
	and 030h		;c29e	e6 30 	. 0
	ld c,a			;c2a0	4f 	O
	or 0c0h		;c2a1	f6 c0 	. .
	out (c),a		;c2a3	ed 79 	. y
	out (c),c		;c2a5	ed 49 	. I
	inc b			;c2a7	04 	.
	ld a,092h		;c2a8	3e 92 	> .
	out (c),a		;c2aa	ed 79 	. y
	push bc			;c2ac	c5 	.
	set 6,c		;c2ad	cb f1 	. .
lc2afh:
	ld b,0f6h		;c2af	06 f6 	. .
	out (c),c		;c2b1	ed 49 	. I
	ld b,0f4h		;c2b3	06 f4 	. .
	in a,(c)		;c2b5	ed 78 	. x
	ld (hl),a			;c2b7	77 	w
	inc hl			;c2b8	23 	#
	inc c			;c2b9	0c 	.
	ld a,c			;c2ba	79 	y
	and 00fh		;c2bb	e6 0f 	. .
	cp 00ah		;c2bd	fe 0a 	. .
	jr nz,lc2afh		;c2bf	20 ee 	  .
	pop bc			;c2c1	c1 	.
	ld a,082h		;c2c2	3e 82 	> .
	out (c),a		;c2c4	ed 79 	. y
	dec b			;c2c6	05 	.
	out (c),c		;c2c7	ed 49 	. I
	pop bc			;c2c9	c1 	.
	ei			;c2ca	fb 	.
	ret			;c2cb	c9 	.
sub_c2cch:
	call sub_fb0bh		;c2cc	cd 0b fb 	. . .
	ld hl,VERSION_MSG		;c2cf	21 95 ff 	! . .
	call DISPLAY_MSG		;c2d2	cd 6a d9 	. j .
	call sub_c3c3h		;c2d5	cd c3 c3 	. . .
	call CHECK_FOR_CPM_ROM		;c2d8	cd cc c4 	. . .
	call SETUP_ENTER_KEY_STRINGS		;c2db	cd 91 c4 	. . .
	call INITIALISE_VARIABLES		;c2de	cd 29 c3 	. ) .
	ld a,(0be09h)		;c2e1	3a 09 be 	: . .
	cp 063h		;c2e4	fe 63 	. c
	call nz,RESET_INTERNAL_VARIABLES_TO_DEFAULT		;c2e6	c4 99 c3 	. . .

	;Now check for R being pressed
	ld a,032h		;c2e9	3e 32 	> 2
	call CHECK_FOR_KEY_PRESSED		;c2eb	cd 59 c2 	. Y .
	;If R was pressed then initialise everything back to defaults
	call nz,RESET_INTERNAL_VARIABLES_TO_DEFAULT		;c2ee	c4 99 c3 	. . .


	call sub_cb6dh		;c2f1	cd 6d cb 	. m .
	call sub_c636h		;c2f4	cd 36 c6 	. 6 .

	;Check for SHIFT key
	ld a,015h		;c2f7	3e 15 	> .
	call KM_TEST_KEY		;c2f9	cd 1e bb 	. . .
	ld (iy+046h),l		;c2fc	fd 75 46 	. u F
	ld (iy+047h),h		;c2ff	fd 74 47 	. t G
	xor a			;c302	af 	.
	ld (iy+011h),a		;c303	fd 77 11 	. w .
	dec a			;c306	3d 	=
	ld (iy+00ch),a		;c307	fd 77 0c 	. w .
	ld a,001h		;c30a	3e 01 	> .
	ld (iy+041h),a		;c30c	fd 77 41 	. w A
	call sub_f541h		;c30f	cd 41 f5 	. A .
	ld a,004h		;c312	3e 04 	> .
	ld (iy+00dh),a		;c314	fd 77 0d 	. w .
	;Ok, so this next bit grabs the code from call TXT_OUTPUT and stores it
	;at iy+5a to iy+5c
	ld hl,(0bb5bh)		;c317	2a 5b bb 	* [ .
	ld a,(TXT_OUTPUT)		;c31a	3a 5a bb 	: Z .
	ld (iy+05ah),a		;c31d	fd 77 5a 	. w Z
	ld (iy+05bh),l		;c320	fd 75 5b 	. u [
	ld (iy+05ch),h		;c323	fd 74 5c 	. t \
	jp ldf4fh		;c326	c3 4f df 	. O .
INITIALISE_VARIABLES:
	xor a			;c329	af 	.
	;A=0
	;Now set the working space variables to initial values
	;TODO find out what these actually relate to
	ld (iy+013h),a		;c32a	fd 77 13 	. w .
	ld (iy+036h),a		;c32d	fd 77 36 	. w 6
	ld (iy+008h),a		;c330	fd 77 08 	. w .
	ld (iy+015h),a		;c333	fd 77 15 	. w .
	ld (iy+016h),a		;c336	fd 77 16 	. w .
	ld (iy+03bh),a		;c339	fd 77 3b 	. w ;
	ld (iy+04eh),a		;c33c	fd 77 4e 	. w N
	ld (iy+042h),a		;Home drive number for |CD   ;c33f	fd 77 42 	. w B
	ld (iy+043h),a		;Home drive letter for |CD   ;c342	fd 77 43 	. w C
	ld (iy+044h),a		;Home track for |CD          ;c345	fd 77 44 	. w D
	ld (DISK_ERROR_MESSAGE_FLAG),a		;c348	32 78 be 	2 x .
	dec a			;c34b	3d 	=
	ld (iy+04fh),a		;c34c	fd 77 4f 	. w O
	ld (iy+03fh),a		;c34f	fd 77 3f 	. w ?
	ld (iy+040h),a		;c352	fd 77 40 	. w @
	ld hl,0faffh		;c355	21 ff fa 	! . .
	ld (iy+058h),l		;c358	fd 75 58 	. u X
	ld (iy+059h),h		;c35b	fd 74 59 	. t Y
	call sub_f174h		;c35e	cd 74 f1 	. t .
	ld a,081h		;c361	3e 81 	> .
	ld (iy+045h),a		;Home sector for |CD         ;c363	fd 77 45 	. w E
	ld a,010h		;c366	3e 10 	> .
	ld (iy+009h),a		;c368	fd 77 09 	. w .

	ld de,00021h		;c36b	11 21 00 	. ! .
	push iy		;c36e	fd e5 	. .
	pop hl			;c370	e1 	.
	;de=021h
	;hl=iy (iy is the working space for the ROM)

	add hl,de			;c371	19 	.
	;hl=hl+021h, aka iy+021h
	ld b,009h		;c372	06 09 	. .
lc374h:
	ld (hl),000h		;c374	36 00 	6 .
	inc hl			;c376	23 	#
	djnz lc374h		;c377	10 fb 	. .
	;So here we set iy+0x21h to iy+0x2a to zero

	ld de,0002ah		;c379	11 2a 00 	. * .
	push iy		;c37c	fd e5 	. .
	pop hl			;c37e	e1 	.
	add hl,de			;c37f	19 	.
	;Same as above - hl=(iy+0x2ah)

	ld b,009h		;c380	06 09 	. .
lc382h:
	ld (hl),081h		;c382	36 81 	6 .
	inc hl			;c384	23 	#
	djnz lc382h		;c385	10 fb 	. .
  ;sets iy+0x2a to iy+0x33 to 0x81h

	call RSX_DISK		;c387	cd 26 de 	. & .
	push iy		;c38a	fd e5 	. .
	pop hl			;c38c	e1 	.
	ld de,00080h		;c38d	11 80 00 	. . .
	add hl,de			;c390	19 	.
	;hl=(iy+0x80h)

	ld b,004h		;c391	06 04 	. .
lc393h:
	ld (hl),0ffh		;c393	36 ff 	6 .
	inc hl			;c395	23 	#
	djnz lc393h		;c396	10 fb 	. .
	;(iy+080h to 084h gets set to 0ffh)

	ret			;c398	c9 	.
RESET_INTERNAL_VARIABLES_TO_DEFAULT:
; reset all internal settings which are normally preserved through a reset.
;
; as Ghidra defines it:
;
; void FUN_ram_c399(void)
;
; {
;   short sVar1;
;   undefined *puVar2;
;   undefined1 *puVar3;
;
;   puVar3 = &DAT_ram_c3b2;
;   puVar2 = &DAT_ram_be00;
;   sVar1 = 10;
;   do {
;     *puVar2 = *puVar3;
;     puVar2 = puVar2 + 1;
;     puVar3 = puVar3 + 1;
;     sVar1 = sVar1 + -1;
;   } while (sVar1 != 0);
;   puVar2 = &DAT_ram_be44;
;   puVar3 = &DAT_ram_c3bc;
;   sVar1 = 7;
;   do {
;     *puVar2 = *puVar3;
;     puVar2 = puVar2 + 1;
;     puVar3 = puVar3 + 1;
;     sVar1 = sVar1 + -1;
;   } while (sVar1 != 0);
;   FUN_ram_fb88();
;   return;
; }
;
; So.. copy 10 data from c3b2 to be00
; then copy 7 data from c3bc to 0be44
;
	ld hl,lc3b2h		;c399	21 b2 c3 	! . .
	ld de,0be00h		;c39c	11 00 be 	. . .
	ld bc,0000ah		;c39f	01 0a 00 	. . .
	ldir		;c3a2	ed b0 	. .
	;Repeats LDI (LD (DE),(HL), then increments DE, HL, and decrements BC) until BC=0.
	;Note that if BC=0 before this instruction is called, it will loop around until BC=0 again.
	ld de,0be44h		;c3a4	11 44 be 	. D .
	ld hl,lc3bch		;c3a7	21 bc c3 	! . .
	ld bc,00007h		;c3aa	01 07 00 	. . .
	ldir		;c3ad	ed b0 	. .
	jp MAKE_A_BEEP		;c3af	c3 88 fb 	. . .
lc3b2h:
;Initial values for be00 to be0a
;TODO what do these relate to?
  db         00h
  db         09h
  db         08h
  db         0Ah
  db         0Bh
  db         0FFh
  db         0FFh
  db         0DCh
  db         0FFh
  db         063h

lc3bch:
;Initial values for be44 to be4a
;TODO what do these relate to?
  db         032h
  db         00h
  db         064h
  db         01h
  db         0AFh
  db         0Fh
  db         0Ch

sub_c3c3h:
	push iy		;c3c3	fd e5 	. .
	pop hl			;c3c5	e1 	.
	ld de,00033h		;c3c6	11 33 00 	. 3 .
	add hl,de			;c3c9	19 	.
	ld de,KL_FIND_COMMAND		;c3ca	11 d4 bc 	. . .
	ex de,hl			;c3cd	eb 	.
	ld bc,00003h		;c3ce	01 03 00 	. . .
	ldir		;c3d1	ed b0 	. .
	ld ix,lc3e6h		;c3d3	dd 21 e6 c3 	. ! . .
	ld de,0006bh		;c3d7	11 6b 00 	. k .
	push iy		;c3da	fd e5 	. .
	pop hl			;c3dc	e1 	.
	add hl,de			;c3dd	19 	.
	ld de,KL_FIND_COMMAND		;c3de	11 d4 bc 	. . .
	ld b,001h		;c3e1	06 01 	. .
	jp MAKE_JP_AT_DE_USING_HL		;c3e3	c3 74 de 	. t .
lc3e6h:
	;db 0e8h,0c3h
	dw EXECUTE_RSX_COMMAND ;Vector to the sub below. Used in sub_c3c3h
EXECUTE_RSX_COMMAND:
;This is a z80dasm fixup - theres a call to mid z80dasm instruction so it misses a label.
	RES        0x2,(IY+0xd) ;ram:c3e8 fd cb 0d 96
	; z80dasm started at the db 0c3h part above, and missed that theres a call into here.
	; Old instructions were:
	; jp lcbfch+1		;c3e7	c3 fd cb 	. . .
	; dec c			;c3ea	0d 	.
	; sub (hl)			;c3eb	96		.
	ld a,(hl)			;c3ec	7e 	~
	cp 03ah		;c3ed	fe 3a 	. :
	jp nc,lc47dh		;c3ef	d2 7d c4 	. } .
	sub 030h		;c3f2	d6 30 	. 0
	jp c,lc47dh		;c3f4	da 7d c4 	. } .
	push ix		;c3f7	dd e5 	. .
	ld c,a			;c3f9	4f 	O
	inc hl			;c3fa	23 	#
	push hl			;c3fb	e5 	.
	push bc			;c3fc	c5 	.
	call sub_RELOCATE_ROM_SELECT_DESELECT		;c3fd	cd 5e d9 	. ^ .
	pop bc			;c400	c1 	.
	ld hl,COMMAND_TABLE		;c401	21 04 c0 	! . .
	call ROM_SELECT_DESELECT_RELOCATED		;c404	cd c0 be 	. . .
	ld e,a			;c407	5f 	_
	inc hl			;c408	23 	#
	call ROM_SELECT_DESELECT_RELOCATED		;c409	cd c0 be 	. . .
	ld h,a			;c40c	67 	g
	ld l,e			;c40d	6b 	k
	push hl			;c40e	e5 	.
	ld hl,ROM_TYPE		;c40f	21 00 c0 	! . .
	call ROM_SELECT_DESELECT_RELOCATED		;c412	cd c0 be 	. . .
	pop hl			;c415	e1 	.
	cp 001h		;c416	fe 01 	. .
	jr nz,lc433h		;c418	20 19 	  .
	ld de,0beb0h		;c41a	11 b0 be 	. . .
	push hl			;c41d	e5 	.
lc41eh:
	call ROM_SELECT_DESELECT_RELOCATED		;c41e	cd c0 be 	. . .
	ld (de),a			;c421	12 	.
	inc hl			;c422	23 	#
	inc de			;c423	13 	.
	bit 7,a		;c424	cb 7f 	. 
	jr z,lc41eh		;c426	28 f6 	( .
	ld hl,0beb0h		;c428	21 b0 be 	! . .
	push bc			;c42b	c5 	.
	call lc47dh		;c42c	cd 7d c4 	. } .
	pop bc			;c42f	c1 	.
	pop hl			;c430	e1 	.
	jr nc,lc460h		;c431	30 2d 	0 -
lc433h:
	ld de,RSX_JUMPS		;c433	11 06 c0 	. . .
	pop ix		;c436	dd e1 	. .
lc438h:
	push ix		;c438	dd e5 	. .
	cp 0ffh		;c43a	fe ff 	. .
	jr z,lc460h		;c43c	28 22 	( "
lc43eh:
	call ROM_SELECT_DESELECT_RELOCATED		;c43e	cd c0 be 	. . .
	cp (ix+000h)		;c441	dd be 00 	. . .
	jr nz,lc466h		;c444	20 20
	inc ix		;c446	dd 23 	. #
	inc hl			;c448	23 	#
	bit 7,a		;c449	cb 7f 	. 
	jr z,lc43eh		;c44b	28 f1 	( .
	pop ix		;c44d	dd e1 	. .
	push de			;c44f	d5 	.
	ld hl,ROM_TYPE		;c450	21 00 c0 	! . .
	call ROM_SELECT_DESELECT_RELOCATED		;c453	cd c0 be 	. . .
	pop hl			;c456	e1 	.
	pop ix		;c457	dd e1 	. .
	and 07fh		;c459	e6 7f 	. 
	jp z,MC_START_PROGRAM		;c45b	ca 16 bd 	. . .
	scf			;c45e	37 	7
	ret			;c45f	c9 	.
lc460h:
	pop ix		;c460	dd e1 	. .
	pop ix		;c462	dd e1 	. .
	xor a			;c464	af 	.
	ret			;c465	c9 	.
lc466h:
	pop ix		;c466	dd e1 	. .
lc468h:
	call ROM_SELECT_DESELECT_RELOCATED		;c468	cd c0 be 	. . .
	inc hl			;c46b	23 	#
	bit 7,a		;c46c	cb 7f 	. 
	jr z,lc468h		;c46e	28 f8 	( .
	inc de			;c470	13 	.
	inc de			;c471	13 	.
	inc de			;c472	13 	.
	call ROM_SELECT_DESELECT_RELOCATED		;c473	cd c0 be 	. . .
	and a			;c476	a7 	.
	jr nz,lc438h		;c477	20 bf 	  .
	pop ix		;c479	dd e1 	. .
	xor a			;c47b	af 	.
	ret			;c47c	c9 	.
lc47dh:
	push hl			;c47d	e5 	.
	push iy		;c47e	fd e5 	. .
	pop hl			;c480	e1 	.
	ld de,00033h		;c481	11 33 00 	. 3 .
	add hl,de			;c484	19 	.
	ld (0bf01h),hl		;c485	22 01 bf 	" . .
	ld a,0c3h		;c488	3e c3 	> .
	ld (0bf00h),a		;c48a	32 00 bf 	2 . .
	pop hl			;c48d	e1 	.
	jp 0bf00h		;c48e	c3 00 bf 	. . .
SETUP_ENTER_KEY_STRINGS:
	ld de,0bf00h		;c491	11 00 bf 	. . .
	ld hl,STR_CLI_RUN_DISK_start		;c494	21 be c4 	! . .
	ld bc,PCBC_INSTRUCTION		;c497	01 0e 00 	. . .
	ldir		;c49a	ed b0 	. .
	ld b,08dh		;c49c	06 8d 	. .
	ld a,006h		;c49e	3e 06 	> .
	call KM_SET_SHIFT		;c4a0	cd 2d bb 	. - .
	ld b,0feh		;c4a3	06 fe 	. .
	ld a,046h		;c4a5	3e 46 	> F
	call KM_SET_SHIFT		;c4a7	cd 2d bb 	. - .
	ld b,08ch		;c4aa	06 8c 	. .
	ld c,009h		;c4ac	0e 09 	. .
	ld hl,0bf05h		;c4ae	21 05 bf 	! . .
	call KM_SET_EXPAND		;c4b1	cd 0f bb 	. . .
	ld hl,0bf00h		;c4b4	21 00 bf 	! . .
	ld b,08dh		;c4b7	06 8d 	. .
	ld c,005h		;c4b9	0e 05 	. .
	jp KM_SET_EXPAND		;c4bb	c3 0f bb 	. . .

; BLOCK 'STR_CLI_RUN_DISK' (start 0xc4be end 0xc4cb)
STR_CLI_RUN_DISK_start:
  defb '|CLI',13,'RUN"DISC',13 ; adding a ' to make my editor happy otherwise it thinks the string was left open (it was)
STR_CLI_RUN_DISK_end:
CHECK_FOR_CPM_ROM:
	ld ix,0bee0h		;c4cc	dd 21 e0 be 	. ! . .
	ld (ix+000h),'D'		;c4d0	dd 36 00 44 	. 6 . D
	ld (ix+001h),'I'		;c4d4	dd 36 01 49 	. 6 . I
	ld (ix+002h),'R' + 0x80 ;0d2h		;c4d8	dd 36 02 d2 	. 6 . .
	ld hl,0bee0h		;c4dc	21 e0 be 	! . .
	xor a			;c4df	af 	.
	call lc47dh		;c4e0	cd 7d c4 	. } .
	ld a,c			;c4e3	79 	y
	jr c,lc4efh		;c4e4	38 09 	8 .
	call MSG_CPM_ROM_MISSING		;c4e6	cd ba fb 	. . .
	ld a,03fh		;c4e9	3e 3f 	> ?
	ld (iy+001h),a		;c4eb	fd 77 01 	. w .
	ret			;c4ee	c9 	.
lc4efh:
	add a,030h		;c4ef	c6 30 	. 0
	ld (iy+001h),a		;c4f1	fd 77 01 	. w .
	ret			;c4f4	c9 	.

;=======================================================================
RSX_HELP:
;=======================================================================
	cp 002h		;c4f5	fe 02 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;c4f7	d2 9f fb 	. . .
	and a			;c4fa	a7 	.
	jr z,lc550h		;c4fb	28 53 	( S
	call PRINT_CR_LF		;c4fd	cd 7d d9 	. } .
	call sub_RELOCATE_ROM_SELECT_DESELECT		;c500	cd 5e d9 	. ^ .
	ld c,(ix+000h)		;c503	dd 4e 00 	. N .
	ld hl,ROM_TYPE		;c506	21 00 c0 	! . .
	call ROM_SELECT_DESELECT_RELOCATED		;c509	cd c0 be 	. . .
	and 07fh		;c50c	e6 7f 	. 
	cp 003h		;c50e	fe 03 	. .
	ret nc			;c510	d0 	.
	call sub_c581h		;c511	cd 81 c5 	. . .
	ld hl,COMMAND_TABLE		;c514	21 04 c0 	! . .
	call ROM_SELECT_DESELECT_RELOCATED		;c517	cd c0 be 	. . .
	ld e,a			;c51a	5f 	_
	inc hl			;c51b	23 	#
	call ROM_SELECT_DESELECT_RELOCATED		;c51c	cd c0 be 	. . .
	ld d,a			;c51f	57 	W
	ex de,hl			;c520	eb 	.
lc521h:
	ld d,000h		;c521	16 00 	. .
lc523h:
	call ROM_SELECT_DESELECT_RELOCATED		;c523	cd c0 be 	. . .
	and a			;c526	a7 	.
	jp z,PRINT_CR_LF		;c527	ca 7d d9 	. } .
	ld e,a			;c52a	5f 	_
	and 07fh		;c52b	e6 7f 	. 
	cp 020h		;c52d	fe 20 	.
	jr c,lc53bh		;c52f	38 0a 	8 .
	call TXT_OUTPUT		;c531	cd 5a bb 	. Z .
	call sub_c5ffh		;c534	cd ff c5 	. . .
	jp nz,PRINT_CR_LF		;c537	c2 7d d9 	. } .
	inc d			;c53a	14 	.
lc53bh:
	inc hl			;c53b	23 	#
	bit 7,e		;c53c	cb 7b 	. {
	jr z,lc523h		;c53e	28 e3 	( .
lc540h:
	ld a,d			;c540	7a 	z
	cp 014h		;c541	fe 14 	. .
	jr z,lc54bh		;c543	28 06 	( .
	call sub_c5ebh		;c545	cd eb c5 	. . .
	inc d			;c548	14 	.
	jr lc540h		;c549	18 f5 	. .
lc54bh:
	call NEWLINE_IF_NO_DISPLAY_SPACE		;c54b	cd ab d8 	. . .
	jr lc521h		;c54e	18 d1 	. .
lc550h:
	ld (0bf0fh),a		;c550	32 0f bf 	2 . .
	call PRINT_CR_LF		;c553	cd 7d d9 	. } .
	call sub_RELOCATE_ROM_SELECT_DESELECT		;c556	cd 5e d9 	. ^ .
	ld b,010h		;c559	06 10 	. .
lc55bh:
	ld a,010h		;c55b	3e 10 	> .
	sub b			;c55d	90 	.
	ld c,a			;c55e	4f 	O
	call sub_c578h		;c55f	cd 78 c5 	. x .
	jr z,lc571h		;c562	28 0d 	( .
	ld a,(0bf0fh)		;c564	3a 0f bf 	: . .
	and a			;c567	a7 	.
	jr nz,lc571h		;c568	20 07 	  .
	inc a			;c56a	3c 	<
	ld (0bf0fh),a		;c56b	32 0f bf 	2 . .
	call sub_c581h		;c56e	cd 81 c5 	. . .
lc571h:
	call sub_c5ffh		;c571	cd ff c5 	. . .
	ret nz			;c574	c0 	.
	djnz lc55bh		;c575	10 e4 	. .
	ret			;c577	c9 	.
sub_c578h:
	ld hl,ROM_TYPE		;c578	21 00 c0 	! . .
	call ROM_SELECT_DESELECT_RELOCATED		;c57b	cd c0 be 	. . .
	cp 003h		;c57e	fe 03 	. .
	ret nc			;c580	d0 	.
sub_c581h:
	ld hl,MSG_ROM		;c581	21 fa c5 	! . .
	push bc			;c584	c5 	.
	call DISPLAY_MSG		;c585	cd 6a d9 	. j .
	pop bc			;c588	c1 	.
	ld a,c			;c589	79 	y
	add a,030h		;c58a	c6 30 	. 0
	cp 03ah		;c58c	fe 3a 	. :
	push af			;c58e	f5 	.
	call c,sub_c5ebh		;c58f	dc eb c5 	. . .
	pop af			;c592	f1 	.
	call nc,sub_c5f0h		;c593	d4 f0 c5 	. . .
	call TXT_OUTPUT		;c596	cd 5a bb 	. Z .
	ld hl,COMMAND_TABLE		;c599	21 04 c0 	! . .
	call ROM_SELECT_DESELECT_RELOCATED		;c59c	cd c0 be 	. . .
	ld e,a			;c59f	5f 	_
	inc hl			;c5a0	23 	#
	call ROM_SELECT_DESELECT_RELOCATED		;c5a1	cd c0 be 	. . .
	ld d,a			;c5a4	57 	W
	ex de,hl			;c5a5	eb 	.
	call sub_c5ebh		;c5a6	cd eb c5 	. . .
	push bc			;c5a9	c5 	.
	ld b,011h		;c5aa	06 11 	. .
lc5ach:
	call ROM_SELECT_DESELECT_RELOCATED		;c5ac	cd c0 be 	. . .
	ld e,a			;c5af	5f 	_
	and 07fh		;c5b0	e6 7f 	. 
	call TXT_OUTPUT		;c5b2	cd 5a bb 	. Z .
	dec b			;c5b5	05 	.
	inc hl			;c5b6	23 	#
	bit 7,e		;c5b7	cb 7b 	. {
	jr z,lc5ach		;c5b9	28 f1 	( .
lc5bbh:
	call sub_c5ebh		;c5bb	cd eb c5 	. . .
	djnz lc5bbh		;c5be	10 fb 	. .
	pop bc			;c5c0	c1 	.
	call sub_c5ebh		;c5c1	cd eb c5 	. . .
	ld hl,ROM_VERSION		;c5c4	21 01 c0 	! . .
	call ROM_SELECT_DESELECT_RELOCATED		;c5c7	cd c0 be 	. . .
	add a,030h		;c5ca	c6 30 	. 0
	call TXT_OUTPUT		;c5cc	cd 5a bb 	. Z .
	ld a,02eh		;c5cf	3e 2e 	> .
	call TXT_OUTPUT		;c5d1	cd 5a bb 	. Z .
	inc hl			;c5d4	23 	#
	call ROM_SELECT_DESELECT_RELOCATED		;c5d5	cd c0 be 	. . .
	add a,030h		;c5d8	c6 30 	. 0
	call TXT_OUTPUT		;c5da	cd 5a bb 	. Z .
	inc hl			;c5dd	23 	#
	call ROM_SELECT_DESELECT_RELOCATED		;c5de	cd c0 be 	. . .
	add a,030h		;c5e1	c6 30 	. 0
	call TXT_OUTPUT		;c5e3	cd 5a bb 	. Z .
	call PRINT_CR_LF		;c5e6	cd 7d d9 	. } .
	xor a			;c5e9	af 	.
	ret			;c5ea	c9 	.
sub_c5ebh:
	ld a,020h		;c5eb	3e 20 	>
	jp TXT_OUTPUT		;c5ed	c3 5a bb 	. Z .
sub_c5f0h:
	push af			;c5f0	f5 	.
	ld a,031h		;c5f1	3e 31 	> 1
	call TXT_OUTPUT		;c5f3	cd 5a bb 	. Z .
	pop af			;c5f6	f1 	.
	sub 00ah		;c5f7	d6 0a 	. .
	ret			;c5f9	c9 	.
MSG_ROM:
	defb 'ROM ',0
sub_c5ffh:
	call sub_c61dh		;c5ff	cd 1d c6 	. . .
	ret z			;c602	c8 	.
lc603h:
	call sub_c61dh		;c603	cd 1d c6 	. . .
	jr nz,lc603h		;c606	20 fb 	  .
lc608h:
	call KM_READ_CHAR		;c608	cd 09 bb 	. . .
	jr c,lc608h		;c60b	38 fb 	8 .
	call KM_WAIT_CHAR		;c60d	cd 06 bb 	. . .
	cp 0fch		;c610	fe fc 	. .
	jp nz,lda14h		;c612	c2 14 da 	. . .
	push hl			;c615	e5 	.
	call KM_BREAK_EVENT		;c616	cd 4b bb 	. K .
	pop hl			;c619	e1 	.
	jp lda0ah		;c61a	c3 0a da 	. . .
sub_c61dh:
	;042h is the ESC Key
	ld a,042h		;c61d	3e 42 	> B
	push hl			;c61f	e5 	.
	push bc			;c620	c5 	.
	call CHECK_FOR_KEY_PRESSED		;c621	cd 59 c2 	. Y .
	pop bc			;c624	c1 	.
	pop hl			;c625	e1 	.
	ret			;c626	c9 	.
lc627h:
	call sub_c666h		;c627	cd 66 c6 	. f .
	ld c,l			;c62a	4d 	M
	ld b,h			;c62b	44 	D
	push iy		;c62c	fd e5 	. .
	pop hl			;c62e	e1 	.
	ld de,0009bh		;c62f	11 9b 00 	. . .
	add hl,de			;c632	19 	.
	jp KL_LOG_EXT		;c633	c3 d1 bc 	. . .
sub_c636h:
	call sub_c666h		;c636	cd 66 c6 	. f .
	push hl			;c639	e5 	.
	ld de,0001ah		;c63a	11 1a 00 	. . .
	add hl,de			;c63d	19 	.
	ex de,hl			;c63e	eb 	.
	pop hl			;c63f	e1 	.
	ld (hl),e			;c640	73 	s
	inc hl			;c641	23 	#
	ld (hl),d			;c642	72 	r
	inc hl			;c643	23 	#
	ld b,008h		;c644	06 08 	. .
	xor a			;c646	af 	.
lc647h:
	ld (hl),0c3h		;c647	36 c3 	6 .
	inc hl			;c649	23 	#
	ld (hl),a			;c64a	77 	w
	inc hl			;c64b	23 	#
	ld (hl),a			;c64c	77 	w
	inc hl			;c64d	23 	#
	djnz lc647h		;c64e	10 f7 	. .
	ld (hl),a			;c650	77 	w
	call sub_c66eh		;c651	cd 6e c6 	. n .
	ld b,001h		;c654	06 01 	. .
	push ix		;c656	dd e5 	. .
	ld ix,lc664h		;c658	dd 21 64 c6 	. ! d .
	call GENERATE_RST18_AT_HL		;c65c	cd 7e de 	. ~ .
	pop ix		;c65f	dd e1 	. .
	ld (hl),000h		;c661	36 00 	6 .
	ret			;c663	c9 	.
lc664h:
	ld e,b			;c664	58 	X
	rst 0			;c665	c7 	.
sub_c666h:
	push iy		;c666	fd e5 	. .
	pop hl			;c668	e1 	.
	ld de,0009fh		;c669	11 9f 00 	. . .
	add hl,de			;c66c	19 	.
	ret			;c66d	c9 	.
sub_c66eh:
	push iy		;c66e	fd e5 	. .
	pop hl			;c670	e1 	.
	ld de,0013ah		;c671	11 3a 01 	. : .
	add hl,de			;c674	19 	.
	ret			;c675	c9 	.

;=======================================================================
RSX_ALIAS:
;=======================================================================
	and a			;c676	a7 	.
	jr nz,lc6b5h		;c677	20 3c 	  <
	call sub_c666h		;c679	cd 66 c6 	. f .
	ld a,(hl)			;c67c	7e 	~
	inc hl			;c67d	23 	#
	push hl			;c67e	e5 	.
	pop de			;c67f	d1 	.
	inc de			;c680	13 	.
	ld h,(hl)			;c681	66 	f
	ld l,a			;c682	6f 	o
lc683h:
	ld a,(hl)			;c683	7e 	~
	and a			;c684	a7 	.
	ret z			;c685	c8 	.
	and 07fh		;c686	e6 7f 	. 
	call TXT_OUTPUT		;c688	cd 5a bb 	. Z .
	ld a,(hl)			;c68b	7e 	~
	inc hl			;c68c	23 	#
	bit 7,a		;c68d	cb 7f 	. 
	jr z,lc683h		;c68f	28 f2 	( .
	inc de			;c691	13 	.
	push hl			;c692	e5 	.
	ld a,(de)			;c693	1a 	.
	ld l,a			;c694	6f 	o
	inc de			;c695	13 	.
	ld a,(de)			;c696	1a 	.
	ld h,a			;c697	67 	g
	inc de			;c698	13 	.
	push de			;c699	d5 	.
	inc hl			;c69a	23 	#
	ld e,(hl)			;c69b	5e 	^
	inc hl			;c69c	23 	#
	ld d,(hl)			;c69d	56 	V
	ld a,020h		;c69e	3e 20 	>
	call TXT_OUTPUT		;c6a0	cd 5a bb 	. Z .
lc6a3h:
	ld a,(de)			;c6a3	1a 	.
	inc de			;c6a4	13 	.
	cp 00dh		;c6a5	fe 0d 	. .
	jr z,lc6aeh		;c6a7	28 05 	( .
	call TXT_OUTPUT		;c6a9	cd 5a bb 	. Z .
	jr lc6a3h		;c6ac	18 f5 	. .
lc6aeh:
	pop de			;c6ae	d1 	.
	pop hl			;c6af	e1 	.
	call PRINT_CR_LF		;c6b0	cd 7d d9 	. } .
	jr lc683h		;c6b3	18 ce 	. .
lc6b5h:
	cp 002h		;c6b5	fe 02 	. .
	jp nz,MSG_WRONG_PARAMETER_AMT		;c6b7	c2 97 fb 	. . .
	ld l,(ix+002h)		;c6ba	dd 6e 02 	. n .
	ld h,(ix+003h)		;c6bd	dd 66 03 	. f .
	ld a,(hl)			;c6c0	7e 	~
	and a			;c6c1	a7 	.
	jp z,MSG_BAD_ALIAS_SPECIFIED		;c6c2	ca da fb 	. . .
sub_c6c5h:
	cp 011h		;c6c5	fe 11 	. .
	jp nc,MSG_BAD_ALIAS_SPECIFIED		;c6c7	d2 da fb 	. . .
	call sub_c666h		;c6ca	cd 66 c6 	. f .
	ld a,(hl)			;c6cd	7e 	~
	inc hl			;c6ce	23 	#
	push hl			;c6cf	e5 	.
	ld h,(hl)			;c6d0	66 	f
	ld l,a			;c6d1	6f 	o
	pop de			;c6d2	d1 	.
	inc de			;c6d3	13 	.
lc6d4h:
	ld a,(hl)			;c6d4	7e 	~
	and a			;c6d5	a7 	.
	jr z,lc6e4h		;c6d6	28 0c 	( .
	bit 7,a		;c6d8	cb 7f 	. 
	call nz,sub_c6e0h		;c6da	c4 e0 c6 	. . .
	inc hl			;c6dd	23 	#
	jr lc6d4h		;c6de	18 f4 	. .
sub_c6e0h:
	inc de			;c6e0	13 	.
	inc de			;c6e1	13 	.
	inc de			;c6e2	13 	.
	ret			;c6e3	c9 	.
lc6e4h:
	ld a,(de)			;c6e4	1a 	.
	cp 0c3h		;c6e5	fe c3 	. .
	jp nz,MSG_TOO_MANY_ALIASES		;c6e7	c2 de fb 	. . .
	push de			;c6ea	d5 	.
	push hl			;c6eb	e5 	.
	ld l,(ix+002h)		;c6ec	dd 6e 02 	. n .
	ld h,(ix+003h)		;c6ef	dd 66 03 	. f .
	ld b,(hl)			;c6f2	46 	F
	inc hl			;c6f3	23 	#
	ld e,(hl)			;c6f4	5e 	^
	inc hl			;c6f5	23 	#
	ld d,(hl)			;c6f6	56 	V
	pop hl			;c6f7	e1 	.
lc6f8h:
	ld a,(de)			;c6f8	1a 	.
	cp 05bh		;c6f9	fe 5b 	. [
	call nc,FUNC_SUBTRACT_32		;c6fb	d4 9d d9 	. . .
	ld (hl),a			;c6fe	77 	w
	inc de			;c6ff	13 	.
	inc hl			;c700	23 	#
	djnz lc6f8h		;c701	10 f5 	. .
	ld (hl),000h		;c703	36 00 	6 .
	dec hl			;c705	2b 	+
	set 7,(hl)		;c706	cb fe 	. .
	pop hl			;c708	e1 	.
	inc hl			;c709	23 	#
	push hl			;c70a	e5 	.
	call sub_c66eh		;c70b	cd 6e c6 	. n .
	ld de,00007h		;c70e	11 07 00 	. . .
	add hl,de			;c711	19 	.
	ld a,(hl)			;c712	7e 	~
	and a			;c713	a7 	.
	jr nz,lc71ch		;c714	20 06 	  .
	pop bc			;c716	c1 	.
	ld de,lc627h		;c717	11 27 c6 	. ' .
	push de			;c71a	d5 	.
	push bc			;c71b	c5 	.
lc71ch:
	ld a,(hl)			;c71c	7e 	~
	inc hl			;c71d	23 	#
	and a			;c71e	a7 	.
	jr nz,lc71ch		;c71f	20 fb 	  .
	dec hl			;c721	2b 	+
	ex de,hl			;c722	eb 	.
	pop hl			;c723	e1 	.
	ld (hl),e			;c724	73 	s
	inc hl			;c725	23 	#
	ld (hl),d			;c726	72 	r
	ex de,hl			;c727	eb 	.
	ld (hl),021h		;c728	36 21 	6 !
	ld e,l			;c72a	5d 	]
	ld d,h			;c72b	54 	T
	ld bc,00006h		;c72c	01 06 00 	. . .
	add hl,bc			;c72f	09 	.
	ex de,hl			;c730	eb 	.
	inc hl			;c731	23 	#
	ld (hl),e			;c732	73 	s
	inc hl			;c733	23 	#
	ld (hl),d			;c734	72 	r
	inc hl			;c735	23 	#
	ld (hl),0c3h		;c736	36 c3 	6 .
	push hl			;c738	e5 	.
	call sub_c66eh		;c739	cd 6e c6 	. n .
	ex de,hl			;c73c	eb 	.
	pop hl			;c73d	e1 	.
	inc hl			;c73e	23 	#
	ld (hl),e			;c73f	73 	s
	inc hl			;c740	23 	#
	ld (hl),d			;c741	72 	r
	inc hl			;c742	23 	#
	push hl			;c743	e5 	.
	call sub_da80h		;c744	cd 80 da 	. . .
	ld c,b			;c747	48 	H
	pop de			;c748	d1 	.
	ld a,c			;c749	79 	y
	and a			;c74a	a7 	.
	jr z,lc751h		;c74b	28 04 	( .
	ld b,000h		;c74d	06 00 	. .
	ldir		;c74f	ed b0 	. .
lc751h:
	ex de,hl			;c751	eb 	.
	ld (hl),00dh		;c752	36 0d 	6 .
	inc hl			;c754	23 	#
	ld (hl),000h		;c755	36 00 	6 .
	ret			;c757	c9 	.
	and a			;c758	a7 	.
	call nz,sub_c76bh		;c759	c4 6b c7 	. k .
	push hl			;c75c	e5 	.
	ld hl,RESET_ENTRY_RST_0		;c75d	21 00 00 	! . .
	add hl,sp			;c760	39 	9
	ld a,l			;c761	7d 	}
	pop hl			;c762	e1 	.
	cp 040h		;c763	fe 40 	. @
	jp c,MSG_BAD_ALIAS_SPECIFIED		;c765	da da fb 	. . .
	jp lf249h		;c768	c3 49 f2 	. I .
sub_c76bh:
	push hl			;c76b	e5 	.
	ld hl,0bf1fh		;c76c	21 1f bf 	! . .
	ld e,a			;c76f	5f 	_
	ld d,000h		;c770	16 00 	. .
	add hl,de			;c772	19 	.
	add hl,de			;c773	19 	.
	ld b,a			;c774	47 	G
lc775h:
	ld a,(ix+001h)		;c775	dd 7e 01 	. ~ .
	ld (hl),a			;c778	77 	w
	dec hl			;c779	2b 	+
	ld a,(ix+000h)		;c77a	dd 7e 00 	. ~ .
	ld (hl),a			;c77d	77 	w
	dec hl			;c77e	2b 	+
	inc ix		;c77f	dd 23 	. #
	inc ix		;c781	dd 23 	. #
	djnz lc775h		;c783	10 f0 	. .
	pop hl			;c785	e1 	.
	ret			;c786	c9 	.
	call sub_ca63h		;c787	cd 63 ca 	. c .
	ld d,(ix+000h)		;c78a	dd 56 00 	. V .
	ld e,001h		;c78d	1e 01 	. .
	call sub_c973h		;c78f	cd 73 c9 	. s .
	jp lcaa0h		;c792	c3 a0 ca 	. . .
lc795h:
	pop af			;c795	f1 	.
	pop bc			;c796	c1 	.
lc797h:
	pop hl			;c797	e1 	.
	pop bc			;c798	c1 	.
	pop de			;c799	d1 	.
	call lcaa0h		;c79a	cd a0 ca 	. . .
	ld hl,0be4ch		;c79d	21 4c be 	! L .
	ld a,(hl)			;c7a0	7e 	~
	inc hl			;c7a1	23 	#
	and 008h		;c7a2	e6 08 	. .
	or (hl)			;c7a4	b6 	.
	dec hl			;c7a5	2b 	+
	dec hl			;c7a6	2b 	+
	scf			;c7a7	37 	7
	ccf			;c7a8	3f 	?
	ret			;c7a9	c9 	.

;=======================================================================
RSX_HIDDEN_05:
;=======================================================================
	push de			;c7aa	d5 	.
	ld (iy+WS_DRIVE_NUMBER),e		;c7ab	fd 73 04 	. s .
	call sub_c7b3h		;c7ae	cd b3 c7 	. . .
	pop de			;c7b1	d1 	.
	ret			;c7b2	c9 	.
sub_c7b3h:
	ld a,e			;c7b3	7b 	{
	call sub_f143h		;c7b4	cd 43 f1 	. C .
	ret nz			;c7b7	c0 	.
	cp 008h		;c7b8	fe 08 	. .
	jp nc,lfa52h		;c7ba	d2 52 fa 	. R .
	call sub_ca63h		;c7bd	cd 63 ca 	. c .
	call sub_cb4ah		;c7c0	cd 4a cb 	. J .
	push de			;c7c3	d5 	.
	push bc			;c7c4	c5 	.
	ld b,001h		;c7c5	06 01 	. .
	ld a,045h		;c7c7	3e 45 	> E
	jr lc7eah		;c7c9	18 1f 	. .

;=======================================================================
RSX_HIDDEN_04:
;=======================================================================
	push de			;c7cb	d5 	.
	ld (iy+WS_DRIVE_NUMBER),e		;c7cc	fd 73 04 	. s .
	call sub_c7d4h		;c7cf	cd d4 c7 	. . .
	pop de			;c7d2	d1 	.
	ret			;c7d3	c9 	.
sub_c7d4h:
	ld a,e			;c7d4	7b 	{
	call sub_f143h		;c7d5	cd 43 f1 	. C .
	ret nz			;c7d8	c0 	.
	cp 008h		;c7d9	fe 08 	. .
	jp nc,lfa73h		;c7db	d2 73 fa 	. s .
	call sub_ca63h		;c7de	cd 63 ca 	. c .
	call sub_cb4ah		;c7e1	cd 4a cb 	. J .
	push de			;c7e4	d5 	.
	push bc			;c7e5	c5 	.
	ld b,0ffh		;c7e6	06 ff 	. .
	ld a,066h		;c7e8	3e 66 	> f
lc7eah:
	push hl			;c7ea	e5 	.
	push bc			;c7eb	c5 	.
lc7ech:
	push af			;c7ec	f5 	.
	call sub_c973h		;c7ed	cd 73 c9 	. s .
	jr nc,lc795h		;c7f0	30 a3 	0 .
	pop af			;c7f2	f1 	.
	di			;c7f3	f3 	.
	call sub_cad5h		;c7f4	cd d5 ca 	. . .
	ld a,e			;c7f7	7b 	{
	call sub_cad5h		;c7f8	cd d5 ca 	. . .
	ld a,d			;c7fb	7a 	z
	call sub_cad5h		;c7fc	cd d5 ca 	. . .
	ld a,e			;c7ff	7b 	{
	sra a		;c800	cb 2f 	. /
	sra a		;c802	cb 2f 	. /
	and 001h		;c804	e6 01 	. .
	call sub_cad5h		;c806	cd d5 ca 	. . .
	pop bc			;c809	c1 	.
	push bc			;c80a	c5 	.
	ld a,c			;c80b	79 	y
	call sub_cad5h		;c80c	cd d5 ca 	. . .
	ld a,002h		;c80f	3e 02 	> .
	call sub_cad5h		;c811	cd d5 ca 	. . .
	pop bc			;c814	c1 	.
	push bc			;c815	c5 	.
	ld a,c			;c816	79 	y
	call sub_cad5h		;c817	cd d5 ca 	. . .
	ld a,c			;c81a	79 	y
	and 0c0h		;c81b	e6 c0 	. .
	cp 080h		;c81d	fe 80 	. .
	ld a,009h		;c81f	3e 09 	> .
	jr z,lc825h		;c821	28 02 	( .
	ld a,02ah		;c823	3e 2a 	> *
lc825h:
	call sub_cad5h		;c825	cd d5 ca 	. . .
	ld a,0ffh		;c828	3e ff 	> .
	call sub_cad5h		;c82a	cd d5 ca 	. . .
	pop bc			;c82d	c1 	.
	pop hl			;c82e	e1 	.
	push hl			;c82f	e5 	.
	push bc			;c830	c5 	.
	bit 7,b		;c831	cb 78 	. x
	jp z,lc968h		;c833	ca 68 c9 	. h .
	call sub_caefh		;c836	cd ef ca 	. . .
lc839h:
	call sub_cb05h		;c839	cd 05 cb 	. . .
	pop bc			;c83c	c1 	.
	push bc			;c83d	c5 	.
	ld a,(0be4ch)		;c83e	3a 4c be 	: L .
	bit 3,a		;c841	cb 5f 	. _
	jr nz,lc891h		;c843	20 4c 	  L
	bit 4,a		;c845	cb 67 	. g
	jr nz,lc88ch		;c847	20 43 	  C
	ld a,(0be4dh)		;c849	3a 4d be 	: M .
	bit 1,a		;c84c	cb 4f 	. O
	jr nz,lc86ch		;c84e	20 1c 	  .
	ld a,(0be4eh)		;c850	3a 4e be 	: N .
	bit 6,a		;c853	cb 77 	. w
	jr nz,lc89fh		;c855	20 48 	  H
	and 037h		;c857	e6 37 	. 7
	jr nz,lc8cbh		;c859	20 70 	  p
	ld a,(0be4dh)		;c85b	3a 4d be 	: M .
	and 035h		;c85e	e6 35 	. 5
	jr nz,lc89fh		;c860	20 3d 	  =
lc862h:
	pop bc			;c862	c1 	.
	pop hl			;c863	e1 	.
	pop bc			;c864	c1 	.
	pop de			;c865	d1 	.
	call lcaa0h		;c866	cd a0 ca 	. . .
	xor a			;c869	af 	.
	scf			;c86a	37 	7
	ret			;c86b	c9 	.
lc86ch:
	call sub_fc65h		;c86c	cd 65 fc 	. e .
lc86fh:
	pop bc			;c86f	c1 	.
	di			;c870	f3 	.
	jp nz,lc797h		;c871	c2 97 c7 	. . .
	pop hl			;c874	e1 	.
	push hl			;c875	e5 	.
	push bc			;c876	c5 	.
	jr c,lc862h		;c877	38 e9 	8 .
lc879h:
	di			;c879	f3 	.
	ld a,066h		;c87a	3e 66 	> f
	bit 7,b		;c87c	cb 78 	. x
	jp nz,lc7ech		;c87e	c2 ec c7 	. . .
	ld a,045h		;c881	3e 45 	> E
	bit 0,b		;c883	cb 40 	. @
	jp nz,lc7ech		;c885	c2 ec c7 	. . .
	pop bc			;c888	c1 	.
	jp lc91dh		;c889	c3 1d c9 	. . .
lc88ch:
	call sub_fc61h		;c88c	cd 61 fc 	. a .
	jr lc86fh		;c88f	18 de 	. .
lc891h:
	ld a,(iy+00ah)		;c891	fd 7e 0a 	. ~ .
	and a			;c894	a7 	.
	jr nz,lc8c2h		;c895	20 2b 	  +
	call sub_cb4ah		;c897	cd 4a cb 	. J .
	call sub_fc5dh		;c89a	cd 5d fc 	. ] .
	jr lc86fh		;c89d	18 d0 	. .
lc89fh:
	ld a,(iy+00ah)		;c89f	fd 7e 0a 	. ~ .
	and 003h		;c8a2	e6 03 	. .
	jr nz,lc8c2h		;c8a4	20 1c 	  .
	pop bc			;c8a6	c1 	.
	push bc			;c8a7	c5 	.
	ld hl,lc86fh		;c8a8	21 6f c8 	! o .
	push hl			;c8ab	e5 	.
	ld a,(DISK_ERROR_MESSAGE_FLAG)		;c8ac	3a 78 be 	: x .
	and a			;c8af	a7 	.
	jp nz,lda18h		;c8b0	c2 18 da 	. . .
	bit 0,(iy+00dh)		;c8b3	fd cb 0d 46 	. . . F
	call z,sub_da91h		;c8b7	cc 91 da 	. . .
	bit 7,b		;c8ba	cb 78 	. x
	jp z,lfc55h		;c8bc	ca 55 fc 	. U .
	jp lfc51h		;c8bf	c3 51 fc 	. Q .
lc8c2h:
	dec a			;c8c2	3d 	=
	ld (iy+00ah),a		;c8c3	fd 77 0a 	. w .
	call lda14h		;c8c6	cd 14 da 	. . .
	jr lc86fh		;c8c9	18 a4 	. .
lc8cbh:
	ld a,(iy+00ah)		;c8cb	fd 7e 0a 	. ~ .
	and a			;c8ce	a7 	.
	jr nz,lc8e7h		;c8cf	20 16 	  .
	call sub_cb4ah		;c8d1	cd 4a cb 	. J .
	ld a,(DISK_ERROR_MESSAGE_FLAG)		;c8d4	3a 78 be 	: x .
	and a			;c8d7	a7 	.
	jr nz,lc8e2h		;c8d8	20 08 	  .
	call sub_da91h		;c8da	cd 91 da 	. . .
	call sub_fc59h		;c8dd	cd 59 fc 	. Y .
	jr lc86fh		;c8e0	18 8d 	. .
lc8e2h:
	call lda0ah		;c8e2	cd 0a da 	. . .
	jr lc86fh		;c8e5	18 88 	. .
lc8e7h:
	dec a			;c8e7	3d 	=
	ld (iy+00ah),a		;c8e8	fd 77 0a 	. w .
	and 003h		;c8eb	e6 03 	. .
	jr z,lc8f8h		;c8ed	28 09 	( .
	cp 002h		;c8ef	fe 02 	. .
	jr z,lc8feh		;c8f1	28 0b 	( .
	inc d			;c8f3	14 	.
	call sub_c973h		;c8f4	cd 73 c9 	. s .
	dec d			;c8f7	15 	.
lc8f8h:
	call sub_c973h		;c8f8	cd 73 c9 	. s .
	jp lc879h		;c8fb	c3 79 c8 	. y .
lc8feh:
	call sub_c9bfh		;c8fe	cd bf c9 	. . .
	jr lc8f8h		;c901	18 f5 	. .
lc903h:
	xor a			;c903	af 	.
	scf			;c904	37 	7
	ret			;c905	c9 	.

;=======================================================================
RSX_HIDDEN_06:
;=======================================================================
	push de			;c906	d5 	.
	call sub_c90ch		;c907	cd 0c c9 	. . .
	pop de			;c90a	d1 	.
	ret			;c90b	c9 	.
sub_c90ch:
	ld a,e			;c90c	7b 	{
	cp 008h		;c90d	fe 08 	. .
	jr z,lc903h		;c90f	28 f2 	( .
	call sub_f143h		;c911	cd 43 f1 	. C .
	call sub_ca63h		;c914	cd 63 ca 	. c .
	call sub_cb4ah		;c917	cd 4a cb 	. J .
	push de			;c91a	d5 	.
	push bc			;c91b	c5 	.
	push hl			;c91c	e5 	.
lc91dh:
	call sub_c973h		;c91d	cd 73 c9 	. s .
	ld b,000h		;c920	06 00 	. .
	push bc			;c922	c5 	.
	jr nc,lc8cbh		;c923	30 a6 	0 .
	pop bc			;c925	c1 	.
	pop hl			;c926	e1 	.
	push hl			;c927	e5 	.
	di			;c928	f3 	.
	push bc			;c929	c5 	.
	ld a,04dh		;c92a	3e 4d 	> M
	call sub_cad5h		;c92c	cd d5 ca 	. . .
	ld a,e			;c92f	7b 	{
	call sub_cad5h		;c930	cd d5 ca 	. . .
	ld a,002h		;c933	3e 02 	> .
	call sub_cad5h		;c935	cd d5 ca 	. . .
	ld c,008h		;c938	0e 08 	. .
	inc hl			;c93a	23 	#
	inc hl			;c93b	23 	#
	ld a,(hl)			;c93c	7e 	~
	and 0c0h		;c93d	e6 c0 	. .
	call nz,sub_c971h		;c93f	c4 71 c9 	. q .
	cp 080h		;c942	fe 80 	. .
	call z,sub_c971h		;c944	cc 71 c9 	. q .
	ld a,c			;c947	79 	y
	call sub_cad5h		;c948	cd d5 ca 	. . .
	ld c,050h		;c94b	0e 50 	. P
	ld a,(hl)			;c94d	7e 	~
	and 0c0h		;c94e	e6 c0 	. .
	call nz,sub_c971h		;c950	c4 71 c9 	. q .
	call nz,sub_c971h		;c953	c4 71 c9 	. q .
	cp 080h		;c956	fe 80 	. .
	call z,sub_c96eh		;c958	cc 6e c9 	. n .
	ld a,c			;c95b	79 	y
	call sub_cad5h		;c95c	cd d5 ca 	. . .
	ld a,0e5h		;c95f	3e e5 	> .
	call sub_cad5h		;c961	cd d5 ca 	. . .
	pop bc			;c964	c1 	.
	pop hl			;c965	e1 	.
	push hl			;c966	e5 	.
	push bc			;c967	c5 	.
lc968h:
	call sub_cb34h		;c968	cd 34 cb 	. 4 .
	jp lc839h		;c96b	c3 39 c8 	. 9 .
sub_c96eh:
	ld c,011h		;c96e	0e 11 	. .
	ret			;c970	c9 	.
sub_c971h:
	inc c			;c971	0c 	.
	ret			;c972	c9 	.
sub_c973h:
	push hl			;c973	e5 	.
	push de			;c974	d5 	.
	push iy		;c975	fd e5 	. .
	pop hl			;c977	e1 	.
	ld de,00080h		;c978	11 80 00 	. . .
	add hl,de			;c97b	19 	.
	pop de			;c97c	d1 	.
	push de			;c97d	d5 	.
	ld a,e			;c97e	7b 	{
	and 003h		;c97f	e6 03 	. .
	ld e,a			;c981	5f 	_
	ld d,000h		;c982	16 00 	. .
	add hl,de			;c984	19 	.
	pop de			;c985	d1 	.
	ld a,(hl)			;c986	7e 	~
	cp d			;c987	ba 	.
	jr z,lc9bbh		;c988	28 31 	( 1
	cp 0ffh		;c98a	fe ff 	. .
	jr nz,lc996h		;c98c	20 08 	  .
	push hl			;c98e	e5 	.
	call sub_c9bfh		;c98f	cd bf c9 	. . .
	pop hl			;c992	e1 	.
	di			;c993	f3 	.
	ld (hl),000h		;c994	36 00 	6 .
lc996h:
	ld a,00fh		;c996	3e 0f 	> .
	call sub_cad5h		;c998	cd d5 ca 	. . .
	ld a,e			;c99b	7b 	{
	and 003h		;c99c	e6 03 	. .
	call sub_cad5h		;c99e	cd d5 ca 	. . .
	ld a,d			;c9a1	7a 	z
	bit 1,(iy+041h)		;c9a2	fd cb 41 4e 	. . A N
	jr z,lc9a9h		;c9a6	28 01 	( .
	add a,a			;c9a8	87 	.
lc9a9h:
	call sub_cad5h		;c9a9	cd d5 ca 	. . .
	ld a,(hl)			;c9ac	7e 	~
	sub d			;c9ad	92 	.
	jr nc,lc9b2h		;c9ae	30 02 	0 .
	ld a,d			;c9b0	7a 	z
	sub (hl)			;c9b1	96 	.
lc9b2h:
	inc a			;c9b2	3c 	<
	call sub_cb51h		;c9b3	cd 51 cb 	. Q .
	ld a,d			;c9b6	7a 	z
	ld (hl),a			;c9b7	77 	w
	call sub_c9e9h		;c9b8	cd e9 c9 	. . .
lc9bbh:
	pop hl			;c9bb	e1 	.
	xor a			;c9bc	af 	.
	scf			;c9bd	37 	7
	ret			;c9be	c9 	.
sub_c9bfh:
	call sub_c9deh		;c9bf	cd de c9 	. . .
	ld a,04dh		;c9c2	3e 4d 	> M
	call sub_cb51h		;c9c4	cd 51 cb 	. Q .
	call sub_c9e9h		;c9c7	cd e9 c9 	. . .
	ret z			;c9ca	c8 	.
	ld a,00ah		;c9cb	3e 0a 	> .
	call sub_cb51h		;c9cd	cd 51 cb 	. Q .
	call sub_c9deh		;c9d0	cd de c9 	. . .
	ld a,00ah		;c9d3	3e 0a 	> .
	call sub_cb51h		;c9d5	cd 51 cb 	. Q .
	jp sub_c9e9h		;c9d8	c3 e9 c9 	. . .
sub_c9dbh:
	call sub_f143h		;c9db	cd 43 f1 	. C .
sub_c9deh:
	ld a,007h		;c9de	3e 07 	> .
	call sub_cad5h		;c9e0	cd d5 ca 	. . .
	ld a,e			;c9e3	7b 	{
	and 003h		;c9e4	e6 03 	. .
	jp sub_cad5h		;c9e6	c3 d5 ca 	. . .
sub_c9e9h:
	ld a,063h		;c9e9	3e 63 	> c
	ld (0be4dh),a		;c9eb	32 4d be 	2 M .
	ld a,008h		;c9ee	3e 08 	> .
	call sub_cad5h		;c9f0	cd d5 ca 	. . .
	call sub_cb05h		;c9f3	cd 05 cb 	. . .
	ld a,(0be4dh)		;c9f6	3a 4d be 	: M .
	cp 063h		;c9f9	fe 63 	. c
	jr z,sub_c9e9h		;c9fb	28 ec 	( .
	ld a,(0be4ch)		;c9fd	3a 4c be 	: L .
	bit 4,a		;ca00	cb 67 	. g
	ret			;ca02	c9 	.
sub_ca03h:
	res 2,(iy+00dh)		;ca03	fd cb 0d 96 	. . . .
	ld a,e			;ca07	7b 	{
	cp 0c7h		;ca08	fe c7 	. .
	ret nc			;ca0a	d0 	.
	push de			;ca0b	d5 	.
	call sub_f143h		;ca0c	cd 43 f1 	. C .
	call sub_ca14h		;ca0f	cd 14 ca 	. . .
	pop de			;ca12	d1 	.
	ret			;ca13	c9 	.
sub_ca14h:
	call sub_ca30h		;ca14	cd 30 ca 	. 0 .
	ret z			;ca17	c8 	.
	call sub_c9bfh		;ca18	cd bf c9 	. . .
	ld (iy+03bh),000h		;ca1b	fd 36 3b 00 	. 6 ; .
	call sub_ca30h		;ca1f	cd 30 ca 	. 0 .
	ret z			;ca22	c8 	.
	call sub_ca30h		;ca23	cd 30 ca 	. 0 .
	ret z			;ca26	c8 	.
	ret c			;ca27	d8 	.
	call sub_fc5dh		;ca28	cd 5d fc 	. ] .
	jp nz,lda0fh		;ca2b	c2 0f da 	. . .
	jr sub_ca14h		;ca2e	18 e4 	. .
sub_ca30h:
	call sub_ca63h		;ca30	cd 63 ca 	. c .
	ld a,04ah		;ca33	3e 4a 	> J
	call sub_cad5h		;ca35	cd d5 ca 	. . .
	ld a,e			;ca38	7b 	{
	call sub_cad5h		;ca39	cd d5 ca 	. . .
	call sub_cb05h		;ca3c	cd 05 cb 	. . .
	call lcaa0h		;ca3f	cd a0 ca 	. . .
	ld a,(0be4ch)		;ca42	3a 4c be 	: L .
	and 018h		;ca45	e6 18 	. .
	scf			;ca47	37 	7
	ccf			;ca48	3f 	?
	ret nz			;ca49	c0 	.
	ld a,(0be4dh)		;ca4a	3a 4d be 	: M .
	and 001h		;ca4d	e6 01 	. .
	scf			;ca4f	37 	7
	ret nz			;ca50	c0 	.
	ld a,(0be52h)		;ca51	3a 52 be 	: R .
	cp 002h		;ca54	fe 02 	. .
	scf			;ca56	37 	7
	ret nz			;ca57	c0 	.
	ld a,(0be4fh)		;ca58	3a 4f be 	: O .
	ld b,a			;ca5b	47 	G
	ld a,(0be51h)		;ca5c	3a 51 be 	: Q .
	ld c,a			;ca5f	4f 	O
	xor a			;ca60	af 	.
	scf			;ca61	37 	7
	ret			;ca62	c9 	.
sub_ca63h:
	push bc			;ca63	c5 	.
	res 3,(iy+00dh)		;ca64	fd cb 0d 9e 	. . . .
	push hl			;ca68	e5 	.
	push de			;ca69	d5 	.
	ld hl,0be67h		;ca6a	21 67 be 	! g .
	call KL_DEL_TICKER		;ca6d	cd ec bc 	. . .
	xor a			;ca70	af 	.
	ld (0be5fh),a		;ca71	32 5f be 	2 _ .
	pop de			;ca74	d1 	.
	pop hl			;ca75	e1 	.
	ld bc,0fa7eh		;ca76	01 7e fa 	. ~ .
	inc a			;ca79	3c 	<
	out (c),a		;ca7a	ed 79 	. y
	ld a,(iy+03bh)		;ca7c	fd 7e 3b 	. ~ ;
	and a			;ca7f	a7 	.
	jr nz,lca92h		;ca80	20 10 	  .
	ld bc,(0be44h)		;ca82	ed 4b 44 be 	. K D .
lca86h:
	ei			;ca86	fb 	.
	halt			;ca87	76 	v
	halt			;ca88	76 	v
	halt			;ca89	76 	v
	halt			;ca8a	76 	v
	halt			;ca8b	76 	v
	halt			;ca8c	76 	v
	dec bc			;ca8d	0b 	.
	ld a,b			;ca8e	78 	x
	or c			;ca8f	b1 	.
	jr nz,lca86h		;ca90	20 f4 	  .
lca92h:
	di			;ca92	f3 	.
	pop bc			;ca93	c1 	.
	xor a			;ca94	af 	.
	ld (iy+048h),a		;ca95	fd 77 48 	. w H
	ld (iy+049h),a		;ca98	fd 77 49 	. w I
	inc a			;ca9b	3c 	<
	ld (iy+03bh),a		;ca9c	fd 77 3b 	. w ;
	ret			;ca9f	c9 	.
lcaa0h:
	push hl			;caa0	e5 	.
	ld hl,(0be46h)		;caa1	2a 46 be 	* F .
	ld (iy+048h),l		;caa4	fd 75 48 	. u H
	ld (iy+049h),h		;caa7	fd 74 49 	. t I
	set 3,(iy+00dh)		;caaa	fd cb 0d de 	. . . .
	xor a			;caae	af 	.
	ld (iy+00eh),a		;caaf	fd 77 0e 	. w .
	pop hl			;cab2	e1 	.
	ei			;cab3	fb 	.
	ret			;cab4	c9 	.
sub_cab5h:
	ld a,(iy+WS_DRIVE_NUMBER)		;cab5	fd 7e 04 	. ~ .
	call sub_f143h		;cab8	cd 43 f1 	. C .
	call sub_ca63h		;cabb	cd 63 ca 	. c .
	ld a,004h		;cabe	3e 04 	> .
	call sub_cad5h		;cac0	cd d5 ca 	. . .
	ld a,e			;cac3	7b 	{
	and 007h		;cac4	e6 07 	. .
	call sub_cad5h		;cac6	cd d5 ca 	. . .
	call sub_cb05h		;cac9	cd 05 cb 	. . .
	call lcaa0h		;cacc	cd a0 ca 	. . .
	ld a,(0be4ch)		;cacf	3a 4c be 	: L .
	and 0f8h		;cad2	e6 f8 	. .
	ret			;cad4	c9 	.
sub_cad5h:
	push af			;cad5	f5 	.
	ld bc,0fb7eh		;cad6	01 7e fb 	. ~ .
lcad9h:
	in a,(c)		;cad9	ed 78 	. x
	add a,a			;cadb	87 	.
	jr nc,lcad9h		;cadc	30 fb 	0 .
	add a,a			;cade	87 	.
	jr nc,lcae3h		;cadf	30 02 	0 .
	pop af			;cae1	f1 	.
	ret			;cae2	c9 	.
lcae3h:
	pop af			;cae3	f1 	.
	inc c			;cae4	0c 	.
	out (c),a		;cae5	ed 79 	. y
	dec c			;cae7	0d 	.
	ld a,00fh		;cae8	3e 0f 	> .
lcaeah:
	dec a			;caea	3d 	=
	nop			;caeb	00 	.
	jr nz,lcaeah		;caec	20 fc 	  .
	ret			;caee	c9 	.
sub_caefh:
	ld bc,0fb7eh		;caef	01 7e fb 	. ~ .
	jp lcafbh		;caf2	c3 fb ca 	. . .
lcaf5h:
	inc c			;caf5	0c 	.
	in a,(c)		;caf6	ed 78 	. x
	ld (hl),a			;caf8	77 	w
	dec c			;caf9	0d 	.
	inc hl			;cafa	23 	#
lcafbh:
	in a,(c)		;cafb	ed 78 	. x
	jp p,lcafbh		;cafd	f2 fb ca 	. . .
	and 020h		;cb00	e6 20 	.
	jr nz,lcaf5h		;cb02	20 f1 	  .
	ret			;cb04	c9 	.
sub_cb05h:
	ld bc,0fb7eh		;cb05	01 7e fb 	. ~ .
	push hl			;cb08	e5 	.
	push de			;cb09	d5 	.
	ld hl,0be4ch		;cb0a	21 4c be 	! L .
	ld d,000h		;cb0d	16 00 	. .
lcb0fh:
	in a,(c)		;cb0f	ed 78 	. x
	cp 0c0h		;cb11	fe c0 	. .
	jr c,lcb0fh		;cb13	38 fa 	8 .
	inc c			;cb15	0c 	.
	in a,(c)		;cb16	ed 78 	. x
	dec c			;cb18	0d 	.
	ld (hl),a			;cb19	77 	w
	inc hl			;cb1a	23 	#
	inc d			;cb1b	14 	.
	ld a,005h		;cb1c	3e 05 	> .
lcb1eh:
	dec a			;cb1e	3d 	=
	jr nz,lcb1eh		;cb1f	20 fd 	  .
	in a,(c)		;cb21	ed 78 	. x
	and 010h		;cb23	e6 10 	. .
	jr nz,lcb0fh		;cb25	20 e8 	  .
	ld hl,0be4ch		;cb27	21 4c be 	! L .
	ld a,(hl)			;cb2a	7e 	~
	and 0c0h		;cb2b	e6 c0 	. .
	dec hl			;cb2d	2b 	+
	ld (hl),d			;cb2e	72 	r
	pop de			;cb2f	d1 	.
	pop hl			;cb30	e1 	.
	ret nz			;cb31	c0 	.
	scf			;cb32	37 	7
	ret			;cb33	c9 	.
sub_cb34h:
	ld bc,0fb7eh		;cb34	01 7e fb 	. ~ .
	jp lcb40h		;cb37	c3 40 cb 	. @ .
lcb3ah:
	inc c			;cb3a	0c 	.
	ld a,(hl)			;cb3b	7e 	~
	out (c),a		;cb3c	ed 79 	. y
	dec c			;cb3e	0d 	.
	inc hl			;cb3f	23 	#
lcb40h:
	in a,(c)		;cb40	ed 78 	. x
	jp p,lcb40h		;cb42	f2 40 cb 	. @ .
	and 020h		;cb45	e6 20 	.
	jr nz,lcb3ah		;cb47	20 f1 	  .
	ret			;cb49	c9 	.
sub_cb4ah:
	ld a,(iy+009h)		;cb4a	fd 7e 09 	. ~ .
	ld (iy+00ah),a		;cb4d	fd 77 0a 	. w .
	ret			;cb50	c9 	.
sub_cb51h:
	push af			;cb51	f5 	.
	ld a,(0be4ah)		;cb52	3a 4a be 	: J .
	call sub_cb62h		;cb55	cd 62 cb 	. b .
	pop af			;cb58	f1 	.
	dec a			;cb59	3d 	=
	jr nz,sub_cb51h		;cb5a	20 f5 	  .
	ld a,(0be49h)		;cb5c	3a 49 be 	: I .
	jp sub_cb62h		;cb5f	c3 62 cb 	. b .
sub_cb62h:
	push af			;cb62	f5 	.
	ld a,0f6h		;cb63	3e f6 	> .
lcb65h:
	dec a			;cb65	3d 	=
	jr nz,lcb65h		;cb66	20 fd 	  .
	pop af			;cb68	f1 	.
	dec a			;cb69	3d 	=
	jr nz,sub_cb62h		;cb6a	20 f6 	  .
	ret			;cb6c	c9 	.
sub_cb6dh:
	ld a,003h		;cb6d	3e 03 	> .
	call sub_cad5h		;cb6f	cd d5 ca 	. . .
	ld a,(0be4ah)		;cb72	3a 4a be 	: J .
	dec a			;cb75	3d 	=
	rlc a		;cb76	cb 07 	. .
	rlc a		;cb78	cb 07 	. .
	rlc a		;cb7a	cb 07 	. .
	cpl			;cb7c	2f 	/
	and 0f0h		;cb7d	e6 f0 	. .
	or 001h		;cb7f	f6 01 	. .
	call sub_cad5h		;cb81	cd d5 ca 	. . .
	ld a,003h		;cb84	3e 03 	> .
	jp sub_cad5h		;cb86	c3 d5 ca 	. . .

;=======================================================================
RSX_RANDOM:
;=======================================================================

	and a			;cb89	a7 	.
	jp nz,MSG_TOO_MANY_PARAMETERS		;cb8a	c2 9f fb 	. . .

	;Check output buffer (header)
	ld a,(iy+01bh)		;cb8d	fd 7e 1b 	. ~ .
	or (iy+01ch)		;cb90	fd b6 1c 	. . .
	jp nz,MSG_OUTPUT_FILE_ALEADY_OPEN		;cb93	c2 ce fb 	. . .

	;Check input buffer (file header)
	ld l,(iy+017h)		;cb96	fd 6e 17 	. n .
	ld h,(iy+018h)		;cb99	fd 66 18 	. f .
	ld a,h			;cb9c	7c 	|
	or l			;cb9d	b5 	.
	jp z,MSG_INPUT_FILE_NOT_OPEN		;cb9e	ca ca fb 	. . .
	push hl			;cba1	e5 	.
	pop ix		;cba2	dd e1 	. .
	ld a,(iy+039h)		;cba4	fd 7e 39 	. ~ 9
	cp 001h		;cba7	fe 01 	. .
	call z,sub_e604h		;cba9	cc 04 e6 	. . .

	;Check buffer type to see if type is set to I (049h)
	ld a,(ix-00fh)		;cbac	dd 7e f1 	. ~ .
	cp 049h		;cbaf	fe 49 	. I
	jp nz,MSG_INPUT_FILE_NOT_OPEN		;cbb1	c2 ca fb 	. . .
	push ix		;cbb4	dd e5 	. .
	pop hl			;cbb6	e1 	.
	ld (iy+01bh),l		;cbb7	fd 75 1b 	. u .
	ld (iy+01ch),h		;cbba	fd 74 1c 	. t .
	ld (ix-00fh),052h		;Buffer type - Random (R/052h) ;cbbd	dd 36 f1 52 	. 6 . R
	ld (iy+03ah),002h		;cbc1	fd 36 3a 02 	. 6 : .
	ret			;cbc5	c9 	.
lcbc6h:
	ld l,(ix+000h)		;cbc6	dd 6e 00 	. n .
	ld h,(ix+001h)		;cbc9	dd 66 01 	. f .
	ld a,(iy+01fh)		;cbcc	fd 7e 1f 	. ~ .
	ld (hl),a			;cbcf	77 	w
	inc hl			;cbd0	23 	#
	ld a,(iy+020h)		;cbd1	fd 7e 20 	. ~
	ld (hl),a			;cbd4	77 	w
	ret			;cbd5	c9 	.

;=======================================================================
RSX_POINT:
;=======================================================================
	cp 002h		;cbd6	fe 02 	. .
	jr z,lcbc6h		;cbd8	28 ec 	( .
	jp nc,MSG_TOO_MANY_PARAMETERS		;cbda	d2 9f fb 	. . .
	and a			;cbdd	a7 	.
	jp z,MSG_WRONG_PARAMETER_AMT		;cbde	ca 97 fb 	. . .
	ld a,(iy+017h)		;cbe1	fd 7e 17 	. ~ .
	or (iy+018h)		;cbe4	fd b6 18 	. . .
	jp z,MSG_INPUT_FILE_NOT_OPEN		;cbe7	ca ca fb 	. . .
	ld c,(iy+01fh)		;cbea	fd 4e 1f 	. N .
	ld b,(iy+020h)		;cbed	fd 46 20 	. F
	ld l,(ix+000h)		;cbf0	dd 6e 00 	. n .
	ld h,(ix+001h)		;cbf3	dd 66 01 	. f .
	and a			;cbf6	a7 	.
	sbc hl,bc		;cbf7	ed 42 	. B
	ret z			;cbf9	c8 	.
	jr c,lcc06h		;cbfa	38 0a 	8 .
lcbfch:
	call CAS_IN_CHAR		;cbfc	cd 80 bc 	. . .
	ret nc			;cbff	d0 	.
	dec hl			;cc00	2b 	+
	ld a,h			;cc01	7c 	|
	or l			;cc02	b5 	.
	jr nz,lcbfch		;cc03	20 f7 	  .
	ret			;cc05	c9 	.
lcc06h:
	ld l,(iy+01fh)		;cc06	fd 6e 1f 	. n .
	ld h,(iy+020h)		;cc09	fd 66 20 	. f
	ld c,(ix+000h)		;cc0c	dd 4e 00 	. N .
	ld b,(ix+001h)		;cc0f	dd 46 01 	. F .
	and a			;cc12	a7 	.
	sbc hl,bc		;cc13	ed 42 	. B
	ld c,(iy+017h)		;cc15	fd 4e 17 	. N .
	ld b,(iy+018h)		;cc18	fd 46 18 	. F .
	push bc			;cc1b	c5 	.
	pop ix		;cc1c	dd e1 	. .
lcc1eh:
	ld a,h			;cc1e	7c 	|
	or l			;cc1f	b5 	.
	ret z			;cc20	c8 	.
	push hl			;cc21	e5 	.
	call sub_cc3ch		;cc22	cd 3c cc 	. < .
	pop hl			;cc25	e1 	.
	ret nz			;cc26	c0 	.
	dec hl			;cc27	2b 	+
	jr lcc1eh		;cc28	18 f4 	. .
lcc2ah:
	ld a,(ix-00fh)		;cc2a	dd 7e f1 	. ~ .
	cp 052h		;cc2d	fe 52 	. R
	call z,sub_cc83h		;cc2f	cc 83 cc 	. . .
	ld c,(ix+001h)		;cc32	dd 4e 01 	. N .
	ld b,(ix+000h)		;cc35	dd 46 00 	. F .
	call sub_cc93h		;cc38	cd 93 cc 	. . .
	ret nz			;cc3b	c0 	.
sub_cc3ch:
	ld l,(ix-009h)		;cc3c	dd 6e f7 	. n .
	ld h,(ix-008h)		;cc3f	dd 66 f8 	. f .
	ld a,h			;cc42	7c 	|
	or l			;cc43	b5 	.
	jr z,lcc2ah		;cc44	28 e4 	( .
	dec hl			;cc46	2b 	+
	ld (ix-009h),l		;cc47	dd 75 f7 	. u .
	ld (ix-008h),h		;cc4a	dd 74 f8 	. t .
	ld l,(ix-004h)		;cc4d	dd 6e fc 	. n .
	ld h,(ix-003h)		;cc50	dd 66 fd 	. f .
	inc hl			;cc53	23 	#
	ld (ix-004h),l		;cc54	dd 75 fc 	. u .
	ld (ix-003h),h		;cc57	dd 74 fd 	. t .
	ld l,(ix-00eh)		;cc5a	dd 6e f2 	. n .
	ld h,(ix-00dh)		;cc5d	dd 66 f3 	. f .
	inc hl			;cc60	23 	#
	ld (ix-00eh),l		;cc61	dd 75 f2 	. u .
	ld (ix-00dh),h		;cc64	dd 74 f3 	. t .
	ld l,(ix-006h)		;cc67	dd 6e fa 	. n .
	ld h,(ix-005h)		;cc6a	dd 66 fb 	. f .
	dec hl			;cc6d	2b 	+
	ld (ix-006h),l		;cc6e	dd 75 fa 	. u .
	ld (ix-005h),h		;cc71	dd 74 fb 	. t .
	ld l,(iy+01fh)		;cc74	fd 6e 1f 	. n .
	ld h,(iy+020h)		;cc77	fd 66 20 	. f
	dec hl			;cc7a	2b 	+
	ld (iy+020h),h		;cc7b	fd 74 20 	. t
	ld (iy+01fh),l		;cc7e	fd 75 1f 	. u .
	xor a			;cc81	af 	.
	ret			;cc82	c9 	.
sub_cc83h:
	bit 2,(iy+03ah)		;cc83	fd cb 3a 56 	. . : V
	ret z			;cc87	c8 	.
	call sub_e648h		;cc88	cd 48 e6 	. H .
	call sub_eac1h		;cc8b	cd c1 ea 	. . .
	res 2,(iy+03ah)		;cc8e	fd cb 3a 96 	. . : .
	ret			;cc92	c9 	.
sub_cc93h:
	call sub_e648h		;cc93	cd 48 e6 	. H .
	push ix		;cc96	dd e5 	. .
	push ix		;cc98	dd e5 	. .
	pop hl			;cc9a	e1 	.
	call sub_ef61h		;cc9b	cd 61 ef 	. a .
	pop ix		;cc9e	dd e1 	. .
	ret nz			;cca0	c0 	.
	ld (ix-002h),b		;cca1	dd 70 fe 	. p .
	ld (ix-001h),c		;cca4	dd 71 ff 	. q .
	ld hl,00200h		;cca7	21 00 02 	! . .
	ld (ix-004h),l		;ccaa	dd 75 fc 	. u .
	ld (ix-003h),h		;ccad	dd 74 fd 	. t .
	push ix		;ccb0	dd e5 	. .
	pop hl			;ccb2	e1 	.
	ld de,00200h		;ccb3	11 00 02 	. . .
	add hl,de			;ccb6	19 	.
	ld (ix-006h),l		;ccb7	dd 75 fa 	. u .
	ld (ix-005h),h		;ccba	dd 74 fb 	. t .
	ld hl,001fch		;ccbd	21 fc 01 	! . .
	ld (ix-009h),l		;ccc0	dd 75 f7 	. u .
	ld (ix-008h),h		;ccc3	dd 74 f8 	. t .
	xor a			;ccc6	af 	.
	ld (ix-00eh),a		;ccc7	dd 77 f2 	. w .
	ld (ix-00dh),a		;ccca	dd 77 f3 	. w .
	ret			;cccd	c9 	.

;=======================================================================
RSX_LS:
;=======================================================================
	and a			;ccce	a7 	.
	jp nz,MSG_TOO_MANY_PARAMETERS		;cccf	c2 9f fb 	. . .
	call PRINT_CR_LF		;ccd2	cd 7d d9 	. } .
	call sub_da62h		;ccd5	cd 62 da 	. b .
	call sub_d9a0h		;ccd8	cd a0 d9 	. . .
	ret nc			;ccdb	d0 	.
	cp 081h		;ccdc	fe 81 	. .
	ld a,000h		;ccde	3e 00 	> .
	jp nz,RSX_DIR		;cce0	c2 ce d4 	. . .
	ld ix,0bee0h		;cce3	dd 21 e0 be 	. ! . .
	ld b,010h		;cce7	06 10 	. .
	ld hl,0bee0h		;cce9	21 e0 be 	! . .
lccech:
	ld (hl),03fh		;ccec	36 3f 	6 ?
	inc hl			;ccee	23 	#
	djnz lccech		;ccef	10 fb 	. .
	call sub_e26fh		;ccf1	cd 6f e2 	. o .
	jp PRINT_CR_LF		;ccf4	c3 7d d9 	. } .

;=======================================================================
RSX_EXEC:
;=======================================================================
	cp 002h		;ccf7	fe 02 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;ccf9	d2 9f fb 	. . .
	and a			;ccfc	a7 	.
	call z,PROMPT_ENTER_NAME		;ccfd	cc dc d8 	. . .
lcd00h:
	call sub_da80h		;cd00	cd 80 da 	. . .
	call sub_d882h		;cd03	cd 82 d8 	. . .
	call CAS_IN_OPEN		;cd06	cd 77 bc 	. w .
	jr nc,lcd33h		;cd09	30 28 	0 (
	jr lcd10h		;cd0b	18 03 	. .
lcd0dh:
	call TXT_OUTPUT		;cd0d	cd 5a bb 	. Z .
lcd10h:
	call CAS_IN_CHAR		;cd10	cd 80 bc 	. . .
	jr nc,lcd33h		;cd13	30 1e 	0 .
	cp 07ch		;cd15	fe 7c 	. |
	jr nz,lcd0dh		;cd17	20 f4 	  .
	push iy		;cd19	fd e5 	. .
	pop hl			;cd1b	e1 	.
	ld de,00218h		;cd1c	11 18 02 	. . .
	add hl,de			;cd1f	19 	.
lcd20h:
	call CAS_IN_CHAR		;cd20	cd 80 bc 	. . .
	jr nc,lcd33h		;cd23	30 0e 	0 .
	ld (hl),a			;cd25	77 	w
	inc hl			;cd26	23 	#
	cp 00dh		;cd27	fe 0d 	. .
	jr nz,lcd20h		;cd29	20 f5 	  .
	call sub_f246h		;cd2b	cd 46 f2 	. F .
	call CAS_IN_CHAR		;cd2e	cd 80 bc 	. . .
	jr c,lcd0dh		;cd31	38 da 	8 .
lcd33h:
	jp CAS_IN_CLOSE		;cd33	c3 7a bc 	. z .


;=======================================================================
RSX_ACCESS:
;=======================================================================
	cp 003h		;cd36	fe 03 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;cd38	d2 9f fb 	. . .
	cp 002h		;cd3b	fe 02 	. .
	jp nz,MSG_WRONG_PARAMETER_AMT		;cd3d	c2 97 fb 	. . .
	ld a,(ix+000h)		;cd40	dd 7e 00 	. ~ .
	xor 024h		;cd43	ee 24 	. $
	inc ix		;cd45	dd 23 	. #
	inc ix		;cd47	dd 23 	. #
	ld (iy+002h),a		;cd49	fd 77 02 	. w .
	call sub_da1bh		;cd4c	cd 1b da 	. . .
	ret nz			;cd4f	c0 	.
	call sub_da6ah		;cd50	cd 6a da 	. j .
	ret nc			;cd53	d0 	.
	ld ix,0bef0h		;cd54	dd 21 f0 be 	. ! . .
	call sub_edb6h		;cd58	cd b6 ed 	. . .
	jp nz,MSG_BAD_FILE_NAME		;cd5b	c2 8e fb 	. . .
	ld (iy+014h),002h		;cd5e	fd 36 14 02 	. 6 . .
	call sub_f798h		;cd62	cd 98 f7 	. . .
	ret nc			;cd65	d0 	.
	bit 3,(iy+041h)		;cd66	fd cb 41 5e 	. . A ^
	jp z,MSG_NO_MATCH		;cd6a	ca ab fb 	. . .
	ret			;cd6d	c9 	.
sub_cd6eh:
	push hl			;cd6e	e5 	.
	pop ix		;cd6f	dd e1 	. .
	ld a,(iy+04eh)		;cd71	fd 7e 4e 	. ~ N
	cp (ix+01bh)		;cd74	dd be 1b 	. . .
	jp nz,MSG_ACCESS_DENIED		;cd77	c2 a7 fb 	. . .
	ld a,(iy+04fh)		;cd7a	fd 7e 4f 	. ~ O
	cp (ix+01ch)		;cd7d	dd be 1c 	. . .
	jp nz,MSG_ACCESS_DENIED		;cd80	c2 a7 fb 	. . .
	ld a,(iy+002h)		;cd83	fd 7e 02 	. ~ .
	ld (ix+011h),a		;cd86	dd 77 11 	. w .
	call sub_efbch		;cd89	cd bc ef 	. . .
	ld (iy+040h),0ffh		;cd8c	fd 36 40 ff 	. 6 @ .
	ld (iy+03fh),0ffh		;cd90	fd 36 3f ff 	. 6 ? .
	ret			;cd94	c9 	.
sub_cd95h:
	push hl			;cd95	e5 	.
	pop ix		;cd96	dd e1 	. .
	ld a,(ix+00ah)		;cd98	dd 7e 0a 	. ~ .
	bit 0,(iy+002h)		;cd9b	fd cb 02 46 	. . . F
	call sub_cdb5h		;cd9f	cd b5 cd 	. . .
	ld (ix+00ah),a		;cda2	dd 77 0a 	. w .
	ld a,(ix+009h)		;cda5	dd 7e 09 	. ~ .
	bit 1,(iy+002h)		;cda8	fd cb 02 4e 	. . . N
	call sub_cdb5h		;cdac	cd b5 cd 	. . .
	ld (ix+009h),a		;cdaf	dd 77 09 	. w .
	jp sub_efbch		;cdb2	c3 bc ef 	. . .
sub_cdb5h:
	jr z,lcdbah		;cdb5	28 03 	( .
	set 7,a		;cdb7	cb ff 	. .
	ret			;cdb9	c9 	.
lcdbah:
	res 7,a		;cdba	cb bf 	. .
	ret			;cdbc	c9 	.

;=======================================================================
RSX_COPY:
;=======================================================================
	cp 003h		;cdbd	fe 03 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;cdbf	d2 9f fb 	. . .
	cp 002h		;cdc2	fe 02 	. .
	jp c,MSG_WRONG_PARAMETER_AMT		;cdc4	da 97 fb 	. . .
	push ix		;cdc7	dd e5 	. .
	call sub_ceach		;cdc9	cd ac ce 	. . .
	pop ix		;cdcc	dd e1 	. .
	ld l,(ix+002h)		;cdce	dd 6e 02 	. n .
	ld h,(ix+003h)		;cdd1	dd 66 03 	. f .
	ld b,(hl)			;cdd4	46 	F
	inc hl			;cdd5	23 	#
	ld a,(hl)			;cdd6	7e 	~
	inc hl			;cdd7	23 	#
	ld h,(hl)			;cdd8	66 	f
	ld l,a			;cdd9	6f 	o
	ld (0bf20h),hl		;cdda	22 20 bf 	"   .
	ld a,b			;cddd	78 	x
	ld (0bf22h),a		;cdde	32 22 bf 	2 " .
	call sub_da80h		;cde1	cd 80 da 	. . .
	ld (0bf23h),hl		;cde4	22 23 bf 	" # .
	ld a,b			;cde7	78 	x
	ld (0bf25h),a		;cde8	32 25 bf 	2 % .
	and a			;cdeb	a7 	.
	jr z,lce4ah		;cdec	28 5c 	( \
	call sub_da1bh		;cdee	cd 1b da 	. . .
	ex de,hl			;cdf1	eb 	.
	ld ix,0bf10h		;cdf2	dd 21 10 bf 	. ! . .
	call sub_edb6h		;cdf6	cd b6 ed 	. . .
	jp nz,MSG_BAD_FILE_NAME		;cdf9	c2 8e fb 	. . .
	bit 7,e		;cdfc	cb 7b 	. {
	jr z,lce4ah		;cdfe	28 4a 	( J
	ld (iy+014h),003h		;ce00	fd 36 14 03 	. 6 . .
	ld a,(iy+013h)		;ce04	fd 7e 13 	. ~ .
	push af			;ce07	f5 	.
	call sub_da04h		;ce08	cd 04 da 	. . .
	push af			;ce0b	f5 	.
	call sub_d9fdh		;ce0c	cd fd d9 	. . .
	push af			;ce0f	f5 	.
	ld a,(iy+005h)		;ce10	fd 7e 05 	. ~ .
	push af			;ce13	f5 	.
	ld a,(iy+006h)		;ce14	fd 7e 06 	. ~ .
	push af			;ce17	f5 	.
	ld c,(iy+WS_CURRENT_DRIVE_LETTER)		;ce18	fd 4e 03 	. N .
	xor a			;ce1b	af 	.
	push ix		;ce1c	dd e5 	. .
	call CHANGE_DRIVE		;ce1e	cd c9 d7 	. . .
	pop ix		;ce21	dd e1 	. .
	pop af			;ce23	f1 	.
	ld (iy+006h),a		;ce24	fd 77 06 	. w .
	call sub_d9f7h		;ce27	cd f7 d9 	. . .
	pop af			;ce2a	f1 	.
	ld (iy+005h),a		;ce2b	fd 77 05 	. w .
	call sub_d9f0h		;ce2e	cd f0 d9 	. . .
	ld a,(iy+WS_CURRENT_DRIVE_LETTER)		;ce31	fd 7e 03 	. ~ .
	push af			;ce34	f5 	.
	call sub_f798h		;ce35	cd 98 f7 	. . .
	pop af			;ce38	f1 	.
	ld (iy+WS_CURRENT_DRIVE_LETTER),a		;ce39	fd 77 03 	. w .
	pop af			;ce3c	f1 	.
	call sub_d9f0h		;ce3d	cd f0 d9 	. . .
	pop af			;ce40	f1 	.
	call sub_d9f7h		;ce41	cd f7 d9 	. . .
	pop af			;ce44	f1 	.
	ld c,a			;ce45	4f 	O
	xor a			;ce46	af 	.
	jp CHANGE_DRIVE		;ce47	c3 c9 d7 	. . .
lce4ah:
	ld hl,(0bf23h)		;ce4a	2a 23 bf 	* # .
	ld a,(0bf25h)		;ce4d	3a 25 bf 	: % .
	ld b,a			;ce50	47 	G
	call sub_cec5h		;ce51	cd c5 ce 	. . .
	ret nc			;ce54	d0 	.
	ld hl,(0bf20h)		;ce55	2a 20 bf 	*   .
	ld a,(0bf22h)		;ce58	3a 22 bf 	: " .
	ld b,a			;ce5b	47 	G
sub_ce5ch:
	push de			;ce5c	d5 	.
	ld de,08000h		;ce5d	11 00 80 	. . .
	call CAS_OUT_OPEN		;ce60	cd 8c bc 	. . .
	pop de			;ce63	d1 	.
	jr nc,sub_ceach		;ce64	30 46 	0 F
	push hl			;ce66	e5 	.
	pop ix		;ce67	dd e1 	. .
	ld bc,00012h		;ce69	01 12 00 	. . .
	add hl,bc			;ce6c	09 	.
	ex de,hl			;ce6d	eb 	.
	add hl,bc			;ce6e	09 	.
	ld a,(hl)			;ce6f	7e 	~
	ld (ix+012h),a		;ce70	dd 77 12 	. w .
	inc hl			;ce73	23 	#
	ld a,(hl)			;ce74	7e 	~
	ld (ix+018h),a		;ce75	dd 77 18 	. w .
	inc hl			;ce78	23 	#
	ld a,(hl)			;ce79	7e 	~
	ld (ix+019h),a		;ce7a	dd 77 19 	. w .
	inc hl			;ce7d	23 	#
	inc de			;ce7e	13 	.
	inc de			;ce7f	13 	.
	inc de			;ce80	13 	.
	ld bc,00002h		;ce81	01 02 00 	. . .
	ldir		;ce84	ed b0 	. .
	ld bc,00003h		;ce86	01 03 00 	. . .
	add hl,bc			;ce89	09 	.
	inc de			;ce8a	13 	.
	inc de			;ce8b	13 	.
	inc de			;ce8c	13 	.
	dec bc			;ce8d	0b 	.
	ldir		;ce8e	ed b0 	. .
	call CAS_IN_CHAR		;ce90	cd 80 bc 	. . .
	jr nc,sub_ceach		;ce93	30 17 	0 .
	call CAS_OUT_CHAR		;ce95	cd 95 bc 	. . .
	jr nc,sub_ceach		;ce98	30 12 	0 .
	set 2,(iy+041h)		;ce9a	fd cb 41 d6 	. . A .
	set 4,(iy+041h)		;ce9e	fd cb 41 e6 	. . A .
lcea2h:
	call CAS_IN_CHAR		;cea2	cd 80 bc 	. . .
	jr nc,lceb9h		;cea5	30 12 	0 .
lcea7h:
	call CAS_OUT_CHAR		;cea7	cd 95 bc 	. . .
	jr c,lcea2h		;ceaa	38 f6 	8 .
sub_ceach:
	call CAS_OUT_ABANDON		;ceac	cd 92 bc 	. . .
	call CAS_IN_ABANDON		;ceaf	cd 7d bc 	. } .
	res 4,(iy+041h)		;ceb2	fd cb 41 a6 	. . A .
	jp lda0ah		;ceb6	c3 0a da 	. . .
lceb9h:
	cp 01ah		;ceb9	fe 1a 	. .
	jr z,lcea7h		;cebb	28 ea 	( .
	call CAS_OUT_CLOSE		;cebd	cd 8f bc 	. . .
	call sub_ceach		;cec0	cd ac ce 	. . .
	xor a			;cec3	af 	.
	ret			;cec4	c9 	.
sub_cec5h:
	ld de,08800h		;cec5	11 00 88 	. . .
	call CAS_IN_OPEN		;cec8	cd 77 bc 	. w .
	ret nc			;cecb	d0 	.
	ld de,08898h		;cecc	11 98 88 	. . .
	push de			;cecf	d5 	.
	ld bc,RAM_LAM		;ced0	01 20 00 	.   .
	ldir		;ced3	ed b0 	. .
	pop de			;ced5	d1 	.
	ld hl,(088abh)		;ced6	2a ab 88 	* . .
	ld a,h			;ced9	7c 	|
	or l			;ceda	b5 	.
	scf			;cedb	37 	7
	ret nz			;cedc	c0 	.
	ld hl,(088b0h)		;cedd	2a b0 88 	* . .
	ld (088abh),hl		;cee0	22 ab 88 	" . .
	ret			;cee3	c9 	.
sub_cee4h:
	ld (0bf23h),hl		;cee4	22 23 bf 	" # .
	ld a,b			;cee7	78 	x
	ld (0bf25h),a		;cee8	32 25 bf 	2 % .
	ld c,000h		;ceeb	0e 00 	. .
	call sub_d878h		;ceed	cd 78 d8 	. x .
	push hl			;cef0	e5 	.
	pop ix		;cef1	dd e1 	. .
	ld hl,(0bf20h)		;cef3	2a 20 bf 	*   .
	ld a,(0bf22h)		;cef6	3a 22 bf 	: " .
	and a			;cef9	a7 	.
	jr z,lcf07h		;cefa	28 0b 	( .
	ld b,a			;cefc	47 	G
lcefdh:
	ld a,(hl)			;cefd	7e 	~
	ld (ix+000h),a		;cefe	dd 77 00 	. w .
	inc hl			;cf01	23 	#
	inc ix		;cf02	dd 23 	. #
	inc c			;cf04	0c 	.
	djnz lcefdh		;cf05	10 f6 	. .
lcf07h:
	ld hl,(0bf23h)		;cf07	2a 23 bf 	* # .
	ld a,(0bf25h)		;cf0a	3a 25 bf 	: % .
	ld b,a			;cf0d	47 	G
lcf0eh:
	ld a,(hl)			;cf0e	7e 	~
	cp 02eh		;cf0f	fe 2e 	. .
	call z,sub_cf41h		;cf11	cc 41 cf 	. A .
	ld (ix+000h),a		;cf14	dd 77 00 	. w .
	inc hl			;cf17	23 	#
	inc ix		;cf18	dd 23 	. #
	inc c			;cf1a	0c 	.
	djnz lcf0eh		;cf1b	10 f1 	. .
	cp 020h		;cf1d	fe 20 	.
	call z,sub_cf41h		;cf1f	cc 41 cf 	. A .
	ld hl,(0bf23h)		;cf22	2a 23 bf 	* # .
	ld a,(0bf25h)		;cf25	3a 25 bf 	: % .
	ld b,a			;cf28	47 	G
	push bc			;cf29	c5 	.
	call sub_cec5h		;cf2a	cd c5 ce 	. . .
	pop bc			;cf2d	c1 	.
	ld (iy+014h),000h		;cf2e	fd 36 14 00 	. 6 . .
	jp nc,lda0fh		;cf32	d2 0f da 	. . .
	call sub_d878h		;cf35	cd 78 d8 	. x .
	ld b,c			;cf38	41 	A
	call sub_ce5ch		;cf39	cd 5c ce 	. \ .
	ld (iy+014h),003h		;cf3c	fd 36 14 03 	. 6 . .
	ret			;cf40	c9 	.
sub_cf41h:
	ld a,(ix-001h)		;cf41	dd 7e ff 	. ~ .
	cp 020h		;cf44	fe 20 	.
	ld a,02eh		;cf46	3e 2e 	> .
	ret nz			;cf48	c0 	.
	ld a,c			;cf49	79 	y
	and a			;cf4a	a7 	.
	ret z			;cf4b	c8 	.
	dec c			;cf4c	0d 	.
	dec ix		;cf4d	dd 2b 	. +
	jr sub_cf41h		;cf4f	18 f0 	. .

;=======================================================================
RSX_BGET:
;=======================================================================
	cp 002h		;cf51	fe 02 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;cf53	d2 9f fb 	. . .
	and a			;cf56	a7 	.
	jp z,MSG_WRONG_PARAMETER_AMT		;cf57	ca 97 fb 	. . .
	push ix		;cf5a	dd e5 	. .
	call CAS_IN_CHAR		;cf5c	cd 80 bc 	. . .
	pop ix		;cf5f	dd e1 	. .
	ld l,(ix+000h)		;cf61	dd 6e 00 	. n .
	ld h,(ix+001h)		;cf64	dd 66 01 	. f .
	push hl			;cf67	e5 	.
	pop ix		;cf68	dd e1 	. .
	jr c,lcf78h		;cf6a	38 0c 	8 .
	cp 01ah		;cf6c	fe 1a 	. .
	jr z,lcf78h		;cf6e	28 08 	( .
	ld (ix+000h),a		;cf70	dd 77 00 	. w .
	ld (ix+001h),0ffh		;cf73	dd 36 01 ff 	. 6 . .
	ret			;cf77	c9 	.
lcf78h:
	ld (ix+000h),a		;cf78	dd 77 00 	. w .
	ld (ix+001h),000h		;cf7b	dd 36 01 00 	. 6 . .
	ret			;cf7f	c9 	.

;=======================================================================
RSX_BPUT:
;=======================================================================
	cp 002h		;cf80	fe 02 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;cf82	d2 9f fb 	. . .
	and a			;cf85	a7 	.
	jp z,MSG_WRONG_PARAMETER_AMT		;cf86	ca 97 fb 	. . .
	ld a,(ix+000h)		;cf89	dd 7e 00 	. ~ .
	jp CAS_OUT_CHAR		;cf8c	c3 95 bc 	. . .

;=======================================================================
RSX_CAT:
;=======================================================================
	ld (iy+03fh),0ffh		;cf8f	fd 36 3f ff 	. 6 ? .
	ld (iy+040h),0ffh		;cf93	fd 36 40 ff 	. 6 @ .
	and a			;cf97	a7 	.
	jr nz,lcfa3h		;cf98	20 09 	  .
	call CAS_IN_ABANDON		;cf9a	cd 7d bc 	. } .
	call sub_d882h		;cf9d	cd 82 d8 	. . .
	jp CAS_CATALOG		;cfa0	c3 9b bc 	. . .
lcfa3h:
	cp 001h		;cfa3	fe 01 	. .
	jp nz,MSG_WRONG_PARAMETER_AMT		;cfa5	c2 97 fb 	. . .
	call sub_da80h		;cfa8	cd 80 da 	. . .
	ld a,b			;cfab	78 	x
	cp 011h		;cfac	fe 11 	. .
	jp nc,MSG_UNKNOWN_FILE_SYSTEM		;cfae	d2 c6 fb 	. . .
	ld de,0bef0h		;cfb1	11 f0 be 	. . .
lcfb4h:
	ld a,(hl)			;cfb4	7e 	~
	cp 061h		;cfb5	fe 61 	. a
	call nc,FUNC_SUBTRACT_32		;cfb7	d4 9d d9 	. . .
	ld (de),a			;cfba	12 	.
	inc hl			;cfbb	23 	#
	inc de			;cfbc	13 	.
	djnz lcfb4h		;cfbd	10 f5 	. .
	ex de,hl			;cfbf	eb 	.
	ld (hl),02eh		;cfc0	36 2e 	6 .
	inc hl			;cfc2	23 	#
	ld (hl),049h		;cfc3	36 49 	6 I
	inc hl			;cfc5	23 	#
	ld (hl),0ceh		;cfc6	36 ce 	6 .
	ld hl,CAS_IN_OPEN		;cfc8	21 77 bc 	! w .
	ld bc,00015h		;cfcb	01 15 00 	. . .
	ld de,0bca0h		;cfce	11 a0 bc 	. . .
	ldir		;cfd1	ed b0 	. .
	ld hl,CAS_CATALOG		;cfd3	21 9b bc 	! . .
	ld bc,00003h		;cfd6	01 03 00 	. . .
	ldir		;cfd9	ed b0 	. .
	ld hl,0bef0h		;cfdb	21 f0 be 	! . .
	xor a			;cfde	af 	.
	call EXECUTE_RSX_COMMAND		;cfdf	cd e8 c3 	. . .
	jr nc,lcff1h		;cfe2	30 0d 	0 .
	xor a			;cfe4	af 	.
	call KL_FAR_PCHL		;cfe5	cd 1b 00 	. . .
	call sub_d882h		;cfe8	cd 82 d8 	. . .
	call CAS_CATALOG		;cfeb	cd 9b bc 	. . .
	jp lcff7h		;cfee	c3 f7 cf 	. . .
lcff1h:
	call lcff7h		;cff1	cd f7 cf 	. . .
	jp MSG_UNKNOWN_FILE_SYSTEM		;cff4	c3 c6 fb 	. . .
lcff7h:
	ld hl,0bca0h		;cff7	21 a0 bc 	! . .
	ld de,CAS_IN_OPEN		;cffa	11 77 bc 	. w .
	ld bc,00015h		;cffd	01 15 00 	. . .
	ldir		;d000	ed b0 	. .
	ld bc,00003h		;d002	01 03 00 	. . .
	ld de,CAS_CATALOG		;d005	11 9b bc 	. . .
	ldir		;d008	ed b0 	. .
	ret			;d00a	c9 	.
	ld (0bee0h),de		;d00b	ed 53 e0 be 	. S . .
	ld (0bee2h),hl		;d00f	22 e2 be 	" . .
	ld a,b			;d012	78 	x
	ld (0bee4h),a		;d013	32 e4 be 	2 . .
	ld a,(iy+008h)		;d016	fd 7e 08 	. ~ .
	and a			;d019	a7 	.
	call nz,sub_d11fh		;d01a	c4 1f d1 	. . .
	ld a,b			;d01d	78 	x
	cp 002h		;d01e	fe 02 	. .
	jr c,ld028h		;d020	38 06 	8 .
	ld a,(hl)			;d022	7e 	~
	cp 02dh		;d023	fe 2d 	. -
	jp z,le472h		;d025	ca 72 e4 	. r .
ld028h:
	ld a,(iy+05eh)		;d028	fd 7e 5e 	. ~ ^
	ld (CAS_IN_OPEN),a		;d02b	32 77 bc 	2 w .
	ld hl,(0bc78h)		;d02e	2a 78 bc 	* x .
	push hl			;d031	e5 	.
	ld l,(iy+05fh)		;d032	fd 6e 5f 	. n _
	ld h,(iy+060h)		;d035	fd 66 60 	. f `
	ld (0bc78h),hl		;d038	22 78 bc 	" x .
	ld hl,(0bee2h)		;d03b	2a e2 be 	* . .
	call CAS_IN_OPEN		;d03e	cd 77 bc 	. w .
	ld (0bf00h),hl		;d041	22 00 bf 	" . .
	pop hl			;d044	e1 	.
	push af			;d045	f5 	.
	ld (0bc78h),hl		;d046	22 78 bc 	" x .
	ld a,0c3h		;d049	3e c3 	> .
	ld (CAS_IN_OPEN),a		;d04b	32 77 bc 	2 w .
	ld hl,(0bf00h)		;d04e	2a 00 bf 	* . .
	pop af			;d051	f1 	.
	ret			;d052	c9 	.
	ld (0bee0h),de		;d053	ed 53 e0 be 	. S . .
	ld (0bee2h),hl		;d057	22 e2 be 	" . .
	ld a,b			;d05a	78 	x
	ld (0bee4h),a		;d05b	32 e4 be 	2 . .
	ld a,(iy+008h)		;d05e	fd 7e 08 	. ~ .
	and a			;d061	a7 	.
	call nz,sub_d11ah		;d062	c4 1a d1 	. . .
	ld a,b			;d065	78 	x
	cp 002h		;d066	fe 02 	. .
	jr c,ld070h		;d068	38 06 	8 .
	ld a,(hl)			;d06a	7e 	~
	cp 02dh		;d06b	fe 2d 	. -
	jp z,le80dh		;d06d	ca 0d e8 	. . .
ld070h:
	ld a,(iy+061h)		;d070	fd 7e 61 	. ~ a
	ld (CAS_OUT_OPEN),a		;d073	32 8c bc 	2 . .
	ld hl,(0bc8dh)		;d076	2a 8d bc 	* . .
	push hl			;d079	e5 	.
	ld l,(iy+062h)		;d07a	fd 6e 62 	. n b
	ld h,(iy+063h)		;d07d	fd 66 63 	. f c
	ld (0bc8dh),hl		;d080	22 8d bc 	" . .
	ld hl,(0bee2h)		;d083	2a e2 be 	* . .
	call CAS_OUT_OPEN		;d086	cd 8c bc 	. . .
	ld (0bf00h),hl		;d089	22 00 bf 	" . .
	pop hl			;d08c	e1 	.
	push af			;d08d	f5 	.
	ld (0bc8dh),hl		;d08e	22 8d bc 	" . .
	ld a,0c3h		;d091	3e c3 	> .
	ld (CAS_OUT_OPEN),a		;d093	32 8c bc 	2 . .
	ld hl,(0bf00h)		;d096	2a 00 bf 	* . .
	pop af			;d099	f1 	.
	ret			;d09a	c9 	.

;=======================================================================
RSX_FS:
;=======================================================================
	ld ix,ld116h		;d09b	dd 21 16 d1 	. ! . .
	ld de,0008dh		;d09f	11 8d 00 	. . .
	push iy		;d0a2	fd e5 	. .
	pop hl			;d0a4	e1 	.
	add hl,de			;d0a5	19 	.
	ld de,0bf09h		;d0a6	11 09 bf 	. . .
	ld b,002h		;d0a9	06 02 	. .
	call MAKE_JP_AT_DE_USING_HL		;d0ab	cd 74 de 	. t .
	ld hl,(0bc78h)		;d0ae	2a 78 bc 	* x .
	ld a,(0bf0ah)		;d0b1	3a 0a bf 	: . .
	cp l			;d0b4	bd 	.
	jr nz,ld0bdh		;d0b5	20 06 	  .
	ld a,(0bf0bh)		;d0b7	3a 0b bf 	: . .
	cp h			;d0ba	bc 	.
	jr z,ld0e2h		;d0bb	28 25 	( %
ld0bdh:
	ld (iy+05fh),l		;d0bd	fd 75 5f 	. u _
	ld (iy+060h),h		;d0c0	fd 74 60 	. t `
	ld a,(CAS_IN_OPEN)		;d0c3	3a 77 bc 	: w .
	ld (iy+05eh),a		;d0c6	fd 77 5e 	. w ^
	ld hl,(0bf0ah)		;d0c9	2a 0a bf 	* . .
	ld (0bc78h),hl		;d0cc	22 78 bc 	" x .
	ld (iy+04ah),l		;d0cf	fd 75 4a 	. u J
	ld (iy+04bh),h		;d0d2	fd 74 4b 	. t K
	ld a,(0bf09h)		;d0d5	3a 09 bf 	: . .
	ld (CAS_IN_OPEN),a		;d0d8	32 77 bc 	2 w .
	xor a			;d0db	af 	.
	ld (iy+017h),a		;d0dc	fd 77 17 	. w .
	ld (iy+018h),a		;d0df	fd 77 18 	. w .
ld0e2h:
	ld hl,(0bc8dh)		;d0e2	2a 8d bc 	* . .
	ld a,(0bf0dh)		;d0e5	3a 0d bf 	: . .
	cp l			;d0e8	bd 	.
	jr nz,ld0f0h		;d0e9	20 05 	  .
	ld a,(0bf0eh)		;d0eb	3a 0e bf 	: . .
	cp h			;d0ee	bc 	.
	ret z			;d0ef	c8 	.
ld0f0h:
	ld a,(CAS_OUT_OPEN)		;d0f0	3a 8c bc 	: . .
	ld (iy+061h),a		;d0f3	fd 77 61 	. w a
	ld (iy+062h),l		;d0f6	fd 75 62 	. u b
	ld (iy+063h),h		;d0f9	fd 74 63 	. t c
	ld hl,(0bf0dh)		;d0fc	2a 0d bf 	* . .
	ld (0bc8dh),hl		;d0ff	22 8d bc 	" . .
	ld (iy+04ch),l		;d102	fd 75 4c 	. u L
	ld (iy+04dh),h		;d105	fd 74 4d 	. t M
	ld a,(0bf0ch)		;d108	3a 0c bf 	: . .
	ld (CAS_OUT_OPEN),a		;d10b	32 8c bc 	2 . .
	xor a			;d10e	af 	.
	ld (iy+01bh),a		;d10f	fd 77 1b 	. w .
	ld (iy+01ch),a		;d112	fd 77 1c 	. w .
	ret			;d115	c9 	.
ld116h:
	dec bc			;d116	0b 	.
	ret nc			;d117	d0 	.
	ld d,e			;d118	53 	S
	ret nc			;d119	d0 	.
sub_d11ah:
	ld hl,STR_LOADING_end		;d11a	21 55 d1 	! U .
	jr ld122h		;d11d	18 03 	. .
sub_d11fh:
	ld hl,STR_LOADING_start		;d11f	21 4c d1 	! L .
ld122h:
	call DISPLAY_MSG		;d122	cd 6a d9 	. j .
	ld hl,(0bee2h)		;d125	2a e2 be 	* . .
	ld a,(0bee4h)		;d128	3a e4 be 	: . .
	ld b,a			;d12b	47 	G
	ld a,022h		;d12c	3e 22 	> "
	call TXT_OUTPUT		;d12e	cd 5a bb 	. Z .
	call sub_d13ch		;d131	cd 3c d1 	. < .
	ld a,022h		;d134	3e 22 	> "
	call TXT_OUTPUT		;d136	cd 5a bb 	. Z .
	jp PRINT_CR_LF		;d139	c3 7d d9 	. } .
sub_d13ch:
	push hl			;d13c	e5 	.
	push bc			;d13d	c5 	.
	ld a,b			;d13e	78 	x
	and a			;d13f	a7 	.
	jr z,ld149h		;d140	28 07 	( .
ld142h:
	ld a,(hl)			;d142	7e 	~
	call TXT_OUTPUT		;d143	cd 5a bb 	. Z .
	inc hl			;d146	23 	#
	djnz ld142h		;d147	10 f9 	. .
ld149h:
	pop bc			;d149	c1 	.
	pop hl			;d14a	e1 	.
	ret			;d14b	c9 	.

; BLOCK 'STR_LOADING' (start 0xd14c end 0xd155)
STR_LOADING_start:
	defb 'Loading ',0
STR_LOADING_end:

; BLOCK 'STR_SAVING' (start 0xd155 end 0xd15d)
STR_SAVING_start:
	defb 'Saving ',0
STR_SAVING_end:

;=======================================================================
RSX_INFO:
;=======================================================================
	cp 005h		;d15d	fe 05 	. .
	jp z,ld1cfh		;d15f	ca cf d1 	. . .
	cp 002h		;d162	fe 02 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;d164	d2 9f fb 	. . .
	and a			;d167	a7 	.
	jr z,ld198h		;d168	28 2e 	( .
	call sub_da80h		;d16a	cd 80 da 	. . .
	ld (0bee0h),hl		;d16d	22 e0 be 	" . .
	ld a,b			;d170	78 	x
	ld (0bee2h),a		;d171	32 e2 be 	2 . .
	and a			;d174	a7 	.
	jp z,ld1a4h		;d175	ca a4 d1 	. . .
	call sub_da1bh		;d178	cd 1b da 	. . .
	ex de,hl			;d17b	eb 	.
ld17ch:
	ld ix,0bed0h		;d17c	dd 21 d0 be 	. ! . .
	call sub_edb6h		;d180	cd b6 ed 	. . .
	ret nz			;d183	c0 	.
	bit 7,e		;d184	cb 7b 	. {
	jr z,ld1a4h		;d186	28 1c 	( .
	ld (iy+014h),001h		;d188	fd 36 14 01 	. 6 . .
	call sub_f798h		;d18c	cd 98 f7 	. . .
	ret nc			;d18f	d0 	.
	bit 3,(iy+041h)		;d190	fd cb 41 5e 	. . A ^
	jp z,MSG_NO_MATCH		;d194	ca ab fb 	. . .
	ret			;d197	c9 	.
ld198h:
	ld hl,0bed0h		;d198	21 d0 be 	! . .
	ld (hl),02ah		;d19b	36 2a 	6 *
	ld b,001h		;d19d	06 01 	. .
	call sub_da62h		;d19f	cd 62 da 	. b .
	jr ld17ch		;d1a2	18 d8 	. .
ld1a4h:
	ld hl,(0bee0h)		;d1a4	2a e0 be 	* . .
	ld a,(0bee2h)		;d1a7	3a e2 be 	: . .
	ld b,a			;d1aa	47 	G
sub_d1abh:
	call sub_d882h		;d1ab	cd 82 d8 	. . .
	push bc			;d1ae	c5 	.
	push hl			;d1af	e5 	.
	call CAS_IN_OPEN		;d1b0	cd 77 bc 	. w .
	push hl			;d1b3	e5 	.
	pop ix		;d1b4	dd e1 	. .
	pop hl			;d1b6	e1 	.
	pop bc			;d1b7	c1 	.
	jp nc,lda0fh		;d1b8	d2 0f da 	. . .
	call sub_d13ch		;d1bb	cd 3c d1 	. < .
	call sub_d2c3h		;d1be	cd c3 d2 	. . .
	jp CAS_IN_CLOSE		;d1c1	c3 7a bc 	. z .
sub_d1c4h:
	ld l,(ix+000h)		;d1c4	dd 6e 00 	. n .
	ld h,(ix+001h)		;d1c7	dd 66 01 	. f .
	inc ix		;d1ca	dd 23 	. #
	inc ix		;d1cc	dd 23 	. #
	ret			;d1ce	c9 	.
ld1cfh:
	ld l,(ix+008h)		;d1cf	dd 6e 08 	. n .
	ld h,(ix+009h)		;d1d2	dd 66 09 	. f .
	ld b,(hl)			;d1d5	46 	F
	inc hl			;d1d6	23 	#
	ld a,(hl)			;d1d7	7e 	~
	inc hl			;d1d8	23 	#
	ld h,(hl)			;d1d9	66 	f
	ld l,a			;d1da	6f 	o
	push ix		;d1db	dd e5 	. .
	call sub_d882h		;d1dd	cd 82 d8 	. . .
	call CAS_IN_OPEN		;d1e0	cd 77 bc 	. w .
	pop ix		;d1e3	dd e1 	. .
	ret nc			;d1e5	d0 	.
	push hl			;d1e6	e5 	.
	pop iy		;d1e7	fd e1 	. .
	ld a,(iy+012h)		;d1e9	fd 7e 12 	. ~ .
	call sub_d1c4h		;d1ec	cd c4 d1 	. . .
	ld a,(iy+01ah)		;d1ef	fd 7e 1a 	. ~ .
	ld (hl),a			;d1f2	77 	w
	inc hl			;d1f3	23 	#
	ld a,(iy+01bh)		;d1f4	fd 7e 1b 	. ~ .
	ld (hl),a			;d1f7	77 	w
	call sub_d1c4h		;d1f8	cd c4 d1 	. . .
	ld e,(iy+013h)		;d1fb	fd 5e 13 	. ^ .
	ld d,(iy+014h)		;d1fe	fd 56 14 	. V .
	ld a,e			;d201	7b 	{
	or d			;d202	b2 	.
	call z,sub_d220h		;d203	cc 20 d2 	.   .
	ld (hl),e			;d206	73 	s
	inc hl			;d207	23 	#
	ld (hl),d			;d208	72 	r
	call sub_d1c4h		;d209	cd c4 d1 	. . .
	ld a,(iy+015h)		;d20c	fd 7e 15 	. ~ .
	ld (hl),a			;d20f	77 	w
	inc hl			;d210	23 	#
	ld a,(iy+016h)		;d211	fd 7e 16 	. ~ .
	ld (hl),a			;d214	77 	w
	call sub_d1c4h		;d215	cd c4 d1 	. . .
	ld a,(iy+012h)		;d218	fd 7e 12 	. ~ .
	ld (hl),a			;d21b	77 	w
	inc hl			;d21c	23 	#
	ld (hl),000h		;d21d	36 00 	6 .
	ret			;d21f	c9 	.
sub_d220h:
	ld e,(iy+018h)		;d220	fd 5e 18 	. ^ .
	ld d,(iy+019h)		;d223	fd 56 19 	. V .
	ret			;d226	c9 	.

;=======================================================================
RSX_LIST:
;=======================================================================
	cp 002h		;d227	fe 02 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;d229	d2 9f fb 	. . .
	ld b,a			;d22c	47 	G
	and a			;d22d	a7 	.
	jr z,ld233h		;d22e	28 03 	( .
	call sub_da80h		;d230	cd 80 da 	. . .
ld233h:
	call sub_d882h		;d233	cd 82 d8 	. . .
	call CAS_IN_OPEN		;d236	cd 77 bc 	. w .
	ret nc			;d239	d0 	.
ld23ah:
	call CAS_IN_CHAR		;d23a	cd 80 bc 	. . .
	jr nc,ld247h		;d23d	30 08 	0 .
	call TXT_OUTPUT		;d23f	cd 5a bb 	. Z .
	call sub_c5ffh		;d242	cd ff c5 	. . .
	jr z,ld23ah		;d245	28 f3 	( .
ld247h:
	call CAS_IN_CLOSE		;d247	cd 7a bc 	. z .
	jp PRINT_CR_LF		;d24a	c3 7d d9 	. } .

;=======================================================================
RSX_DUMP:
;=======================================================================
	cp 003h		;d24d	fe 03 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;d24f	d2 9f fb 	. . .
	ld hl,RESET_ENTRY_RST_0		;d252	21 00 00 	! . .
	cp 002h		;d255	fe 02 	. .
	call z,sub_d1c4h		;d257	cc c4 d1 	. . .
	ld (0bef8h),hl		;d25a	22 f8 be 	" . .
	ld b,a			;d25d	47 	G
	and a			;d25e	a7 	.
	jr z,ld265h		;d25f	28 04 	( .
	call sub_PREP_STRING_PARAM		;d261	cd 85 da 	. . .
	ex de,hl			;d264	eb 	.
ld265h:
	call sub_d882h		;d265	cd 82 d8 	. . .
	call CAS_IN_OPEN		;d268	cd 77 bc 	. w .
	ret nc			;d26b	d0 	.
	ld hl,(0bef8h)		;d26c	2a f8 be 	* . .
ld26fh:
	ld a,h			;d26f	7c 	|
	call sub_d93ah		;d270	cd 3a d9 	. : .
	ld a,l			;d273	7d 	}
	call sub_d93ah		;d274	cd 3a d9 	. : .
	ld a,020h		;d277	3e 20 	>
	call TXT_OUTPUT		;d279	cd 5a bb 	. Z .
	ld b,010h		;d27c	06 10 	. .
	ld de,0bf10h		;d27e	11 10 bf 	. . .
ld281h:
	call CAS_IN_CHAR		;d281	cd 80 bc 	. . .
	jr nc,ld2b6h		;d284	30 30 	0 0
ld286h:
	ld (de),a			;d286	12 	.
	inc de			;d287	13 	.
	call sub_d93ah		;d288	cd 3a d9 	. : .
	inc hl			;d28b	23 	#
	ld a,020h		;d28c	3e 20 	>
	call TXT_OUTPUT		;d28e	cd 5a bb 	. Z .
	djnz ld281h		;d291	10 ee 	. .
	call TXT_OUTPUT		;d293	cd 5a bb 	. Z .
	ld de,0bf10h		;d296	11 10 bf 	. . .
	ld b,010h		;d299	06 10 	. .
ld29bh:
	ld a,(de)			;d29b	1a 	.
	inc de			;d29c	13 	.
	and 07fh		;d29d	e6 7f 	. 
	cp 020h		;d29f	fe 20 	.
	call c,sub_d2c0h		;d2a1	dc c0 d2 	. . .
	cp 060h		;d2a4	fe 60 	. `
	call z,sub_d2c0h		;d2a6	cc c0 d2 	. . .
	call TXT_OUTPUT		;d2a9	cd 5a bb 	. Z .
	djnz ld29bh		;d2ac	10 ed 	. .
	call PRINT_CR_LF		;d2ae	cd 7d d9 	. } .
	call sub_c5ffh		;d2b1	cd ff c5 	. . .
	jr z,ld26fh		;d2b4	28 b9 	( .
ld2b6h:
	cp 01ah		;d2b6	fe 1a 	. .
	jr z,ld286h		;d2b8	28 cc 	( .
	call CAS_IN_CLOSE		;d2ba	cd 7a bc 	. z .
	jp PRINT_CR_LF		;d2bd	c3 7d d9 	. } .
sub_d2c0h:
	ld a,02eh		;d2c0	3e 2e 	> .
	ret			;d2c2	c9 	.
sub_d2c3h:
	ld a,020h		;d2c3	3e 20 	>
	call TXT_OUTPUT		;d2c5	cd 5a bb 	. Z .
	ld a,(ix+012h)		;d2c8	dd 7e 12 	. ~ .
	and 00fh		;d2cb	e6 0f 	. .
	add a,024h		;d2cd	c6 24 	. $
	call TXT_OUTPUT		;d2cf	cd 5a bb 	. Z .
	ld a,020h		;d2d2	3e 20 	>
	call TXT_OUTPUT		;d2d4	cd 5a bb 	. Z .
	ld a,(ix+016h)		;d2d7	dd 7e 16 	. ~ .
	call sub_d93ah		;d2da	cd 3a d9 	. : .
	ld a,(ix+015h)		;d2dd	dd 7e 15 	. ~ .
	call sub_d93ah		;d2e0	cd 3a d9 	. : .
	ld a,020h		;d2e3	3e 20 	>
	call TXT_OUTPUT		;d2e5	cd 5a bb 	. Z .
	ld a,02bh		;d2e8	3e 2b 	> +
	call TXT_OUTPUT		;d2ea	cd 5a bb 	. Z .
	ld l,(ix+013h)		;d2ed	dd 6e 13 	. n .
	ld h,(ix+014h)		;d2f0	dd 66 14 	. f .
	ld a,h			;d2f3	7c 	|
	or l			;d2f4	b5 	.
	jr nz,ld2fdh		;d2f5	20 06 	  .
	ld l,(ix+018h)		;d2f7	dd 6e 18 	. n .
	ld h,(ix+019h)		;d2fa	dd 66 19 	. f .
ld2fdh:
	ld a,h			;d2fd	7c 	|
	call sub_d93ah		;d2fe	cd 3a d9 	. : .
	ld a,l			;d301	7d 	}
	call sub_d93ah		;d302	cd 3a d9 	. : .
	ld a,020h		;d305	3e 20 	>
	call TXT_OUTPUT		;d307	cd 5a bb 	. Z .
	call TXT_OUTPUT		;d30a	cd 5a bb 	. Z .
	ld a,078h		;d30d	3e 78 	> x
	call TXT_OUTPUT		;d30f	cd 5a bb 	. Z .
	ld a,(ix+01bh)		;d312	dd 7e 1b 	. ~ .
	call sub_d93ah		;d315	cd 3a d9 	. : .
	ld a,(ix+01ah)		;d318	dd 7e 1a 	. ~ .
	call sub_d93ah		;d31b	cd 3a d9 	. : .
	ld a,(ix+012h)		;d31e	dd 7e 12 	. ~ .
	and 001h		;d321	e6 01 	. .
	jp z,PRINT_CR_LF		;d323	ca 7d d9 	. } .
	ld a,020h		;d326	3e 20 	>
	call TXT_OUTPUT		;d328	cd 5a bb 	. Z .
	call TXT_OUTPUT		;d32b	cd 5a bb 	. Z .
	ld a,050h		;d32e	3e 50 	> P
	call TXT_OUTPUT		;d330	cd 5a bb 	. Z .
	jp PRINT_CR_LF		;d333	c3 7d d9 	. } .

;=======================================================================
RSX_TITLE:
;=======================================================================
	cp 002h		;d336	fe 02 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;d338	d2 9f fb 	. . .
	and a			;d33b	a7 	.
	call z,PROMPT_ENTER_NAME		;d33c	cc dc d8 	. . .
	call sub_da6ah		;d33f	cd 6a da 	. j .
	ld a,b			;d342	78 	x
	cp 011h		;d343	fe 11 	. .
	jp nc,MSG_BAD_FILE_NAME		;d345	d2 8e fb 	. . .
	push hl			;d348	e5 	.
	push bc			;d349	c5 	.
	call sub_ef86h		;d34a	cd 86 ef 	. . .
	pop bc			;d34d	c1 	.
	pop de			;d34e	d1 	.
	ret nz			;d34f	c0 	.
	inc hl			;d350	23 	#
	inc hl			;d351	23 	#
	push bc			;d352	c5 	.
	push hl			;d353	e5 	.
	ld b,010h		;d354	06 10 	. .
ld356h:
	ld (hl),020h		;d356	36 20 	6
	inc hl			;d358	23 	#
	djnz ld356h		;d359	10 fb 	. .
	pop hl			;d35b	e1 	.
	pop bc			;d35c	c1 	.
ld35dh:
	ld a,b			;d35d	78 	x
	and a			;d35e	a7 	.
	jr z,ld368h		;d35f	28 07 	( .
	ld a,(de)			;d361	1a 	.
	ld (hl),a			;d362	77 	w
	inc hl			;d363	23 	#
	inc de			;d364	13 	.
	dec b			;d365	05 	.
	jr ld35dh		;d366	18 f5 	. .
ld368h:
	call sub_efech		;d368	cd ec ef 	. . .
	ld (iy+040h),0ffh		;d36b	fd 36 40 ff 	. 6 @ .
	ld (iy+03fh),0ffh		;d36f	fd 36 3f ff 	. 6 ? .
	ret			;d373	c9 	.

;=======================================================================
RSX_OPT:
;=======================================================================
	cp 003h		;d374	fe 03 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;d376	d2 9f fb 	. . .
	cp 002h		;d379	fe 02 	. .
	jp nz,MSG_WRONG_PARAMETER_AMT		;d37b	c2 97 fb 	. . .
	ld a,(ix+002h)		;d37e	dd 7e 02 	. ~ .
OPT_1:
;Loading messages off/on
	cp 001h		;d381	fe 01 	. .
	jr nz,OPT_2		;d383	20 07 	  .
	ld a,(ix+000h)		;d385	dd 7e 00 	. ~ .
	ld (iy+008h),a		;d388	fd 77 08 	. w .
	ret			;d38b	c9 	.
OPT_2:
;Case sensitivity of filesnames off/on
	cp 002h		;d38c	fe 02 	. .
	jr nz,OPT_3		;d38e	20 12 	  .
	ld a,(ix+000h)		;d390	dd 7e 00 	. ~ .
	xor 001h		;d393	ee 01 	. .
	and 001h		;d395	e6 01 	. .
	ld c,a			;d397	4f 	O
	ld a,(iy+041h)		;d398	fd 7e 41 	. ~ A
	and 0feh		;d39b	e6 fe 	. .
	or c			;d39d	b1 	.
	ld (iy+041h),a		;d39e	fd 77 41 	. w A
	ret			;d3a1	c9 	.
OPT_3:
;Cassette loading messages off/on
	cp 003h		;d3a2	fe 03 	. .
	jr nz,OPT_4		;d3a4	20 0a 	  .
	ld a,(ix+000h)		;d3a6	dd 7e 00 	. ~ .
	and 001h		;d3a9	e6 01 	. .
	xor 001h		;d3ab	ee 01 	. .
	jp CAS_NOISY		;d3ad	c3 6b bc 	. k .
OPT_4:
;overwrite file: 0=ask 1=overwrite 2=create backup
	cp 004h		;d3b0	fe 04 	. .
	jr nz,OPT_5		;d3b2	20 07 	  .
	ld a,(ix+000h)		;d3b4	dd 7e 00 	. ~ .
	ld (iy+016h),a		;d3b7	fd 77 16 	. w .
	ret			;d3ba	c9 	.
OPT_5:
;disk read error retry count (default=16)
	cp 005h		;d3bb	fe 05 	. .
	jr nz,OPT_6		;d3bd	20 0a 	  .
	ld a,(ix+000h)		;d3bf	dd 7e 00 	. ~ .
	ld (iy+009h),a		;d3c2	fd 77 09 	. w .
	ld (0be66h),a		;d3c5	32 66 be 	2 f .
	ret			;d3c8	c9 	.
OPT_6:
;disk error message (off/on)
	cp 006h		;d3c9	fe 06 	. .
	jr nz,OPT_7		;d3cb	20 07 	  .
	ld a,(ix+000h)		;d3cd	dd 7e 00 	. ~ .
	ld (DISK_ERROR_MESSAGE_FLAG),a		;d3d0	32 78 be 	2 x .
	ret			;d3d3	c9 	.
OPT_7:
;motor on time in 1/50sec (default=1 second)
	cp 007h		;d3d4	fe 07 	. .
	jr nz,OPT_8		;d3d6	20 0d 	  .
	ld a,(ix+000h)		;d3d8	dd 7e 00 	. ~ .
	ld (0be44h),a		;d3db	32 44 be 	2 D .
	ld a,(ix+001h)		;d3de	dd 7e 01 	. ~ .
	ld (0be45h),a		;d3e1	32 45 be 	2 E .
	ret			;d3e4	c9 	.
OPT_8:
;motor off time in 1/50sec (default=7 second)
	cp 008h		;d3e5	fe 08 	. .
	jr nz,OPT_9		;d3e7	20 0d 	  .
	ld a,(ix+000h)		;d3e9	dd 7e 00 	. ~ .
	ld (0be46h),a		;d3ec	32 46 be 	2 F .
	ld a,(ix+001h)		;d3ef	dd 7e 01 	. ~ .
	ld (0be47h),a		;d3f2	32 47 be 	2 G .
	ret			;d3f5	c9 	.
OPT_9:
;drive tracking speed in ms (default=12)
	cp 009h		;d3f6	fe 09 	. .
	jr nz,OPT_10		;d3f8	20 0a 	  .
	ld hl,0be4ah		;d3fa	21 4a be 	! J .
UPDATE_DRIVE_PARAM:
;See https://www.cpcwiki.eu/index.php/AMSDOS_Memory_Map for details
;Called from several areas with HL=parameter to be changed
	ld a,(ix+000h)		;d3fd	dd 7e 00 	. ~ .
	ld (hl),a			;d400	77 	w
	jp sub_cb6dh		;d401	c3 6d cb 	. m .
OPT_10:
;Head load delay in ms (default=1)
	cp 00ah		;d404	fe 0a 	. .
	jr nz,OPT_11		;d406	20 00 	  .
OPT_11:
;Head unload delay in ms (default=1)
	cp 00bh		;d408	fe 0b 	. .
	jr nz,OPT_12		;d40a	20 05 	  .
	ld hl,0be48h		;d40c	21 48 be 	! H .
	jr UPDATE_DRIVE_PARAM		;d40f	18 ec 	. .
OPT_12:
;Extra external disk drives port number
	cp 00ch		;d411	fe 0c 	. .
	jr nz,OPT_13		;d413	20 0d 	  .
	ld a,(ix+000h)		;d415	dd 7e 00 	. ~ .
	ld (iy+058h),a		;d418	fd 77 58 	. w X
	ld a,(ix+001h)		;d41b	dd 7e 01 	. ~ .
	ld (iy+059h),a		;d41e	fd 77 59 	. w Y
	ret			;d421	c9 	.
OPT_13:
;Enable 40 track disk to be read in 80 track drive in double step (off/on)
	cp 00dh		;d422	fe 0d 	. .
	jr nz,OPT_14		;d424	20 12 	  .
	ld a,(ix+000h)		;d426	dd 7e 00 	. ~ .
	sla a		;d429	cb 27 	. '
	and 002h		;d42b	e6 02 	. .
	ld c,a			;d42d	4f 	O
	ld a,(iy+041h)		;d42e	fd 7e 41 	. ~ A
	and 0fdh		;d431	e6 fd 	. .
	or c			;d433	b1 	.
	ld (iy+041h),a		;d434	fd 77 41 	. w A
	ret			;d437	c9 	.
OPT_14:
;Head settle time in ms (default 15)
	cp 00eh		;d438	fe 0e 	. .
	jp nz,MSG_WRONG_PARAMETER_AMT		;d43a	c2 97 fb 	. . .
	ld hl,0be49h		;d43d	21 49 be 	! I .
	jr UPDATE_DRIVE_PARAM		;d440	18 bb 	. .

;=======================================================================
RSX_CLS:
;Clear screen to Mode 2, with white text (ink 13) on black paper
;=======================================================================
;Takes no arguments, so error if a>0
	and a			;d442	a7 	.
	jp nz,MSG_TOO_MANY_PARAMETERS		;d443	c2 9f fb 	. . .
;otherwise print the CLS string
	jp DO_CLS		;d446	c3 8d d8 	. . .

;=======================================================================
RSX_READSECT:
;=======================================================================
	cp 004h		;d449	fe 04 	. .
	jp nz,MSG_WRONG_PARAMETER_AMT		;d44b	c2 97 fb 	. . .
	ld hl,0be85h		;d44e	21 85 be 	! . .
	ld (hl),084h		;d451	36 84 	6 .
	ld (0be83h),hl		;d453	22 83 be 	" . .
	call sub_d4abh		;d456	cd ab d4 	. . .
	call sub_d48ch		;d459	cd 8c d4 	. . .
	ret			;d45c	c9 	.

;=======================================================================
RSX_WRITESECT:
;=======================================================================
	cp 004h		;d45d	fe 04 	. .
	jp nz,MSG_WRONG_PARAMETER_AMT		;d45f	c2 97 fb 	. . .
	ld hl,0be85h		;d462	21 85 be 	! . .
	ld (hl),085h		;d465	36 85 	6 .
	ld (0be83h),hl		;d467	22 83 be 	" . .
	call sub_d4abh		;d46a	cd ab d4 	. . .
	ld a,h			;d46d	7c 	|
	and 040h		;d46e	e6 40 	. @
	jp z,sub_d48ch		;d470	ca 8c d4 	. . .
	push de			;d473	d5 	.
	push bc			;d474	c5 	.
	push hl			;d475	e5 	.
	push iy		;d476	fd e5 	. .
	pop hl			;d478	e1 	.
	ld de,002ebh		;d479	11 eb 02 	. . .
	add hl,de			;d47c	19 	.
	ex de,hl			;d47d	eb 	.
	pop hl			;d47e	e1 	.
	push de			;d47f	d5 	.
	ld bc,00200h		;d480	01 00 02 	. . .
	call KL_LDIR		;d483	cd 1b b9 	. . .
	pop hl			;d486	e1 	.
	pop bc			;d487	c1 	.
	pop de			;d488	d1 	.
	jp sub_d48ch		;d489	c3 8c d4 	. . .
sub_d48ch:
	push af			;d48c	f5 	.
	push de			;d48d	d5 	.
	push hl			;d48e	e5 	.
	push bc			;d48f	c5 	.
	ld hl,(0be83h)		;d490	2a 83 be 	* . .
	call KL_FIND_COMMAND		;d493	cd d4 bc 	. . .
	ld (0be80h),hl		;d496	22 80 be 	" . .
	ld a,c			;d499	79 	y
	ld (0be82h),a		;d49a	32 82 be 	2 . .
	pop bc			;d49d	c1 	.
	pop hl			;d49e	e1 	.
	pop de			;d49f	d1 	.
	jr nc,ld4a7h		;d4a0	30 05 	0 .
	pop af			;d4a2	f1 	.
	rst 18h			;d4a3	df 	.
	add a,b			;d4a4	80 	.
	cp (hl)			;d4a5	be 	.
	ret			;d4a6	c9 	.
ld4a7h:
	pop af			;d4a7	f1 	.
	jp MSG_CANT_FIND_AMSDOS		;d4a8	c3 b6 fb 	. . .
sub_d4abh:
	ld c,(ix+000h)		;d4ab	dd 4e 00 	. N .
	ld a,(ix+001h)		;d4ae	dd 7e 01 	. ~ .
	and 001h		;d4b1	e6 01 	. .
	ld (iy+012h),a		;d4b3	fd 77 12 	. w .
	ld d,(ix+002h)		;d4b6	dd 56 02 	. V .
	ld e,(ix+004h)		;d4b9	dd 5e 04 	. ^ .
	ld l,(ix+006h)		;d4bc	dd 6e 06 	. n .
	ld h,(ix+007h)		;d4bf	dd 66 07 	. f .
	ld (iy+WS_CURRENT_DRIVE_LETTER),008h		;d4c2	fd 36 03 08 	. 6 . .
	ld a,e			;d4c6	7b 	{
	ld (0be08h),a		;d4c7	32 08 be 	2 . .
	ld (iy+WS_DRIVE_NUMBER),a		;d4ca	fd 77 04 	. w .
	ret			;d4cd	c9 	.

;=======================================================================
RSX_DIR:
;=======================================================================
	cp 002h		;d4ce	fe 02 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;d4d0	d2 9f fb 	. . .
	push af			;d4d3	f5 	.
	call sub_da62h		;d4d4	cd 62 da 	. b .
	call sub_e374h		;d4d7	cd 74 e3 	. t .
	pop af			;d4da	f1 	.
	push af			;d4db	f5 	.
	and a			;d4dc	a7 	.
	call nz,sub_da1bh		;d4dd	c4 1b da 	. . .
	jp nz,ld51ah		;d4e0	c2 1a d5 	. . .
	call sub_d9a0h		;d4e3	cd a0 d9 	. . .
	jp nc,ld51ah		;d4e6	d2 1a d5 	. . .
	call nz,MSG_DISC_NOT_FORMATTED		;d4e9	c4 22 fc 	. " .
	jp nz,ld51ah		;d4ec	c2 1a d5 	. . .
	cp 081h		;d4ef	fe 81 	. .
	jr z,ld51ch		;d4f1	28 29 	( )
	ld hl,0bef8h		;d4f3	21 f8 be 	! . .
	ld a,(iy+001h)		;d4f6	fd 7e 01 	. ~ .
	ld (hl),a			;d4f9	77 	w
	inc hl			;d4fa	23 	#
	ld (hl),044h		;d4fb	36 44 	6 D
	inc hl			;d4fd	23 	#
	ld (hl),049h		;d4fe	36 49 	6 I
	inc hl			;d500	23 	#
	ld (hl),0d2h		;d501	36 d2 	6 .
	ld hl,0bef8h		;d503	21 f8 be 	! . .
	pop af			;d506	f1 	.
	and a			;d507	a7 	.
	jp z,ld68ch		;d508	ca 8c d6 	. . .
	call sub_da80h		;d50b	cd 80 da 	. . .
	call sub_dabah		;d50e	cd ba da 	. . .
	ret nz			;d511	c0 	.
	ld hl,0bef8h		;d512	21 f8 be 	! . .
	ld a,001h		;d515	3e 01 	> .
	jp ld68ch		;d517	c3 8c d6 	. . .
ld51ah:
	pop af			;d51a	f1 	.
	ret			;d51b	c9 	.
ld51ch:
	call sub_d9fdh		;d51c	cd fd d9 	. . .
	ld (iy+005h),a		;d51f	fd 77 05 	. w .
	call sub_da04h		;d522	cd 04 da 	. . .
	ld (iy+006h),a		;d525	fd 77 06 	. w .
	pop af			;d528	f1 	.
	and a			;d529	a7 	.
	jp z,le1d7h		;d52a	ca d7 e1 	. . .
	call sub_da1bh		;d52d	cd 1b da 	. . .
	ld a,b			;d530	78 	x
	and a			;d531	a7 	.
	jp z,le1d7h		;d532	ca d7 e1 	. . .
	ex de,hl			;d535	eb 	.
	ld ix,0bef0h		;d536	dd 21 f0 be 	. ! . .
	call sub_edb6h		;d53a	cd b6 ed 	. . .
	jp c,le1d7h		;d53d	da d7 e1 	. . .
	ret nz			;d540	c0 	.
	jp le1e9h		;d541	c3 e9 e1 	. . .
	ld a,b			;d544	78 	x
	cp 002h		;d545	fe 02 	. .
	ret c			;d547	d8 	.
	inc de			;d548	13 	.
	inc de			;d549	13 	.
	dec b			;d54a	05 	.
	dec b			;d54b	05 	.
	ret			;d54c	c9 	.

;=======================================================================
RSX_USER:
;=======================================================================
	cp 002h		;d54d	fe 02 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;d54f	d2 9f fb 	. . .
	and a			;d552	a7 	.
	jr z,ld558h		;d553	28 03 	( .
	ld a,(ix+000h)		;d555	dd 7e 00 	. ~ .
ld558h:
	ld (iy+04eh),a		;d558	fd 77 4e 	. w N
	ld (iy+04fh),0ffh		;d55b	fd 36 4f ff 	. 6 O .
	and 00fh		;d55f	e6 0f 	. .
	ld (iy+036h),a		;d561	fd 77 36 	. w 6
	ld ix,0bef0h		;d564	dd 21 f0 be 	. ! . .
	ld a,(iy+001h)		;d568	fd 7e 01 	. ~ .
	ld (ix+000h),a		;d56b	dd 77 00 	. w .
	;load ix[1-4] for |USER, then do a RSX call
	ld (ix+001h),055h		;d56e	dd 36 01 55 	. 6 . U
	ld (ix+002h),053h		;d572	dd 36 02 53 	. 6 . S
	ld (ix+003h),045h		;d576	dd 36 03 45 	. 6 . E
	ld (ix+004h),0d2h		;d57a	dd 36 04 d2 	. 6 . .
	ld ix,0bef5h		;d57e	dd 21 f5 be 	. ! . .
	ld a,(iy+036h)		;d582	fd 7e 36 	. ~ 6
	ld (ix+000h),a		;d585	dd 77 00 	. w .
	ld (ix+001h),000h		;d588	dd 36 01 00 	. 6 . .
	jp ld687h		;d58c	c3 87 d6 	. . .

;=======================================================================
RSX_REN:
;=======================================================================
	cp 002h		;d58f	fe 02 	. .
	jp nz,MSG_WRONG_PARAMETER_AMT		;d591	c2 97 fb 	. . .
	call sub_da1bh		;d594	cd 1b da 	. . .
	ret nz			;d597	c0 	.
	call sub_d9a0h		;d598	cd a0 d9 	. . .
	ret nc			;d59b	d0 	.
	jp nz,MSG_DISC_NOT_FORMATTED		;d59c	c2 22 fc 	. " .
	cp 081h		;d59f	fe 81 	. .
	jr z,ld5b8h		;d5a1	28 15 	( .
	ld hl,0bef0h		;d5a3	21 f0 be 	! . .
	ld a,(iy+001h)		;d5a6	fd 7e 01 	. ~ .
	ld (hl),a			;d5a9	77 	w
	inc hl			;d5aa	23 	#
	ld (hl),052h		;d5ab	36 52 	6 R
	inc hl			;d5ad	23 	#
	ld (hl),045h		;d5ae	36 45 	6 E
	inc hl			;d5b0	23 	#
	ld (hl),0ceh		;d5b1	36 ce 	6 .
	ld a,002h		;d5b3	3e 02 	> .
	jp ld689h		;d5b5	c3 89 d6 	. . .
ld5b8h:
	call sub_da80h		;d5b8	cd 80 da 	. . .
	push ix		;d5bb	dd e5 	. .
	ld ix,0bef0h		;d5bd	dd 21 f0 be 	. ! . .
	call sub_edb6h		;d5c1	cd b6 ed 	. . .
	pop ix		;d5c4	dd e1 	. .
	ret nz			;d5c6	c0 	.
	inc ix		;d5c7	dd 23 	. #
	inc ix		;d5c9	dd 23 	. #
	call sub_da80h		;d5cb	cd 80 da 	. . .
	ld ix,0bee0h		;d5ce	dd 21 e0 be 	. ! . .
	ld e,(iy+WS_DRIVE_NUMBER)		;d5d2	fd 5e 04 	. ^ .
	push de			;d5d5	d5 	.
	call sub_edb6h		;d5d6	cd b6 ed 	. . .
	pop de			;d5d9	d1 	.
	ret nz			;d5da	c0 	.
	ld a,(iy+WS_DRIVE_NUMBER)		;d5db	fd 7e 04 	. ~ .
	cp e			;d5de	bb 	.
	jp nz,MSG_BAD_DRIVE		;d5df	c2 c2 fb 	. . .
sub_d5e2h:
	call sub_ed01h		;d5e2	cd 01 ed 	. . .
	ret nz			;d5e5	c0 	.
	ld ix,0bef0h		;d5e6	dd 21 f0 be 	. ! . .
	call sub_ed38h		;d5ea	cd 38 ed 	. 8 .
	jp nz,lfbf7h		;d5ed	c2 f7 fb 	. . .
ld5f0h:
	call sub_db19h		;d5f0	cd 19 db 	. . .
	bit 1,a		;d5f3	cb 4f 	. O
	jr nz,ld602h		;d5f5	20 0b 	  .
	push hl			;d5f7	e5 	.
	push bc			;d5f8	c5 	.
	call sub_fc2ah		;d5f9	cd 2a fc 	. * .
	call MSG_ACCESS_DENIED		;d5fc	cd a7 fb 	. . .
	xor a			;d5ff	af 	.
	jr ld607h		;d600	18 05 	. .
ld602h:
	push hl			;d602	e5 	.
	push bc			;d603	c5 	.
	call sub_d618h		;d604	cd 18 d6 	. . .
ld607h:
	pop bc			;d607	c1 	.
	pop hl			;d608	e1 	.
	ret nz			;d609	c0 	.
	ld de,RAM_LAM		;d60a	11 20 00 	.   .
	add hl,de			;d60d	19 	.
	ld ix,0bef0h		;d60e	dd 21 f0 be 	. ! . .
	call sub_ed38h		;d612	cd 38 ed 	. 8 .
	ret nz			;d615	c0 	.
	jr ld5f0h		;d616	18 d8 	. .
sub_d618h:
	inc hl			;d618	23 	#
	ld de,0bee0h		;d619	11 e0 be 	. . .
	ld ix,0bed0h		;d61c	dd 21 d0 be 	. ! . .
	push bc			;d620	c5 	.
	push hl			;d621	e5 	.
	ld b,010h		;d622	06 10 	. .
ld624h:
	ld a,(de)			;d624	1a 	.
	cp 03fh		;d625	fe 3f 	. ?
	jr nz,ld62ah		;d627	20 01 	  .
	ld a,(hl)			;d629	7e 	~
ld62ah:
	ld (ix+000h),a		;d62a	dd 77 00 	. w .
	inc hl			;d62d	23 	#
	inc de			;d62e	13 	.
	inc ix		;d62f	dd 23 	. #
	djnz ld624h		;d631	10 f1 	. .
	call sub_ed01h		;d633	cd 01 ed 	. . .
	ld ix,0bed0h		;d636	dd 21 d0 be 	. ! . .
	call sub_ed38h		;d63a	cd 38 ed 	. 8 .
	pop hl			;d63d	e1 	.
	pop bc			;d63e	c1 	.
	jp z,MSG_ALREADY_EXISTS		;d63f	ca 0f fc 	. . .
	push hl			;d642	e5 	.
	call sub_ef6ch		;d643	cd 6c ef 	. l .
	pop de			;d646	d1 	.
	ld hl,0bed0h		;d647	21 d0 be 	! . .
	push bc			;d64a	c5 	.
	ld bc,SIDE_CALL_RST_2		;d64b	01 10 00 	. . .
	ldir		;d64e	ed b0 	. .
	pop bc			;d650	c1 	.
	call sub_efbch		;d651	cd bc ef 	. . .
	jp nc,lda0ah		;d654	d2 0a da 	. . .
	xor a			;d657	af 	.
	ret			;d658	c9 	.

;=======================================================================
RSX_ERA:
;=======================================================================
	cp 002h		;d659	fe 02 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;d65b	d2 9f fb 	. . .
	and a			;d65e	a7 	.
	call z,PROMPT_ENTER_NAME		;d65f	cc dc d8 	. . .
	call sub_da1bh		;d662	cd 1b da 	. . .
	ret nz			;d665	c0 	.
	call sub_d9a0h		;d666	cd a0 d9 	. . .
	ret nc			;d669	d0 	.
	jp nz,MSG_DISC_NOT_FORMATTED		;d66a	c2 22 fc 	. " .
	cp 081h		;d66d	fe 81 	. .
	jr z,ld69ah		;d66f	28 29 	( )
	ld hl,0bef0h		;d671	21 f0 be 	! . .
	ld a,(iy+001h)		;d674	fd 7e 01 	. ~ .
	ld (hl),a			;d677	77 	w
	inc hl			;d678	23 	#
	ld (hl),045h		;d679	36 45 	6 E
	inc hl			;d67b	23 	#
	ld (hl),052h		;d67c	36 52 	6 R
	inc hl			;d67e	23 	#
	ld (hl),0c1h		;d67f	36 c1 	6 .
	call sub_da80h		;d681	cd 80 da 	. . .
	call sub_dabah		;d684	cd ba da 	. . .
ld687h:
	ld a,001h		;d687	3e 01 	> .
ld689h:
	ld hl,0bef0h		;d689	21 f0 be 	! . .
ld68ch:
	push af			;d68c	f5 	.
	call KL_FIND_COMMAND		;d68d	cd d4 bc 	. . .
	jr nc,ld696h		;d690	30 04 	0 .
	pop af			;d692	f1 	.
	jp KL_FAR_PCHL		;d693	c3 1b 00 	. . .
ld696h:
	pop af			;d696	f1 	.
	jp MSG_CANT_FIND_AMSDOS		;d697	c3 b6 fb 	. . .
ld69ah:
	call sub_da80h		;d69a	cd 80 da 	. . .
	ld ix,0bee0h		;d69d	dd 21 e0 be 	. ! . .
	call sub_edb6h		;d6a1	cd b6 ed 	. . .
	ret nz			;d6a4	c0 	.
	call sub_ef8bh		;d6a5	cd 8b ef 	. . .
	call sub_ed01h		;d6a8	cd 01 ed 	. . .
	ret nz			;d6ab	c0 	.
	call sub_ed38h		;d6ac	cd 38 ed 	. 8 .
	jp nz,lfbf7h		;d6af	c2 f7 fb 	. . .
ld6b2h:
	call sub_db19h		;d6b2	cd 19 db 	. . .
	bit 1,a		;d6b5	cb 4f 	. O
	jr nz,ld6c2h		;d6b7	20 09 	  .
	push hl			;d6b9	e5 	.
	inc hl			;d6ba	23 	#
	push bc			;d6bb	c5 	.
	call sub_fbe9h		;d6bc	cd e9 fb 	. . .
	xor a			;d6bf	af 	.
	jr ld6c7h		;d6c0	18 05 	. .
ld6c2h:
	push hl			;d6c2	e5 	.
	push bc			;d6c3	c5 	.
	call sub_d6eah		;d6c4	cd ea d6 	. . .
ld6c7h:
	pop bc			;d6c7	c1 	.
	pop hl			;d6c8	e1 	.
	jp nz,lfbf7h		;d6c9	c2 f7 fb 	. . .
	ld de,RAM_LAM		;d6cc	11 20 00 	.   .
	add hl,de			;d6cf	19 	.
	call sub_ed38h		;d6d0	cd 38 ed 	. 8 .
	jr z,ld6b2h		;d6d3	28 dd 	( .
sub_d6d5h:
	call sub_f010h		;d6d5	cd 10 f0 	. . .
	ld (iy+03fh),0ffh		;d6d8	fd 36 3f ff 	. 6 ? .
	ret			;d6dc	c9 	.
sub_d6ddh:
	call sub_ef8bh		;d6dd	cd 8b ef 	. . .
	call sub_ed01h		;d6e0	cd 01 ed 	. . .
	ret nz			;d6e3	c0 	.
	call sub_ed38h		;d6e4	cd 38 ed 	. 8 .
	ret nz			;d6e7	c0 	.
	jr ld6b2h		;d6e8	18 c8 	. .
sub_d6eah:
	bit 2,(hl)		;d6ea	cb 56 	. V
	jp nz,MSG_BAD_FILE		;d6ec	c2 26 fc 	. & .
	push hl			;d6ef	e5 	.
	push bc			;d6f0	c5 	.
	ld de,00012h		;d6f1	11 12 00 	. . .
	add hl,de			;d6f4	19 	.
	ld b,(hl)			;d6f5	46 	F
	inc hl			;d6f6	23 	#
	ld c,(hl)			;d6f7	4e 	N
	ld de,0000ah		;d6f8	11 0a 00 	. . .
	add hl,de			;d6fb	19 	.
	ld a,(hl)			;d6fc	7e 	~
	cp 0feh		;d6fd	fe fe 	. .
	jp nz,ld75dh		;d6ff	c2 5d d7 	. ] .
	push bc			;d702	c5 	.
	ld bc,00081h		;d703	01 81 00 	. . .
	call sub_ef39h		;d706	cd 39 ef 	. 9 .
	pop bc			;d709	c1 	.
	jr nz,ld782h		;d70a	20 76 	  v
	ld a,(iy+005h)		;d70c	fd 7e 05 	. ~ .
	ld (0beb7h),a		;d70f	32 b7 be 	2 . .
	ld a,(iy+006h)		;d712	fd 7e 06 	. ~ .
	ld (0beb8h),a		;d715	32 b8 be 	2 . .
ld718h:
	push bc			;d718	c5 	.
	push iy		;d719	fd e5 	. .
	pop hl			;d71b	e1 	.
	ld de,002ebh		;d71c	11 eb 02 	. . .
	add hl,de			;d71f	19 	.
	call sub_ef48h		;d720	cd 48 ef 	. H .
	pop bc			;d723	c1 	.
	jr nz,ld757h		;d724	20 31 	  1
	ld a,(0beb7h)		;d726	3a b7 be 	: . .
	cp (hl)			;d729	be 	.
	jr nz,ld757h		;d72a	20 2b 	  +
	inc hl			;d72c	23 	#
	ld a,(0beb8h)		;d72d	3a b8 be 	: . .
	cp (hl)			;d730	be 	.
	jr nz,ld757h		;d731	20 24 	  $
	inc hl			;d733	23 	#
	inc hl			;d734	23 	#
	ld a,(hl)			;d735	7e 	~
	cp 002h		;d736	fe 02 	. .
	jr c,ld75ah		;d738	38 20 	8
	ld (iy+038h),a		;d73a	fd 77 38 	. w 8
	dec hl			;d73d	2b 	+
	ld a,(hl)			;d73e	7e 	~
	ld (iy+037h),a		;d73f	fd 77 37 	. w 7
	ld a,b			;d742	78 	x
	ld (0beb7h),a		;d743	32 b7 be 	2 . .
	ld a,c			;d746	79 	y
	ld (0beb8h),a		;d747	32 b8 be 	2 . .
	call sub_f093h		;d74a	cd 93 f0 	. . .
	jr nz,ld757h		;d74d	20 08 	  .
	ld b,(iy+037h)		;d74f	fd 46 37 	. F 7
	ld c,(iy+038h)		;d752	fd 4e 38 	. N 8
	jr ld718h		;d755	18 c1 	. .
ld757h:
	call MSG_BAD_FILE		;d757	cd 26 fc 	. & .
ld75ah:
	call sub_f093h		;d75a	cd 93 f0 	. . .
ld75dh:
	pop bc			;d75d	c1 	.
	call sub_ef39h		;d75e	cd 39 ef 	. 9 .
	pop hl			;d761	e1 	.
	jp nz,MSG_BAD_FILE		;d762	c2 26 fc 	. & .
	ld a,b			;d765	78 	x
	and a			;d766	a7 	.
	jr z,ld773h		;d767	28 0a 	( .
ld769h:
	set 7,(hl)		;d769	cb fe 	. .
	call sub_efbch		;d76b	cd bc ef 	. . .
	jp nz,MSG_BAD_FILE		;d76e	c2 26 fc 	. & .
	xor a			;d771	af 	.
	ret			;d772	c9 	.
ld773h:
	ld a,c			;d773	79 	y
	cp 081h		;d774	fe 81 	. .
	jr nz,ld769h		;d776	20 f1 	  .
	set 7,(hl)		;d778	cb fe 	. .
	ld bc,00200h		;d77a	01 00 02 	. . .
	add hl,bc			;d77d	09 	.
	set 7,(hl)		;d77e	cb fe 	. .
	xor a			;d780	af 	.
	ret			;d781	c9 	.
ld782h:
	pop bc			;d782	c1 	.
	pop hl			;d783	e1 	.
	jp MSG_BAD_FILE		;d784	c3 26 fc 	. & .
DO_LOGICAL_DRIVE:
	inc ix		;d787	dd 23 	. #
	inc ix		;d789	dd 23 	. #
	call sub_PREP_STRING_PARAM		;d78b	cd 85 da 	. . .
	;Returns B=Length of string and DE=location of string
	dec ix		;d78e	dd 2b 	. +
	dec ix		;d790	dd 2b 	. +

	;If lenght of paramter>1 then print "Bad Drive"
	ld a,b			;d792	78 	x
	cp 001h		;d793	fe 01 	. .
	jp nz,MSG_BAD_DRIVE		;d795	c2 c2 fb 	. . .

	;So this section loads (de) into A. If A>ascii(a) then uppercase it (ascii(a)-32) and look for a range between A and I
	ld a,(de)			;d798	1a 	.
	cp 061h		;d799	fe 61 	. a
	call nc,FUNC_SUBTRACT_32		;d79b	d4 9d d9 	. . .
	cp 041h		;d79e	fe 41 	. A
	jp c,MSG_BAD_DRIVE		;d7a0	da c2 fb 	. . .
	cp 049h		;d7a3	fe 49 	. I
	jp nc,MSG_BAD_DRIVE		;d7a5	d2 c2 fb 	. . .


	sub 041h		;d7a8	d6 41 	. A
	;So take a range of letters starting at A and convert into numbers starting at 0
	ld e,a			;d7aa	5f 	_
	ld d,000h		;d7ab	16 00 	. .
	ld hl,0be00h		;d7ad	21 00 be 	! . .
	add hl,de			;d7b0	19 	.
	;HL=be00+drive number
	ld a,(ix+000h)		;d7b1	dd 7e 00 	. ~ .
	cp (hl)			;d7b4	be 	.
	ret z			;d7b5	c8 	.
	ld (hl),a			;d7b6	77 	w
	ld a,(iy+WS_CURRENT_DRIVE_LETTER)		;d7b7	fd 7e 03 	. ~ .
	cp e			;d7ba	bb 	.
	ret nz			;d7bb	c0 	.
	ld c,e			;d7bc	4b 	K
	jr CHANGE_DRIVE		;d7bd	18 0a 	. .

;=======================================================================
RSX_A:
;Just an alias for |DRIVE,"A"
;=======================================================================
	ld c,000h		;d7bf	0e 00 	. .
	jr CHANGE_DRIVE		;d7c1	18 06 	. .

;=======================================================================
RSX_B:
;Just an alias for |DRIVE,"B"
;=======================================================================
	ld c,001h		;d7c3	0e 01 	. .
	jr CHANGE_DRIVE		;d7c5	18 02 	. .

;=======================================================================
RSX_C:
;Just an alias for |DRIVE,"C"
;=======================================================================
	ld c,002h		;d7c7	0e 02 	. .
CHANGE_DRIVE:
	ld a,c			;d7c9	79 	y
	ld (iy+WS_CURRENT_DRIVE_LETTER),a		;d7ca	fd 77 03 	. w .
	jr PROCESS_DRIVE_CHANGE		;d7cd	18 29 	. )

;=======================================================================
RSX_DRIVE:
;=======================================================================
;Input:
; A=Number of Parameters
; IX=pointer to the parameters

;First no more than 2 parameters eg, |DRIVE,"A",8
	cp 003h		;d7cf	fe 03 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;d7d1	d2 9f fb 	. . .

;If 2 parameters, then we're going to alias a logical letter to a physical drive
;eg |DRIVE,"F",0 creates a F drive thats actually the physical A drive
	cp 002h		;d7d4	fe 02 	. .
	jr z,DO_LOGICAL_DRIVE		;d7d6	28 af 	( .
	and a			;d7d8	a7 	.
	jp z,MSG_WRONG_PARAMETER_AMT		;d7d9	ca 97 fb 	. . .
	call sub_PREP_STRING_PARAM		;d7dc	cd 85 da 	. . .
	ld a,b			;d7df	78 	x
	cp 001h		;d7e0	fe 01 	. .
	jp nz,MSG_BAD_DRIVE		;d7e2	c2 c2 fb 	. . .
	ld a,(de)			;d7e5	1a 	.
	cp 061h		;d7e6	fe 61 	. a
	call nc,FUNC_SUBTRACT_32		;d7e8	d4 9d d9 	. . .
	cp 049h		;d7eb	fe 49 	. I
	jp nc,MSG_BAD_DRIVE		;d7ed	d2 c2 fb 	. . .
	sub 041h		;d7f0	d6 41 	. A
	jp c,MSG_BAD_DRIVE		;d7f2	da c2 fb 	. . .
	ld (iy+WS_CURRENT_DRIVE_LETTER),a		;d7f5	fd 77 03 	. w .
PROCESS_DRIVE_CHANGE:
	ld e,a			;d7f8	5f 	_
	call sub_f11ah		;d7f9	cd 1a f1 	. . .
	jp nz,MSG_BAD_DRIVE		;d7fc	c2 c2 fb 	. . .
	ld (iy+WS_DRIVE_NUMBER),e		;d7ff	fd 73 04 	. s .
	call sub_d9a0h		;d802	cd a0 d9 	. . .
	ret nc			;d805	d0 	.
	jp nz,MSG_DISC_NOT_FORMATTED		;d806	c2 22 fc 	. " .
	ld a,(iy+WS_CURRENT_DRIVE_LETTER)		;d809	fd 7e 03 	. ~ .
	ld (iy+013h),a		;d80c	fd 77 13 	. w .
	ld (iy+043h),a		;d80f	fd 77 43 	. w C
	call sub_da62h		;d812	cd 62 da 	. b .
	ld a,(iy+005h)		;d815	fd 7e 05 	. ~ .
	ld (iy+044h),a		;d818	fd 77 44 	. w D
	ld a,(iy+006h)		;d81b	fd 7e 06 	. ~ .
	ld (iy+045h),a		;d81e	fd 77 45 	. w E
	ld a,(iy+WS_DRIVE_NUMBER)		;d821	fd 7e 04 	. ~ .
	ld (iy+042h),a		;d824	fd 77 42 	. w B
ld827h:
	cp 008h		;d827	fe 08 	. .
	ret z			;d829	c8 	.
	ld e,a			;d82a	5f 	_
	call sub_c9dbh		;d82b	cd db c9 	. . .
	ld a,(iy+001h)		;d82e	fd 7e 01 	. ~ .
	cp 03fh		;d831	fe 3f 	. ?
	ret z			;d833	c8 	.
	ld a,e			;d834	7b 	{
	ld hl,0bef0h		;d835	21 f0 be 	! . .
	and 001h		;d838	e6 01 	. .
	add a,0c1h		;d83a	c6 c1 	. .
	ld e,a			;d83c	5f 	_
	ld a,(iy+001h)		;d83d	fd 7e 01 	. ~ .
	ld (hl),a			;d840	77 	w
	inc hl			;d841	23 	#
	ld (hl),e			;d842	73 	s
	xor a			;d843	af 	.
	call ld689h		;d844	cd 89 d6 	. . .
	xor a			;d847	af 	.
	ret			;d848	c9 	.

;=======================================================================
RSX_EB:
;=======================================================================
	and a			;d849	a7 	.
	jp nz,MSG_TOO_MANY_PARAMETERS		;d84a	c2 9f fb 	. . .
	ld ix,0bed0h		;d84d	dd 21 d0 be 	. ! . .
	ld de,0bed2h		;d851	11 d2 be 	. . .
	ld (ix+000h),e		;d854	dd 73 00 	. s .
	ld (ix+001h),d		;d857	dd 72 01 	. r .
	ld hl,ld86dh		;d85a	21 6d d8 	! m .
	ld bc,KL_LOW_PCHL		;d85d	01 0b 00 	. . .
	ldir		;d860	ed b0 	. .
	ld hl,0bedah		;d862	21 da be 	! . .
	ld (0be83h),hl		;d865	22 83 be 	" . .
	ld a,001h		;d868	3e 01 	> .
	jp sub_d48ch		;d86a	c3 8c d4 	. . .
ld86dh:
	defb 5		;d86d	05 	.
	defb 0d5h			;d86e	d5 	.
	defb 0beh			;d86f	be 	.
	defb '*.BAKER',0c1h ;AKA "*.BAKERA"

sub_d878h:
	push de			;d878	d5 	.
	push iy		;d879	fd e5 	. .
	pop hl			;d87b	e1 	.
	ld de,002ebh		;d87c	11 eb 02 	. . .
	add hl,de			;d87f	19 	.
	pop de			;d880	d1 	.
	ret			;d881	c9 	.
sub_d882h:
	push hl			;d882	e5 	.
	push iy		;d883	fd e5 	. .
	pop hl			;d885	e1 	.
	ld de,002ebh		;d886	11 eb 02 	. . .
	add hl,de			;d889	19 	.
	ex de,hl			;d88a	eb 	.
	pop hl			;d88b	e1 	.
	ret			;d88c	c9 	.
DO_CLS:
	ld hl,CLS_DATA	;We're printing characters from here	;d88d	21 9a d8 	! . .
	ld b,011h		;The string is 0x11h long ;d890	06 11 	. .
PRINT_STRING:
	ld a,(hl)			;d892	7e 	~
	call TXT_OUTPUT		;d893	cd 5a bb 	. Z .
	inc hl			;d896	23 	#
	djnz PRINT_STRING		;d897	10 f9 	. .
	ret			;d899	c9 	.
CLS_DATA:
	;This data provides escape codes to do: Clear screen to Mode 2, with white text (ink 13) on black paper
	defb 04h,002h ;Set mode 2
	defb 0eh,000h ;Set paper ink 0 (Black)
	defb 0fh,001h ;Set pen ink 1 (White)
	defb 01ch,00h,00h,00h ;Set ink 0 to 0,0 (Black/Black)
	defb 01ch,01h,0dh,0dh ;Set ink 1 to 13,13 (White/White)
	defb 01dh,00h,00h ;Border 0,0 (Black, Black)

NEWLINE_IF_NO_DISPLAY_SPACE:
;This is related to directory listing.
;If there isn't enough width to display another file (18 characters), print a new line
	push hl			;d8ab	e5 	.
	call TXT_GET_WINDOW		;Returns the size of the current window - returns physical coordinates ;d8ac	cd 69 bb 	. i .
	;Exit: H holds the column number of the left edge, D holds the column number of the right edge, L holds the
	;line number of the top edge, E holds the line number of the bottom edge, A is corrupt, Carry is false if
	;the window covers the entire screen, and the other registers are always preserved
	ld a,d		;D=Column number of right edge	;d8af	7a 	z
	sub h			;So right_edge-left_edge aka get the window width ;d8b0	94 	.
	ld e,a		;Save this to E	;d8b1	5f 	_
	call TXT_GET_CURSOR		;d8b2	cd 78 bb 	. x .
	;Exit: H holds the logical column number, L holds the logical line number, and A contains the roll count, the
	;flags are corrupt, and all the other registers are preserved
	;Notes: The roll count is increased when the screen is scrolled down, and is decreased when it is scrolled up
	ld a,e		;Get the window width back from E	;d8b5	7b 	{
	sub h			;calculate window_width-cursor_column ;d8b6	94 	.
	pop hl			;d8b7	e1 	.
	cp 012h		;If thats>012h (18) print a new line ;d8b8	fe 12 	. .
	jp c,PRINT_CR_LF		;d8ba	da 7d d9 	. } .
	ret			;d8bd	c9 	.

PRINT_ENTER_NAME:
	ld hl,MSG_ENTER_NAME		;d8be	21 cf d8 	! . .
	call DISPLAY_MSG		;d8c1	cd 6a d9 	. j .
	call sub_f1adh		;d8c4	cd ad f1 	. . .
	ld a,b			;d8c7	78 	x
	cp 011h		;d8c8	fe 11 	. .
	jr nc,PRINT_ENTER_NAME		;d8ca	30 f2 	0 .
	jp ld8f7h		;d8cc	c3 f7 d8 	. . .
MSG_ENTER_NAME:
  defb '{Enter name:',0

PROMPT_ENTER_NAME:
	call PRINT_ENTER_NAME ;d8dc
	ld ix,0bee0h		;d8df	dd 21 e0 be 	. ! . .
	ld (ix+000h),0e2h		;d8e3	dd 36 00 e2 	. 6 . .
	ld (ix+001h),0beh		;d8e7	dd 36 01 be 	. 6 . .
	ld (ix+002h),b		;d8eb	dd 70 02 	. p .
	ld (ix+003h),l		;d8ee	dd 75 03 	. u .
	ld (ix+004h),h		;d8f1	dd 74 04 	. t .
	ld a,001h		;d8f4	3e 01 	> .
	ret			;d8f6	c9 	.
ld8f7h:
	push iy		;d8f7	fd e5 	. .
	pop hl			;d8f9	e1 	.
	ld de,00218h		;d8fa	11 18 02 	. . .
	add hl,de			;d8fd	19 	.
	ret			;d8fe	c9 	.
sub_d8ffh:
	ld de,00064h		;d8ff	11 64 00 	. d .
	call sub_d92ch		;d902	cd 2c d9 	. , .
	ld e,00ah		;d905	1e 0a 	. .
	call sub_d92ch		;d907	cd 2c d9 	. , .
	ld e,001h		;d90a	1e 01 	. .
	jp sub_d92ch		;d90c	c3 2c d9 	. , .
sub_d90fh:
	ld l,a			;d90f	6f 	o
	ld h,000h		;d910	26 00 	& .
	ld d,h			;d912	54 	T
	cp 064h		;d913	fe 64 	. d
	jr c,ld91eh		;d915	38 07 	8 .
	ld e,064h		;d917	1e 64 	. d
	call sub_d92ch		;d919	cd 2c d9 	. , .
	ld a,063h		;d91c	3e 63 	> c
ld91eh:
	cp 00ah		;d91e	fe 0a 	. .
	jr c,ld927h		;d920	38 05 	8 .
	ld e,00ah		;d922	1e 0a 	. .
	call sub_d92ch		;d924	cd 2c d9 	. , .
ld927h:
	ld e,001h		;d927	1e 01 	. .
	jp sub_d92ch		;d929	c3 2c d9 	. , .
sub_d92ch:
	ld a,030h		;d92c	3e 30 	> 0
ld92eh:
	and a			;d92e	a7 	.
	sbc hl,de		;d92f	ed 52 	. R
	jr c,ld936h		;d931	38 03 	8 .
	inc a			;d933	3c 	<
	jr ld92eh		;d934	18 f8 	. .
ld936h:
	add hl,de			;d936	19 	.
	jp TXT_OUTPUT		;d937	c3 5a bb 	. Z .
sub_d93ah:
	push af			;d93a	f5 	.
	sra a		;d93b	cb 2f 	. /
	sra a		;d93d	cb 2f 	. /
	sra a		;d93f	cb 2f 	. /
	sra a		;d941	cb 2f 	. /
	call sub_d947h		;d943	cd 47 d9 	. G .
	pop af			;d946	f1 	.
sub_d947h:
	and 00fh		;d947	e6 0f 	. .
	cp 00ah		;d949	fe 0a 	. .
	jr c,ld94fh		;d94b	38 02 	8 .
	add a,007h		;d94d	c6 07 	. .
ld94fh:
	add a,030h		;d94f	c6 30 	. 0
	jp TXT_OUTPUT		;d951	c3 5a bb 	. Z .
ROM_SELECT_DESELECT:
	;This function is never directly called.
	;This gets relocated at sub_RELOCATE_ROM_SELECT_DESELECT, and all the calls are made to the
	;relocated area.
	;My guess is that this is the equivalent of push rom/pop rom
	push bc			;d954	c5 	.
	call KL_ROM_SELECT		;d955	cd 0f b9 	. . .
	; 005   &B90F   KL ROM SELECT
	;       Action: Selects an upper ROM and also enables it
	;       Entry:  C contains the ROM select address of the required ROM
	;       Exit:   C contains the ROM select  address of the previous ROM,
	;               and B contains the state of the previous ROM

	ld a,(hl)			;d958	7e 	~
	call KL_ROM_DESELECT		;d959	cd 18 b9 	. . .
	; 008   &B918   KL ROM DESELECT
	;       Action: Selects the previous upper ROM and sets its state
	;       Entry:  C contains me  ROM  select  address  of  the  ROM to be
	;               reselected, and B contains  the  state  of the required
	;               ROM
	;       Exit:   C contains the ROM select address  of the current ROM, B
	;               is corrupt, and all others are preserved
	;       Notes:  This routine reverses the acoon  of  KL ROM SELECT, and
	;               uses the values that it returns in B and C

	pop bc			;d95c	c1 	.
	ret			;d95d	c9 	.
sub_RELOCATE_ROM_SELECT_DESELECT:
;relocate the code above to 0xbec0.
;I'm assuming that this is because another rom may swap in
;and the calls could fail.
	ld bc,0000ah		;d95e	01 0a 00 	. . .
	ld hl,ROM_SELECT_DESELECT		;d961	21 54 d9 	! T .
	ld de,ROM_SELECT_DESELECT_RELOCATED		;d964	11 c0 be 	. . .
	ldir		;d967	ed b0 	. .
	ret			;d969	c9 	.
DISPLAY_MSG:
	ld a,(hl)			;d96a	7e 	~
	and a			;d96b	a7 	.
	ret z			;d96c	c8 	.
	cp 05ch		;d96d	fe 5c 	. \
	jr z,ld97ch		;d96f	28 0b 	( .
	cp 07bh		;d971	fe 7b 	. {
	call z,PRINT_CR_ONLY		;d973	cc 83 d9 	. . .
	call TXT_OUTPUT		;d976	cd 5a bb 	. Z .
	inc hl			;d979	23 	#
	jr DISPLAY_MSG		;d97a	18 ee 	. .
ld97ch:
	inc hl			;d97c	23 	#

PRINT_CR_LF:
	call PRINT_CR_ONLY		;d97d	cd 83 d9 	. . .
	;Now print the optional LF from the previous call
	jp TXT_OUTPUT		;d980	c3 5a bb 	. Z .
PRINT_CR_ONLY:
;print CR, but LF is optional
;This exits with a=0xa (ascii 10)
	ld a,00dh		;d983	3e 0d 	> .
	call TXT_OUTPUT		;d985	cd 5a bb 	. Z .
	ld a,00ah		;d988	3e 0a 	> .
	ret			;d98a	c9 	.

DELETE_CHAR:
	push af			;d98b	f5 	.
	ld a,008h		;Backspace ;d98c	3e 08 	> .
	call TXT_OUTPUT		;d98e	cd 5a bb 	. Z .
	ld a,020h		;Space to erase the screen character ;d991	3e 20 	>
	call TXT_OUTPUT		;d993	cd 5a bb 	. Z .
	ld a,008h		;Backagain to reset the cursor ;d996	3e 08 	> .
	call TXT_OUTPUT		;d998	cd 5a bb 	. Z .
	pop af			;d99b	f1 	.
	ret			;d99c	c9 	.
FUNC_SUBTRACT_32:
	sub 020h		;A=A-32 (aka A=A-0x20);d99d	d6 20 	.
	ret			;d99f	c9 	.
sub_d9a0h:
	ld a,(iy+WS_DRIVE_NUMBER)		;d9a0	fd 7e 04 	. ~ .
	ld (iy+012h),000h		;d9a3	fd 36 12 00 	. 6 . .
	cp 008h		;d9a7	fe 08 	. .
	jr z,ld9c1h		;d9a9	28 16 	( .
	ld e,a			;d9ab	5f 	_
	bit 2,a		;d9ac	cb 57 	. W
	call nz,sub_d9bch		;d9ae	c4 bc d9 	. . .
	call sub_ca03h		;d9b1	cd 03 ca 	. . .
	ret nz			;d9b4	c0 	.
	ld a,c			;d9b5	79 	y
	and 0e0h		;d9b6	e6 e0 	. .
	inc a			;d9b8	3c 	<
	cp a			;d9b9	bf 	.
	scf			;d9ba	37 	7
	ret			;d9bb	c9 	.
sub_d9bch:
	ld (iy+012h),001h		;d9bc	fd 36 12 01 	. 6 . .
	ret			;d9c0	c9 	.
ld9c1h:
	ld bc,07fc4h		;d9c1	01 c4 7f 	. . 
	out (c),c		;d9c4	ed 49 	. I
	ld a,(04100h)		;d9c6	3a 00 41 	: . A
	cp 044h		;d9c9	fe 44 	. D
	jr nz,ld9d2h		;d9cb	20 05 	  .
	ld a,(04101h)		;d9cd	3a 01 41 	: . A
	cp 052h		;d9d0	fe 52 	. R
ld9d2h:
	ld c,0c0h		;d9d2	0e c0 	. .
	out (c),c		;d9d4	ed 49 	. I
	ld a,081h		;d9d6	3e 81 	> .
	scf			;d9d8	37 	7
	ret			;d9d9	c9 	.
sub_d9dah:
	push de			;d9da	d5 	.
	ld de,0002ah		;d9db	11 2a 00 	. * .
ld9deh:
	push iy		;d9de	fd e5 	. .
	pop hl			;d9e0	e1 	.
	add hl,de			;d9e1	19 	.
	ld d,000h		;d9e2	16 00 	. .
	ld e,(iy+WS_CURRENT_DRIVE_LETTER)		;d9e4	fd 5e 03 	. ^ .
	add hl,de			;d9e7	19 	.
	pop de			;d9e8	d1 	.
	ret			;d9e9	c9 	.
sub_d9eah:
	push de			;d9ea	d5 	.
	ld de,00021h		;d9eb	11 21 00 	. ! .
	jr ld9deh		;d9ee	18 ee 	. .
sub_d9f0h:
	push hl			;d9f0	e5 	.
	call sub_d9eah		;d9f1	cd ea d9 	. . .
ld9f4h:
	ld (hl),a			;d9f4	77 	w
	pop hl			;d9f5	e1 	.
	ret			;d9f6	c9 	.
sub_d9f7h:
	push hl			;d9f7	e5 	.
	call sub_d9dah		;d9f8	cd da d9 	. . .
	jr ld9f4h		;d9fb	18 f7 	. .
sub_d9fdh:
	push hl			;d9fd	e5 	.
	call sub_d9eah		;d9fe	cd ea d9 	. . .
lda01h:
	ld a,(hl)			;da01	7e 	~
	pop hl			;da02	e1 	.
	ret			;da03	c9 	.
sub_da04h:
	push hl			;da04	e5 	.
	call sub_d9dah		;da05	cd da d9 	. . .
	jr lda01h		;da08	18 f7 	. .
lda0ah:
	ld a,095h		;da0a	3e 95 	> .
	cp 096h		;da0c	fe 96 	. .
	ret			;da0e	c9 	.
lda0fh:
	ld a,095h		;da0f	3e 95 	> .
	cp 094h		;da11	fe 94 	. .
	ret			;da13	c9 	.
lda14h:
	ld a,095h		;da14	3e 95 	> .
	cp a			;da16	bf 	.
	ret			;da17	c9 	.
lda18h:
	xor a			;da18	af 	.
	scf			;da19	37 	7
	ret			;da1a	c9 	.
sub_da1bh:
	call sub_da62h		;da1b	cd 62 da 	. b .
	call sub_PREP_STRING_PARAM		;da1e	cd 85 da 	. . .
sub_da21h:
	ld a,b			;da21	78 	x
	cp 002h		;da22	fe 02 	. .
	jr c,lda44h		;da24	38 1e 	8 .
	inc de			;da26	13 	.
	ld a,(de)			;da27	1a 	.
	dec de			;da28	1b 	.
	cp 03ah		;da29	fe 3a 	. :
	jr nz,lda44h		;da2b	20 17 	  .
	ld a,(de)			;da2d	1a 	.
	cp 061h		;da2e	fe 61 	. a
	call nc,FUNC_SUBTRACT_32		;da30	d4 9d d9 	. . .
	cp 049h		;da33	fe 49 	. I
	jp nc,MSG_BAD_DRIVE		;da35	d2 c2 fb 	. . .
	sub 041h		;da38	d6 41 	. A
	jp c,MSG_BAD_DRIVE		;da3a	da c2 fb 	. . .
	ld (iy+WS_CURRENT_DRIVE_LETTER),a		;da3d	fd 77 03 	. w .
	inc de			;da40	13 	.
	inc de			;da41	13 	.
	dec b			;da42	05 	.
	dec b			;da43	05 	.
lda44h:
	ld a,(iy+WS_CURRENT_DRIVE_LETTER)		;da44	fd 7e 03 	. ~ .
lda47h:
	push de			;da47	d5 	.
	ld e,a			;da48	5f 	_
	call sub_f11ah		;da49	cd 1a f1 	. . .
	pop de			;da4c	d1 	.
	jp nz,MSG_BAD_DRIVE		;da4d	c2 c2 fb 	. . .
	push af			;da50	f5 	.
	ld (iy+WS_DRIVE_NUMBER),a		;da51	fd 77 04 	. w .
	call sub_d9fdh		;da54	cd fd d9 	. . .
	ld (iy+005h),a		;da57	fd 77 05 	. w .
	call sub_da04h		;da5a	cd 04 da 	. . .
	ld (iy+006h),a		;da5d	fd 77 06 	. w .
	pop af			;da60	f1 	.
	ret			;da61	c9 	.
sub_da62h:
	ld a,(iy+013h)	;Probably Current Side of Disc	;da62	fd 7e 13 	. ~ .
	ld (iy+WS_CURRENT_DRIVE_LETTER),a	;Probably Current Disc	;da65	fd 77 03 	. w .
	jr lda47h		;da68	18 dd 	. .
sub_da6ah:
	call sub_da1bh		;da6a	cd 1b da 	. . .
	ret nz			;da6d	c0 	.
	push de			;da6e	d5 	.
	push bc			;da6f	c5 	.
	call sub_d9a0h		;da70	cd a0 d9 	. . .
	pop bc			;da73	c1 	.
	pop de			;da74	d1 	.
	ret nz			;da75	c0 	.
	ex de,hl			;da76	eb 	.
	cp 081h		;da77	fe 81 	. .
	jp nz,lda0ah		;da79	c2 0a da 	. . .
	xor a			;da7c	af 	.
	ld a,b			;da7d	78 	x
	scf			;da7e	37 	7
	ret			;da7f	c9 	.
sub_da80h:
	call sub_PREP_STRING_PARAM		;da80	cd 85 da 	. . .
	ex de,hl			;da83	eb 	.
	ret			;da84	c9 	.

sub_PREP_STRING_PARAM:
	;Entry:
	;IX = pointer to parameter address
	;
	;Exit:
	; 	B=Length of string parameter
	;		DE=Location of string
	;   So ld a,(de) would read the first character of the string

	;So HL=parameter address
	ld l,(ix+000h)		;da85	dd 6e 00 	. n .
	ld h,(ix+001h)		;da88	dd 66 01 	. f .
	;B=parameter value(1)
	ld b,(hl)			;da8b	46 	F
	inc hl			;da8c	23 	#
	;E=parameter value(2)
	ld e,(hl)			;da8d	5e 	^
	inc hl			;da8e	23 	#
	;D=parameter value(3)
	ld d,(hl)			;da8f	56 	V
	ret			;da90	c9 	.

sub_da91h:
	push hl			;da91	e5 	.
	push de			;da92	d5 	.
	ld hl,MSG_TRACK		;da93	21 b3 da 	! . .
	call DISPLAY_MSG		;da96	cd 6a d9 	. j .
	ld a,d			;da99	7a 	z
	call sub_d90fh		;da9a	cd 0f d9 	. . .
	ld a,02fh		;da9d	3e 2f 	> /
	call TXT_OUTPUT		;da9f	cd 5a bb 	. Z .
	ld a,026h		;daa2	3e 26 	> &
	call TXT_OUTPUT		;daa4	cd 5a bb 	. Z .
	ld a,c			;daa7	79 	y
	call sub_d93ah		;daa8	cd 3a d9 	. : .
	ld a,020h		;daab	3e 20 	>
	call TXT_OUTPUT		;daad	cd 5a bb 	. Z .
	pop de			;dab0	d1 	.
	pop hl			;dab1	e1 	.
	ret			;dab2	c9 	.
MSG_TRACK:
	db "Track ",0

sub_dabah:
	ld a,b			;daba	78 	x
	cp 002h		;dabb	fe 02 	. .
	jp c,ldb17h		;dabd	da 17 db 	. . .
	cp 010h		;dac0	fe 10 	. .
	jp nc,MSG_BAD_FILE_NAME		;dac2	d2 8e fb 	. . .
	ld (0bee5h),a		;dac5	32 e5 be 	2 . .
	inc hl			;dac8	23 	#
	ld a,(hl)			;dac9	7e 	~
	dec hl			;daca	2b 	+
	cp 03ah		;dacb	fe 3a 	. :
	jp nz,ldb17h		;dacd	c2 17 db 	. . .
	ld a,(hl)			;dad0	7e 	~
	cp 05bh		;dad1	fe 5b 	. [
	call nc,FUNC_SUBTRACT_32		;dad3	d4 9d d9 	. . .
	cp 049h		;dad6	fe 49 	. I
	jp nc,MSG_BAD_DRIVE		;dad8	d2 c2 fb 	. . .
	sub 041h		;dadb	d6 41 	. A
	jp c,MSG_BAD_DRIVE		;dadd	da c2 fb 	. . .
	ld e,a			;dae0	5f 	_
	call sub_f11ah		;dae1	cd 1a f1 	. . .
	jp nz,MSG_BAD_DRIVE		;dae4	c2 c2 fb 	. . .
	call sub_f143h		;dae7	cd 43 f1 	. C .
	jp nz,MSG_BAD_DRIVE		;daea	c2 c2 fb 	. . .
	ld de,0becbh		;daed	11 cb be 	. . .
	add a,041h		;daf0	c6 41 	. A
	cp 043h		;daf2	fe 43 	. C
	jp nc,MSG_BAD_DRIVE		;daf4	d2 c2 fb 	. . .
	ld (de),a			;daf7	12 	.
	inc de			;daf8	13 	.
	inc hl			;daf9	23 	#
	ld a,(0bee5h)		;dafa	3a e5 be 	: . .
	dec a			;dafd	3d 	=
	ld c,a			;dafe	4f 	O
	ld b,000h		;daff	06 00 	. .
	ldir		;db01	ed b0 	. .
	ld hl,0bee5h		;db03	21 e5 be 	! . .
	ld (0bee3h),hl		;db06	22 e3 be 	" . .
	ld hl,0becbh		;db09	21 cb be 	! . .
	ld (0bee6h),hl		;db0c	22 e6 be 	" . .
	ld ix,0bee3h		;db0f	dd 21 e3 be 	. ! . .
	ld a,(0bee5h)		;db13	3a e5 be 	: . .
	ld b,a			;db16	47 	G
ldb17h:
	xor a			;db17	af 	.
	ret			;db18	c9 	.
sub_db19h:
	push bc			;db19	c5 	.
	push hl			;db1a	e5 	.
	ld bc,KL_FAR_PCHL		;db1b	01 1b 00 	. . .
	add hl,bc			;db1e	09 	.
	ld a,(hl)			;db1f	7e 	~
	cp (iy+04eh)		;db20	fd be 4e 	. . N
	jr nz,ldb36h		;db23	20 11 	  .
	inc hl			;db25	23 	#
	ld a,(hl)			;db26	7e 	~
	cp (iy+04fh)		;db27	fd be 4f 	. . O
	jr nz,ldb36h		;db2a	20 0a 	  .
	pop hl			;db2c	e1 	.
	push hl			;db2d	e5 	.
	ld bc,00011h		;db2e	01 11 00 	. . .
	add hl,bc			;db31	09 	.
	ld a,(hl)			;db32	7e 	~
ldb33h:
	pop hl			;db33	e1 	.
	pop bc			;db34	c1 	.
	ret			;db35	c9 	.
ldb36h:
	pop hl			;db36	e1 	.
	push hl			;db37	e5 	.
	ld bc,00011h		;db38	01 11 00 	. . .
	add hl,bc			;db3b	09 	.
	ld a,(hl)			;db3c	7e 	~
	and 0c0h		;db3d	e6 c0 	. .
	ld b,a			;db3f	47 	G
	ld a,(hl)			;db40	7e 	~
	srl a		;db41	cb 3f 	. ?
	srl a		;db43	cb 3f 	. ?
	srl a		;db45	cb 3f 	. ?
	and 007h		;db47	e6 07 	. .
	or b			;db49	b0 	.
	jr ldb33h		;db4a	18 e7 	. .
sub_db4ch:
	push ix		;db4c	dd e5 	. .
	ld b,010h		;db4e	06 10 	. .
ldb50h:
	ld a,(ix+000h)		;db50	dd 7e 00 	. ~ .
	and a			;db53	a7 	.
	call z,SET_A_TO_020H		;db54	cc 0d df 	. . .
	call TXT_OUTPUT		;db57	cd 5a bb 	. Z .
	inc ix		;db5a	dd 23 	. #
	djnz ldb50h		;db5c	10 f2 	. .
	pop ix		;db5e	dd e1 	. .
	ret			;db60	c9 	.

;=======================================================================
RSX_FORMAT:
;=======================================================================
	cp 005h		;db61	fe 05 	. .
	jp z,ldd25h		;db63	ca 25 dd 	. % .
	jp nc,MSG_TOO_MANY_PARAMETERS		;db66	d2 9f fb 	. . .
	ld c,000h		;db69	0e 00 	. .
	cp 004h		;db6b	fe 04 	. .
	jr c,ldb77h		;db6d	38 08 	8 .
	ld c,(ix+000h)		;db6f	dd 4e 00 	. N .
	dec c			;db72	0d 	.
	inc ix		;db73	dd 23 	. #
	inc ix		;db75	dd 23 	. #
ldb77h:
	ld (iy+010h),c		;db77	fd 71 10 	. q .
	ld c,0ffh		;db7a	0e ff 	. .
	cp 003h		;db7c	fe 03 	. .
	jr c,ldb87h		;db7e	38 07 	8 .
	ld c,(ix+000h)		;db80	dd 4e 00 	. N .
	inc ix		;db83	dd 23 	. #
	inc ix		;db85	dd 23 	. #
ldb87h:
	ld (iy+00fh),c		;db87	fd 71 0f 	. q .
	ld c,0ffh		;db8a	0e ff 	. .
	cp 002h		;db8c	fe 02 	. .
	jr c,ldb97h		;db8e	38 07 	8 .
	ld c,(ix+000h)		;db90	dd 4e 00 	. N .
	inc ix		;db93	dd 23 	. #
	inc ix		;db95	dd 23 	. #
ldb97h:
	ld (iy+WS_DRIVE_NUMBER),c		;db97	fd 71 04 	. q .
	ld c,002h		;db9a	0e 02 	. .
	and a			;db9c	a7 	.
	jr z,ldba2h		;db9d	28 03 	( .
	ld c,(ix+000h)		;db9f	dd 4e 00 	. N .
ldba2h:
	ld (iy+002h),c		;dba2	fd 71 02 	. q .
	xor a			;dba5	af 	.
	ld (iy+012h),a		;dba6	fd 77 12 	. w .
	ld a,(iy+00fh)		;dba9	fd 7e 0f 	. ~ .
	ld b,a			;dbac	47 	G
	cp 0ffh		;dbad	fe ff 	. .
	jr nz,ldbd7h		;dbaf	20 26 	  &
	ld b,028h		;dbb1	06 28 	. (
	ld a,(iy+WS_DRIVE_NUMBER)		;dbb3	fd 7e 04 	. ~ .
	ld e,(iy+013h)		;dbb6	fd 5e 13 	. ^ .
	cp 0ffh		;dbb9	fe ff 	. .
	call z,sub_f11ah		;dbbb	cc 1a f1 	. . .
	ld (iy+WS_DRIVE_NUMBER),a		;dbbe	fd 77 04 	. w .
	cp 008h		;dbc1	fe 08 	. .
	jr nz,ldbd7h		;dbc3	20 12 	  .
	ld e,(iy+00bh)		;dbc5	fd 5e 0b 	. ^ .
	bit 7,(iy+00ch)		;dbc8	fd cb 0c 7e 	. . . ~
	call z,sub_dc04h		;dbcc	cc 04 dc 	. . .
	dec e			;dbcf	1d 	.
	ld hl,ldc09h		;dbd0	21 09 dc 	! . .
	ld d,000h		;dbd3	16 00 	. .
	add hl,de			;dbd5	19 	.
	ld b,(hl)			;dbd6	46 	F
ldbd7h:
	ld a,b			;dbd7	78 	x
	ld (iy+00fh),a		;dbd8	fd 77 0f 	. w .
	and a			;dbdb	a7 	.
	ret z			;dbdc	c8 	.
	xor a			;dbdd	af 	.
	ld (iy+03fh),a		;dbde	fd 77 3f 	. w ?
	ld (iy+040h),a		;dbe1	fd 77 40 	. w @
	ld a,(iy+WS_DRIVE_NUMBER)		;dbe4	fd 7e 04 	. ~ .
	ld (0be08h),a		;dbe7	32 08 be 	2 . .
	ld a,008h		;dbea	3e 08 	> .
	ld (iy+WS_CURRENT_DRIVE_LETTER),a		;dbec	fd 77 03 	. w .
	call sub_d9a0h		;dbef	cd a0 d9 	. . .
	ret nc			;dbf2	d0 	.
	call z,sub_fc30h		;dbf3	cc 30 fc 	. 0 .
	ld a,(iy+010h)		;dbf6	fd 7e 10 	. ~ .
	and a			;dbf9	a7 	.
	jp z,ldc29h		;dbfa	ca 29 dc 	. ) .
	jp ldc5fh		;dbfd	c3 5f dc 	. _ .
	ld a,0ffh		;dc00	3e ff 	> .
	and a			;dc02	a7 	.
	ret			;dc03	c9 	.
sub_dc04h:
	ld e,(iy+00ch)		;dc04	fd 5e 0c 	. ^ .
	dec e			;dc07	1d 	.
	ret			;dc08	c9 	.
ldc09h:
	inc bc			;dc09	03 	.
	ld b,009h		;dc0a	06 09 	. .
	inc c			;dc0c	0c 	.
	djnz ldc22h		;dc0d	10 13 	. .
	ld d,019h		;dc0f	16 19 	. .
	inc e			;dc11	1c 	.
	jr nz,$+37		;dc12	20 23 	  #
	ld h,029h		;dc14	26 29 	& )
	inc l			;dc16	2c 	,
	jr nc,ldc4ch		;dc17	30 33 	0 3
	ld (hl),039h		;dc19	36 39 	6 9
	inc a			;dc1b	3c 	<
	ld b,b			;dc1c	40 	@
	ld b,e			;dc1d	43 	C
	ld b,(hl)			;dc1e	46 	F
	ld c,c			;dc1f	49 	I
	ld c,h			;dc20	4c 	L
	ld d,b			;dc21	50 	P
ldc22h:
	ld d,e			;dc22	53 	S
	ld d,(hl)			;dc23	56 	V
	ld e,c			;dc24	59 	Y
	ld e,h			;dc25	5c 	\
	ld h,b			;dc26	60 	`
	ld h,e			;dc27	63 	c
	ld h,(hl)			;dc28	66 	f
ldc29h:
	ld e,(iy+002h)		;dc29	fd 5e 02 	. ^ .
	ld d,000h		;dc2c	16 00 	. .
	ld hl,ldd8dh		;dc2e	21 8d dd 	! . .
	add hl,de			;dc31	19 	.
	ld a,(hl)			;dc32	7e 	~
	ld (0bebfh),a		;dc33	32 bf be 	2 . .
	ld a,(0be08h)		;dc36	3a 08 be 	: . .
	cp 008h		;dc39	fe 08 	. .
	jr z,ldcb0h		;dc3b	28 73 	( s
	ld d,000h		;dc3d	16 00 	. .
	ld e,a			;dc3f	5f 	_
ldc40h:
	push de			;dc40	d5 	.
	push iy		;dc41	fd e5 	. .
	call sub_dd3eh		;dc43	cd 3e dd 	. > .
	pop iy		;dc46	fd e1 	. .
	pop de			;dc48	d1 	.
	inc d			;dc49	14 	.
	cp 0efh		;dc4a	fe ef 	. .
ldc4ch:
	jp z,lda0ah		;dc4c	ca 0a da 	. . .
	ld a,d			;dc4f	7a 	z
	cp (iy+00fh)		;dc50	fd be 0f 	. . .
	jr nz,ldc40h		;dc53	20 eb 	  .
	ld a,(iy+002h)		;dc55	fd 7e 02 	. ~ .
	cp 002h		;dc58	fe 02 	. .
	jp nz,lda14h		;dc5a	c2 14 da 	. . .
	jr ldcb8h		;dc5d	18 59 	. Y
ldc5fh:
	ld a,(iy+002h)		;dc5f	fd 7e 02 	. ~ .
	cp 002h		;dc62	fe 02 	. .
	jp nz,MSG_BAD_FORMAT		;dc64	c2 16 fc 	. . .
	ld a,(0be08h)		;dc67	3a 08 be 	: . .
	cp 008h		;dc6a	fe 08 	. .
	jp z,MSG_BAD_FORMAT		;dc6c	ca 16 fc 	. . .
	ld d,000h		;dc6f	16 00 	. .
	ld a,(0be08h)		;dc71	3a 08 be 	: . .
	ld e,a			;dc74	5f 	_
ldc75h:
	ld (iy+012h),000h		;dc75	fd 36 12 00 	. 6 . .
	ld a,081h		;dc79	3e 81 	> .
	ld (0bebfh),a		;dc7b	32 bf be 	2 . .
	push de			;dc7e	d5 	.
	push iy		;dc7f	fd e5 	. .
	call sub_dd3eh		;dc81	cd 3e dd 	. > .
	pop iy		;dc84	fd e1 	. .
	pop de			;dc86	d1 	.
	cp 0efh		;dc87	fe ef 	. .
	jp z,lda0ah		;dc89	ca 0a da 	. . .
	ld a,(iy+012h)		;dc8c	fd 7e 12 	. ~ .
	xor 001h		;dc8f	ee 01 	. .
	ld (iy+012h),a		;dc91	fd 77 12 	. w .
	ld a,08bh		;dc94	3e 8b 	> .
	ld (0bebfh),a		;dc96	32 bf be 	2 . .
	push de			;dc99	d5 	.
	push iy		;dc9a	fd e5 	. .
	call sub_dd3eh		;dc9c	cd 3e dd 	. > .
	pop iy		;dc9f	fd e1 	. .
	pop de			;dca1	d1 	.
	cp 0efh		;dca2	fe ef 	. .
	jp z,lda0ah		;dca4	ca 0a da 	. . .
	inc d			;dca7	14 	.
	ld a,d			;dca8	7a 	z
	cp (iy+00fh)		;dca9	fd be 0f 	. . .
	jr nz,ldc75h		;dcac	20 c7 	  .
	jr ldcb8h		;dcae	18 08 	. .
ldcb0h:
	ld a,(iy+002h)		;dcb0	fd 7e 02 	. ~ .
	cp 002h		;dcb3	fe 02 	. .
	jp nz,MSG_BAD_FORMAT		;dcb5	c2 16 fc 	. . .
ldcb8h:
	call sub_ef31h		;dcb8	cd 31 ef 	. 1 .
	push hl			;dcbb	e5 	.
	pop ix		;dcbc	dd e1 	. .
	ld a,(iy+00fh)		;dcbe	fd 7e 0f 	. ~ .
	ld (ix+000h),a		;dcc1	dd 77 00 	. w .
	ld a,(iy+010h)		;dcc4	fd 7e 10 	. ~ .
	ld (ix+001h),a		;dcc7	dd 77 01 	. w .
	inc ix		;dcca	dd 23 	. #
	inc ix		;dccc	dd 23 	. #
	ld b,010h		;dcce	06 10 	. .
	ld a,020h		;dcd0	3e 20 	>
ldcd2h:
	ld (ix+000h),a		;dcd2	dd 77 00 	. w .
	inc ix		;dcd5	dd 23 	. #
	djnz ldcd2h		;dcd7	10 f9 	. .
	ld (ix+000h),001h		;dcd9	dd 36 00 01 	. 6 . .
	inc ix		;dcdd	dd 23 	. #
	xor a			;dcdf	af 	.
	ld b,0edh		;dce0	06 ed 	. .
ldce2h:
	ld (ix+000h),a		;dce2	dd 77 00 	. w .
	inc ix		;dce5	dd 23 	. #
	djnz ldce2h		;dce7	10 f9 	. .
	ld (ix+000h),044h		;dce9	dd 36 00 44 	. 6 . D
	ld (ix+001h),052h		;dced	dd 36 01 52 	. 6 . R
	inc ix		;dcf1	dd 23 	. #
	inc ix		;dcf3	dd 23 	. #
	ld (ix+000h),000h		;dcf5	dd 36 00 00 	. 6 . .
	ld (ix+001h),081h		;dcf9	dd 36 01 81 	. 6 . .
	inc ix		;dcfd	dd 23 	. #
	inc ix		;dcff	dd 23 	. #
	ld b,0e0h		;dd01	06 e0 	. .
	ld a,0c0h		;dd03	3e c0 	> .
ldd05h:
	ld (ix+000h),a		;dd05	dd 77 00 	. w .
	inc ix		;dd08	dd 23 	. #
	djnz ldd05h		;dd0a	10 f9 	. .
	ld (ix+000h),001h		;dd0c	dd 36 00 01 	. 6 . .
	ld bc,00081h		;dd10	01 81 00 	. . .
	call sub_efbch		;dd13	cd bc ef 	. . .
	xor a			;dd16	af 	.
	call sub_d9f0h		;dd17	cd f0 d9 	. . .
	ld a,081h		;dd1a	3e 81 	> .
	call sub_d9f7h		;dd1c	cd f7 d9 	. . .
	ld a,0ffh		;dd1f	3e ff 	> .
	ld (iy+040h),a		;dd21	fd 77 40 	. w @
	ret			;dd24	c9 	.
ldd25h:
	ld a,(ix+000h)		;dd25	dd 7e 00 	. ~ .
	ld (0bebfh),a		;dd28	32 bf be 	2 . .
	ld a,(ix+002h)		;dd2b	dd 7e 02 	. ~ .
	ld (iy+012h),a		;dd2e	fd 77 12 	. w .
	ld d,(ix+004h)		;dd31	dd 56 04 	. V .
	ld e,(ix+006h)		;dd34	dd 5e 06 	. ^ .
	ld (iy+WS_DRIVE_NUMBER),e		;dd37	fd 73 04 	. s .
	ld (iy+WS_CURRENT_DRIVE_LETTER),008h		;dd3a	fd 36 03 08 	. 6 . .
sub_dd3eh:

	;Check for ESC pressed
	ld a,042h		;dd3e	3e 42 	> B
	call KM_TEST_KEY		;dd40	cd 1e bb 	. . .
	ld a,0efh		;dd43	3e ef 	> .
	ret nz			;dd45	c0 	.
	push de			;dd46	d5 	.
	ld a,e			;dd47	7b 	{
	call sub_f143h		;dd48	cd 43 f1 	. C .
	and 004h		;dd4b	e6 04 	. .
	pop de			;dd4d	d1 	.
	ld c,001h		;dd4e	0e 01 	. .
	jr nz,ldd53h		;dd50	20 01 	  .
	dec c			;dd52	0d 	.
ldd53h:
	ld hl,0be85h		;dd53	21 85 be 	! . .
	ld (hl),086h		;dd56	36 86 	6 .
	ld (0be83h),hl		;dd58	22 83 be 	" . .
	ld hl,ROM_SELECT_DESELECT_RELOCATED		;dd5b	21 c0 be 	! . .
	push hl			;dd5e	e5 	.
	ld a,(0bebfh)		;dd5f	3a bf be 	: . .
	ld ix,ldd83h		;dd62	dd 21 83 dd 	. ! . .
	ld b,00ah		;dd66	06 0a 	. .
ldd68h:
	ld (hl),d			;dd68	72 	r
	inc hl			;dd69	23 	#
	ld (hl),c			;dd6a	71 	q
	inc hl			;dd6b	23 	#
	ld a,(0bebfh)		;dd6c	3a bf be 	: . .
	add a,(ix+000h)		;dd6f	dd 86 00 	. . .
	ld (hl),a			;dd72	77 	w
	inc hl			;dd73	23 	#
	ld (hl),002h		;dd74	36 02 	6 .
	inc hl			;dd76	23 	#
	inc ix		;dd77	dd 23 	. #
	djnz ldd68h		;dd79	10 ed 	. .
	pop hl			;dd7b	e1 	.
	call sub_d48ch		;dd7c	cd 8c d4 	. . .
	ret c			;dd7f	d8 	.
	ld a,0efh		;dd80	3e ef 	> .
	ret			;dd82	c9 	.
ldd83h:
	nop			;dd83	00 	.
	inc bc			;dd84	03 	.
	ld b,001h		;dd85	06 01 	. .
	inc b			;dd87	04 	.
	rlca			;dd88	07 	.
	ld (bc),a			;dd89	02 	.
	dec b			;dd8a	05 	.
	ex af,af'			;dd8b	08 	. ;adding a ' to make my editor happy.
	add hl,bc			;dd8c	09 	.
ldd8dh:
	ld bc,08141h		;dd8d	01 41 81 	. A .
	pop bc			;dd90	c1 	.
	adc a,e			;dd91	8b 	.

;=======================================================================
RSX_TDUMP:
;=======================================================================
	ld a,001h		;dd92	3e 01 	> .
	ld (0bef0h),a		;dd94	32 f0 be 	2 . .
	ld (0bef1h),a		;dd97	32 f1 be 	2 . .
	call SCR_GET_MODE		;dd9a	cd 11 bc 	. . .
	ld e,a			;dd9d	5f 	_
	ld d,000h		;dd9e	16 00 	. .
	ld hl,lde23h		;dda0	21 23 de 	! # .
	add hl,de			;dda3	19 	.
	ld a,(hl)			;dda4	7e 	~
	ld (0bef2h),a		;dda5	32 f2 be 	2 . .
	call TXT_GET_CURSOR		;dda8	cd 78 bb 	. x .
	ld a,l			;ddab	7d 	}
	ld (0bef3h),a		;ddac	32 f3 be 	2 . .
	ld a,h			;ddaf	7c 	|
	ld (0bef4h),a		;ddb0	32 f4 be 	2 . .
lddb3h:
	ld a,(0bef0h)		;ddb3	3a f0 be 	: . .
	ld h,a			;ddb6	67 	g
	ld a,(0bef1h)		;ddb7	3a f1 be 	: . .
	ld l,a			;ddba	6f 	o
	call TXT_SET_CURSOR		;ddbb	cd 75 bb 	. u .
	call TXT_RD_CHAR		;ddbe	cd 60 bb 	. ` .
	jr c,lddc5h		;ddc1	38 02 	8 .
	ld a,020h		;ddc3	3e 20 	>
lddc5h:
	call MC_PRINT_CHAR		;ddc5	cd 2b bd 	. + .
	push af			;ddc8	f5 	.

	;Check for ESC being pressed
	ld a,042h		;ddc9	3e 42 	> B
	call CHECK_FOR_KEY_PRESSED		;ddcb	cd 59 c2 	. Y .

	jr nz,lddf6h		;ddce	20 26 	  &
	pop af			;ddd0	f1 	.
	jr nc,lddc5h		;ddd1	30 f2 	0 .
	ld a,(0bef0h)		;ddd3	3a f0 be 	: . .
	inc a			;ddd6	3c 	<
	ld (0bef0h),a		;ddd7	32 f0 be 	2 . .
	ld b,a			;ddda	47 	G
	ld a,(0bef2h)		;dddb	3a f2 be 	: . .
	cp b			;ddde	b8 	.
	jr nz,lddb3h		;dddf	20 d2 	  .
	ld a,001h		;dde1	3e 01 	> .
	ld (0bef0h),a		;dde3	32 f0 be 	2 . .
	ld a,(0bef1h)		;dde6	3a f1 be 	: . .
	inc a			;dde9	3c 	<
	ld (0bef1h),a		;ddea	32 f1 be 	2 . .
	cp 01ah		;dded	fe 1a 	. .
	jr z,lddf8h		;ddef	28 07 	( .
	call sub_de02h		;ddf1	cd 02 de 	. . .
	jr lddb3h		;ddf4	18 bd 	. .
lddf6h:
	pop af			;ddf6	f1 	.
	ret			;ddf7	c9 	.
lddf8h:
	call sub_de02h		;ddf8	cd 02 de 	. . .
	ld hl,(0bef3h)		;ddfb	2a f3 be 	* . .
	call TXT_SET_CURSOR		;ddfe	cd 75 bb 	. u .
	ret			;de01	c9 	.
sub_de02h:
	ld a,00dh		;de02	3e 0d 	> .
	call MC_PRINT_CHAR		;de04	cd 2b bd 	. + .
	push af			;de07	f5 	.

	;Check for ESC being pressed
	ld a,042h		;de08	3e 42 	> B
	call CHECK_FOR_KEY_PRESSED		;de0a	cd 59 c2 	. Y .
	jr nz,lddf6h		;de0d	20 e7 	  .

	pop af			;de0f	f1 	.
	jr nc,sub_de02h		;de10	30 f0 	0 .
lde12h:
	ld a,00ah		;de12	3e 0a 	> .
	call MC_PRINT_CHAR		;de14	cd 2b bd 	. + .
	push af			;de17	f5 	.

	;ESC check
	ld a,042h		;de18	3e 42 	> B
	call CHECK_FOR_KEY_PRESSED		;de1a	cd 59 c2 	. Y .
	jr nz,lddf6h		;de1d	20 d7 	  .
	pop af			;de1f	f1 	.
	jr nc,lde12h		;de20	30 f0 	0 .
	ret			;de22	c9 	.
lde23h:
	dec d			;de23	15 	.
	add hl,hl			;de24	29 	)
	ld d,c			;de25	51 	Q

;=======================================================================
RSX_DISK:
;=======================================================================
	call RSX_DISK_OUT		;de26	cd 47 de 	. G .

;=======================================================================
RSX_DISK_IN:
;=======================================================================
	ld b,007h		;de29	06 07 	. .
	ld ix,lde64h		;de2b	dd 21 64 de 	. ! d .
	ld de,00290h		;de2f	11 90 02 	. . .
	push iy		;de32	fd e5 	. .
	pop hl			;de34	e1 	.
	add hl,de			;de35	19 	.
	;So basically CALL CAS_IN_OPEN
	ld de,CAS_IN_OPEN		;de36	11 77 bc 	. w .
	call MAKE_JP_AT_DE_USING_HL		;de39	cd 74 de 	. t .
	ld b,001h		;de3c	06 01 	. .
	;Call CAS_CATALOG
	ld de,CAS_CATALOG		;de3e	11 9b bc 	. . .
	call MAKE_JP_AT_DE_USING_HL		;de41	cd 74 de 	. t .
	jp RSX_FS		;de44	c3 9b d0 	. . .

;=======================================================================
RSX_DISK_OUT:
;=======================================================================
	ld b,005h		;de47	06 05 	. .
	ld ix,lde5ah		;de49	dd 21 5a de 	. ! Z .
	ld de,002c8h		;de4d	11 c8 02 	. . .
	push iy		;de50	fd e5 	. .
	pop hl			;de52	e1 	.
	add hl,de			;de53	19 	.
	ld de,CAS_OUT_OPEN		;de54	11 8c bc 	. . .
	jp MAKE_JP_AT_DE_USING_HL		;de57	c3 74 de 	. t .
lde5ah:
	add hl,hl			;de5a	29 	)
	rst 20h			;de5b	e7 	.
	inc (hl)			;de5c	34 	4
	ex de,hl			;de5d	eb 	.
	inc e			;de5e	1c 	.
	jp (hl)			;de5f	e9 	.
	dec a			;de60	3d 	=
	jp (hl)			;de61	e9 	.
	sbc a,b			;de62	98 	.
	ex de,hl			;de63	eb 	.
lde64h:
	add a,b			;de64	80 	.
	ex (sp),hl			;de65	e3 	.
	ld (bc),a			;de66	02 	.
	push hl			;de67	e5 	.
	jr $-25		;de68	18 e5 	. .
	jr c,$-25		;de6a	38 e5 	8 .
	pop bc			;de6c	c1 	.
	and 0c0h		;de6d	e6 c0 	. .
	and 09bh		;de6f	e6 9b 	. .
	and 0b9h		;de71	e6 b9 	. .
	pop hl			;de73	e1 	.
MAKE_JP_AT_DE_USING_HL:
	;This seems to generate some code.
	;example use:
	; ld de,0bf09h		;d0a6	11 09 bf 	. . .
	; ld b,002h		;d0a9	06 02 	. .
	; call MAKE_JP_AT_DE_USING_HL		;d0ab	cd 74 de 	. t .

	;Generates/patches and address with a jump
	;address to jump is in HL
	;address to patch is in DE
	;So this code makes (DE)=JP (HL)
	ld a,0c3h		;opcode for "jp" ;de74	3e c3 	> .
	ld (de),a			;de76	12 	.
	inc de			;de77	13 	.
	ld a,l			;de78	7d 	}
	ld (de),a			;de79	12 	.
	inc de			;de7a	13 	.
	ld a,h			;de7b	7c 	|
	ld (de),a			;de7c	12 	.
	inc de			;de7d	13 	.
GENERATE_RST18_AT_HL:
	;This patches the address at HL
	;0df is RST 18
	;The RST &18 instruction works a bit differently. Instead of taking parameters in HL and C, it expects them to be in the program flow directly. This is interesting because all registers are passed as is to the called code. You can use them to pass data to the ROM code, bypassing the usual RSX parameter list.
				;
				; 	RST &18;
				; 	DW table
				;         RET
				; table
				; 	DW &C009 ; Address of code to call
				; 	DB &04 ; Rom number to connect

	ld (hl),0dfh	;rst &18	;de7e	36 df 	6 .
	inc hl			;de80	23 	#
	ld a,l			;de81	7d 	}
	add a,003h		;de82	c6 03 	. .
	ld (hl),a			;de84	77 	w
	ld a,h			;de85	7c 	|
	adc a,000h		;de86	ce 00 	. .
	inc hl			;de88	23 	#
	ld (hl),a			;de89	77 	w
	inc hl			;de8a	23 	#
	ld (hl),0c9h	;c9=ret	;de8b	36 c9 	6 .
	inc hl			;de8d	23 	#
	ld a,(ix+000h)		;de8e	dd 7e 00 	. ~ .
	ld (hl),a			;de91	77 	w
	inc ix		;de92	dd 23 	. #
	inc hl			;de94	23 	#
	ld a,(ix+000h)		;de95	dd 7e 00 	. ~ .
	ld (hl),a			;de98	77 	w
	inc ix		;de99	dd 23 	. #
	inc hl			;de9b	23 	#
	ld a,(iy+000h)		;de9c	fd 7e 00 	. ~ .
	ld (hl),a			;de9f	77 	w
	inc hl			;dea0	23 	#
	djnz MAKE_JP_AT_DE_USING_HL		;dea1	10 d1 	. .
	ret			;dea3	c9 	.
	push ix		;dea4	dd e5 	. .
	push hl			;dea6	e5 	.
	push af			;dea7	f5 	.
	call sub_deb0h		;dea8	cd b0 de 	. . .
	pop af			;deab	f1 	.
	pop hl			;deac	e1 	.
	pop ix		;dead	dd e1 	. .
	ret			;deaf	c9 	.
sub_deb0h:
	push af			;deb0	f5 	.
	ld l,(iy+046h)		;deb1	fd 6e 46 	. n F
	ld h,(iy+047h)		;deb4	fd 66 47 	. f G
ldeb7h:
	ld a,(hl)			;deb7	7e 	~
	and 0a0h		;deb8	e6 a0 	. .
	cp 0a0h		;deba	fe a0 	. .
	jr z,ldeb7h		;debc	28 f9 	( .
	pop af			;debe	f1 	.
	bit 0,(iy+011h)		;debf	fd cb 11 46 	. . . F
	call nz,sub_defah		;dec3	c4 fa de 	. . .
	bit 1,(iy+011h)		;dec6	fd cb 11 4e 	. . . N
	call nz,SEND_CHARACTER_TO_PRINTER		;deca	c4 00 df 	. . .
	bit 2,(iy+011h)		;decd	fd cb 11 56 	. . . V
	ret nz			;ded1	c0 	.
	ld hl,(TXT_OUTPUT)		;ded2	2a 5a bb 	* Z .
	push hl			;ded5	e5 	.
	ld hl,(0bb5ch)		;ded6	2a 5c bb 	* \ .
	ld h,a			;ded9	67 	g
	ld a,(iy+05ah)		;deda	fd 7e 5a 	. ~ Z
sub_deddh:
	ld (TXT_OUTPUT),a		;dedd	32 5a bb 	2 Z .
	ld a,(iy+05bh)		;dee0	fd 7e 5b 	. ~ [
	ld (0bb5bh),a		;dee3	32 5b bb 	2 [ .
	ld a,(iy+05ch)		;dee6	fd 7e 5c 	. ~ \
	ld (0bb5ch),a		;dee9	32 5c bb 	2 \ .
	ld a,h			;deec	7c 	|
	call TXT_OUTPUT		;deed	cd 5a bb 	. Z .
	ld a,l			;def0	7d 	}
	ld (0bb5ch),a		;def1	32 5c bb 	2 \ .
	ld a,h			;def4	7c 	|
	pop hl			;def5	e1 	.
	ld (TXT_OUTPUT),hl		;def6	22 5a bb 	" Z .
	ret			;def9	c9 	.
sub_defah:
	push af			;defa	f5 	.
	call CAS_OUT_CHAR		;defb	cd 95 bc 	. . .
	pop af			;defe	f1 	.
	ret			;deff	c9 	.
SEND_CHARACTER_TO_PRINTER:
	;input:
	; A=character to print
	push af			;df00	f5 	.
	;If A=09 (tab?) set char to SPACE
	cp 009h		;df01	fe 09 	. .
	call z,SET_A_TO_020H		;df03	cc 0d df 	. . .
	call MC_PRINT_CHAR		;df06	cd 2b bd 	. + .
	jr nc,CHECK_FOR_ESC		;df09	30 05 	0 .
	pop af			;df0b	f1 	.
	ret			;df0c	c9 	.
SET_A_TO_020H:
;Probably a Space character
	ld a,020h		;df0d	3e 20 	>
	ret			;df0f	c9 	.

CHECK_FOR_ESC:
	;ESC check
	ld a,042h		;df10	3e 42 	> B
	call CHECK_FOR_KEY_PRESSED		;df12	cd 59 c2 	. Y .
	jr nz,ldf1ah		;df15	20 03 	  .
	pop af			;df17	f1 	.
	jr SEND_CHARACTER_TO_PRINTER		;df18	18 e6 	. .
ldf1ah:
	res 1,(iy+011h)		;df1a	fd cb 11 8e 	. . . .
	pop af			;df1e	f1 	.
	ret			;df1f	c9 	.

;=======================================================================
RSX_SPOOL:
;=======================================================================
	and a			;df20	a7 	.
	jr nz,ldf2ah		;df21	20 07 	  .
sub_df23h:
	res 0,(iy+011h)		;df23	fd cb 11 86 	. . . .
	jp CAS_OUT_CLOSE		;df27	c3 8f bc 	. . .
ldf2ah:
	cp 002h		;df2a	fe 02 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;df2c	d2 9f fb 	. . .
	call sub_da80h		;df2f	cd 80 da 	. . .
	ld de,08000h		;df32	11 00 80 	. . .
	call CAS_OUT_OPEN		;df35	cd 8c bc 	. . .
	ret nc			;df38	d0 	.
	set 0,(iy+011h)		;df39	fd cb 11 c6 	. . . .
	jp ldf4fh		;df3d	c3 4f df 	. O .

;=======================================================================
RSX_PRINT:
;=======================================================================
	and a			;df40	a7 	.
	jr z,ldf4bh		;df41	28 08 	( .
	cp 002h		;df43	fe 02 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;df45	d2 9f fb 	. . .
	ld a,(ix+000h)		;df48	dd 7e 00 	. ~ .
ldf4bh:
	ld (iy+011h),a		;df4b	fd 77 11 	. w .
	ret			;df4e	c9 	.
ldf4fh:
	ld b,001h		;df4f	06 01 	. .
	ld ix,ldf62h		;df51	dd 21 62 df 	. ! b .
	ld de,00064h		;df55	11 64 00 	. d .
	push iy		;df58	fd e5 	. .
	pop hl			;df5a	e1 	.
	add hl,de			;df5b	19 	.
	ld de,TXT_OUTPUT		;df5c	11 5a bb 	. Z .
	jp MAKE_JP_AT_DE_USING_HL		;df5f	c3 74 de 	. t .
ldf62h:
	and h			;df62	a4 	.
	sbc a,0feh		;df63	de fe 	. .
	ld (bc),a			;df65	02 	.
	jp nc,MSG_WRONG_PARAMETER_AMT		;df66	d2 97 fb 	. . .
	and a			;df69	a7 	.
	call z,PROMPT_ENTER_NAME		;df6a	cc dc d8 	. . .
	call sub_da1bh		;df6d	cd 1b da 	. . .
	ret nz			;df70	c0 	.
	call sub_da6ah		;df71	cd 6a da 	. j .
	ret nc			;df74	d0 	.
	jp nz,MSG_BAD_DIR		;df75	c2 9b fb 	. . .
	ld ix,0bee0h		;df78	dd 21 e0 be 	. ! . .
	call sub_edb6h		;df7c	cd b6 ed 	. . .
	jp nz,lfb8dh		;df7f	c2 8d fb 	. . .
	call lf07ch		;df82	cd 7c f0 	. | .
	jp nz,MSG_BAD_DIR		;df85	c2 9b fb 	. . .
	ld a,0ffh		;df88	3e ff 	> .
	ld (iy+040h),a		;df8a	fd 77 40 	. w @
	call sub_ed01h		;df8d	cd 01 ed 	. . .
	jp nz,MSG_BAD_DIR		;df90	c2 9b fb 	. . .
	call sub_ed38h		;df93	cd 38 ed 	. 8 .
	jp z,MSG_DIR_ALREADY_EXISTS		;df96	ca be fb 	. . .
	ld a,02ch		;df99	3e 2c 	> ,
	ld (iy+014h),a		;df9b	fd 77 14 	. w .
	call sub_ec18h		;df9e	cd 18 ec 	. . .
	jp nz,MSG_BAD_DIR		;dfa1	c2 9b fb 	. . .
	ld d,(iy+037h)		;dfa4	fd 56 37 	. V 7
	ld e,(iy+038h)		;dfa7	fd 5e 38 	. ^ 8
	ld (hl),d			;dfaa	72 	r
	inc hl			;dfab	23 	#
	ld (hl),e			;dfac	73 	s
	push de			;dfad	d5 	.
	call sub_efbch		;dfae	cd bc ef 	. . .
	pop bc			;dfb1	c1 	.
	ret nz			;dfb2	c0 	.
	push bc			;dfb3	c5 	.
	call sub_ef31h		;dfb4	cd 31 ef 	. 1 .
	ld (hl),044h		;dfb7	36 44 	6 D
	inc hl			;dfb9	23 	#
	ld (hl),052h		;dfba	36 52 	6 R
	inc hl			;dfbc	23 	#
	ld a,(iy+005h)		;dfbd	fd 7e 05 	. ~ .
	ld (hl),a			;dfc0	77 	w
	inc hl			;dfc1	23 	#
	ld a,(iy+006h)		;dfc2	fd 7e 06 	. ~ .
	ld (hl),a			;dfc5	77 	w
	inc hl			;dfc6	23 	#
	ld a,0c0h		;dfc7	3e c0 	> .
	ld b,0e0h		;dfc9	06 e0 	. .
ldfcbh:
	ld (hl),a			;dfcb	77 	w
	inc hl			;dfcc	23 	#
	djnz ldfcbh		;dfcd	10 fc 	. .
ldfcfh:
	ld (hl),a			;dfcf	77 	w
	inc hl			;dfd0	23 	#
	djnz ldfcfh		;dfd1	10 fc 	. .
	ld (hl),001h		;dfd3	36 01 	6 .
	pop bc			;dfd5	c1 	.
	call sub_efbch		;dfd6	cd bc ef 	. . .
	ld a,0ffh		;dfd9	3e ff 	> .
	ld (iy+040h),a		;dfdb	fd 77 40 	. w @
	ret			;dfde	c9 	.

;=======================================================================
RSX_CD:
;=======================================================================
	and a			;dfdf	a7 	.
	jr z,le04ah		;dfe0	28 68 	( h
	cp 001h		;dfe2	fe 01 	. .
	jp nz,MSG_TOO_MANY_PARAMETERS		;dfe4	c2 9f fb 	. . .
	call sub_da6ah		;dfe7	cd 6a da 	. j .
	ret nc			;dfea	d0 	.
	jp nz,MSG_BAD_DIR		;dfeb	c2 9b fb 	. . .
	ld a,b			;dfee	78 	x
	and a			;dfef	a7 	.
	jr z,le010h		;dff0	28 1e 	( .
	ld ix,0bee0h		;dff2	dd 21 e0 be 	. ! . .
	call sub_edb6h		;dff6	cd b6 ed 	. . .
	jr c,le010h		;dff9	38 15 	8 .
	jp nz,lfb8dh		;dffb	c2 8d fb 	. . .
	call sub_ed01h		;dffe	cd 01 ed 	. . .
	ret nz			;e001	c0 	.
	call sub_ed38h		;e002	cd 38 ed 	. 8 .
	jp nz,lfbf7h		;e005	c2 f7 fb 	. . .
	bit 2,(hl)		;e008	cb 56 	. V
	jp z,MSG_BAD_DIR		;e00a	ca 9b fb 	. . .
	call sub_eecfh		;e00d	cd cf ee 	. . .
le010h:
	ld a,(iy+005h)		;e010	fd 7e 05 	. ~ .
	call sub_d9f0h		;e013	cd f0 d9 	. . .
	ld a,(iy+006h)		;e016	fd 7e 06 	. ~ .
	call sub_d9f7h		;e019	cd f7 d9 	. . .
	ld a,(iy+WS_CURRENT_DRIVE_LETTER)		;e01c	fd 7e 03 	. ~ .
	cp 008h		;e01f	fe 08 	. .
	jr nz,le041h		;e021	20 1e 	  .
	ld a,(iy+013h)		;e023	fd 7e 13 	. ~ .
	cp (iy+WS_CURRENT_DRIVE_LETTER)		;e026	fd be 03 	. . .
	ret z			;e029	c8 	.
	ld (iy+WS_CURRENT_DRIVE_LETTER),a		;e02a	fd 77 03 	. w .
	ld hl,0be00h		;e02d	21 00 be 	! . .
	ld e,a			;e030	5f 	_
	ld d,000h		;e031	16 00 	. .
	add hl,de			;e033	19 	.
	ld a,(iy+WS_DRIVE_NUMBER)		;e034	fd 7e 04 	. ~ .
	ld (hl),a			;e037	77 	w
	call le010h		;e038	cd 10 e0 	. . .
	ld a,(iy+WS_DRIVE_NUMBER)		;e03b	fd 7e 04 	. ~ .
	jp ld827h		;e03e	c3 27 d8 	. ' .
le041h:
	ld (iy+013h),a		;e041	fd 77 13 	. w .
	ld a,(iy+WS_DRIVE_NUMBER)		;e044	fd 7e 04 	. ~ .
	jp ld827h		;e047	c3 27 d8 	. ' .
le04ah:
	ld a,(iy+043h)		;e04a	fd 7e 43 	. ~ C
	ld (iy+WS_CURRENT_DRIVE_LETTER),a		;e04d	fd 77 03 	. w .
	ld e,a			;e050	5f 	_
	ld a,(iy+044h)		;e051	fd 7e 44 	. ~ D
	ld (iy+005h),a		;e054	fd 77 05 	. w .
	ld a,(iy+045h)		;e057	fd 7e 45 	. ~ E
	ld (iy+006h),a		;e05a	fd 77 06 	. w .
	ld a,(iy+042h)		;e05d	fd 7e 42 	. ~ B
	ld (iy+WS_DRIVE_NUMBER),a		;e060	fd 77 04 	. w .
	ld hl,0be00h		;e063	21 00 be 	! . .
	ld d,000h		;e066	16 00 	. .
	add hl,de			;e068	19 	.
	ld (hl),a			;e069	77 	w
	jr le010h		;e06a	18 a4 	. .

;=======================================================================
RSX_LINK:
;=======================================================================
	cp 002h		;e06c	fe 02 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;e06e	d2 9f fb 	. . .
	and a			;e071	a7 	.
	call z,PROMPT_ENTER_NAME		;e072	cc dc d8 	. . .
	call sub_da6ah		;e075	cd 6a da 	. j .
	ret nc			;e078	d0 	.
	jp nz,MSG_BAD_DIR		;e079	c2 9b fb 	. . .
	ld ix,0bee1h		;e07c	dd 21 e1 be 	. ! . .
	call sub_edb6h		;e080	cd b6 ed 	. . .
	jp nz,lfb8dh		;e083	c2 8d fb 	. . .
	call sub_ed01h		;e086	cd 01 ed 	. . .
	ret nz			;e089	c0 	.
	call sub_ed38h		;e08a	cd 38 ed 	. 8 .
	jp nz,lfbf7h		;e08d	c2 f7 fb 	. . .
	ld de,0bee0h		;e090	11 e0 be 	. . .
	ld bc,RAM_LAM		;e093	01 20 00 	.   .
	ldir		;e096	ed b0 	. .
	ld a,(0befdh)		;e098	3a fd be 	: . .
	cp 0feh		;e09b	fe fe 	. .
	jp nz,MSG_CANT_LINK_TO_LINKED_FILE		;e09d	c2 d6 fb 	. . .
	ld l,(iy+WS_CURRENT_DRIVE_LETTER)		;e0a0	fd 6e 03 	. n .
	ld a,(iy+013h)		;e0a3	fd 7e 13 	. ~ .
	cp l			;e0a6	bd 	.
	ld l,(iy+WS_DRIVE_NUMBER)		;e0a7	fd 6e 04 	. n .
	call z,sub_e0cfh		;e0aa	cc cf e0 	. . .
	ld a,l			;e0ad	7d 	}
	ld (0befdh),a		;e0ae	32 fd be 	2 . .
	call sub_da62h		;e0b1	cd 62 da 	. b .
	call sub_ec09h		;e0b4	cd 09 ec 	. . .
	ret nz			;e0b7	c0 	.
	ld de,lffeeh		;e0b8	11 ee ff 	. . .
	add hl,de			;e0bb	19 	.
	ex de,hl			;e0bc	eb 	.
	ld hl,0bee0h		;e0bd	21 e0 be 	! . .
	push bc			;e0c0	c5 	.
	ld bc,RAM_LAM		;e0c1	01 20 00 	.   .
	ldir		;e0c4	ed b0 	. .
	pop bc			;e0c6	c1 	.
	call sub_efbch		;e0c7	cd bc ef 	. . .
	ld (iy+040h),0ffh		;e0ca	fd 36 40 ff 	. 6 @ .
	ret			;e0ce	c9 	.
sub_e0cfh:
	ld l,0ffh		;e0cf	2e ff 	. .
	ret			;e0d1	c9 	.

;=======================================================================
RSX_RMDIR:
;=======================================================================
	cp 002h		;e0d2	fe 02 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;e0d4	d2 9f fb 	. . .
	and a			;e0d7	a7 	.
	call z,PROMPT_ENTER_NAME		;e0d8	cc dc d8 	. . .
	call sub_da6ah		;e0db	cd 6a da 	. j .
	ret nc			;e0de	d0 	.
	jp nz,MSG_BAD_FILE_NAME		;e0df	c2 8e fb 	. . .
	ld ix,0bee0h		;e0e2	dd 21 e0 be 	. ! . .
	call sub_edb6h		;e0e6	cd b6 ed 	. . .
	jp nz,lfb8dh		;e0e9	c2 8d fb 	. . .
	call sub_ef8bh		;e0ec	cd 8b ef 	. . .
	call sub_ed01h		;e0ef	cd 01 ed 	. . .
	ret nz			;e0f2	c0 	.
	call sub_ed38h		;e0f3	cd 38 ed 	. 8 .
	jp nz,lfbf7h		;e0f6	c2 f7 fb 	. . .
le0f9h:
	bit 2,(hl)		;e0f9	cb 56 	. V
	jr z,le124h		;e0fb	28 27 	( '
	push hl			;e0fd	e5 	.
	push bc			;e0fe	c5 	.
	push ix		;e0ff	dd e5 	. .
	call sub_e135h		;e101	cd 35 e1 	. 5 .
	pop ix		;e104	dd e1 	. .
	pop bc			;e106	c1 	.
	pop hl			;e107	e1 	.
	ret nz			;e108	c0 	.
	push hl			;e109	e5 	.
	call sub_ef39h		;e10a	cd 39 ef 	. 9 .
	pop hl			;e10d	e1 	.
	ret nz			;e10e	c0 	.
	ld de,RAM_LAM		;e10f	11 20 00 	.   .
	add hl,de			;e112	19 	.
	call sub_ed38h		;e113	cd 38 ed 	. 8 .
	jr z,le0f9h		;e116	28 e1 	( .
sub_e118h:
	call sub_f010h		;e118	cd 10 f0 	. . .
	ld (iy+03fh),0ffh		;e11b	fd 36 3f ff 	. 6 ? .
	ld (iy+040h),0ffh		;e11f	fd 36 40 ff 	. 6 @ .
	ret			;e123	c9 	.
le124h:
	inc hl			;e124	23 	#
	ld b,010h		;e125	06 10 	. .
	call PRINT_STRING		;e127	cd 92 d8 	. . .
	call sub_e118h		;e12a	cd 18 e1 	. . .
	ld a,020h		;e12d	3e 20 	>
	call TXT_OUTPUT		;e12f	cd 5a bb 	. Z .
	jp MSG_BAD_DIR		;e132	c3 9b fb 	. . .
sub_e135h:
	ld (0bedch),hl		;e135	22 dc be 	" . .
	ld (0bedeh),bc		;e138	ed 43 de be 	. C . .
	ld de,00012h		;e13c	11 12 00 	. . .
	add hl,de			;e13f	19 	.
	ld b,(hl)			;e140	46 	F
	inc hl			;e141	23 	#
	ld c,(hl)			;e142	4e 	N
	ld de,0000ah		;e143	11 0a 00 	. . .
	add hl,de			;e146	19 	.
	ld a,(hl)			;e147	7e 	~
	cp 0feh		;e148	fe fe 	. .
	jr nz,le188h		;e14a	20 3c 	  <
	call sub_ef39h		;e14c	cd 39 ef 	. 9 .
	ret nz			;e14f	c0 	.
	ld a,(hl)			;e150	7e 	~
	cp 044h		;e151	fe 44 	. D
	jp nz,MSG_CORRUPTED_DISC		;e153	c2 1a fc 	. . .
	inc hl			;e156	23 	#
	ld a,(hl)			;e157	7e 	~
	cp 052h		;e158	fe 52 	. R
	jp nz,MSG_CORRUPTED_DISC		;e15a	c2 1a fc 	. . .
	inc hl			;e15d	23 	#
	inc hl			;e15e	23 	#
	inc hl			;e15f	23 	#
	push bc			;e160	c5 	.
	ld de,0bef0h		;e161	11 f0 be 	. . .
	ld b,010h		;e164	06 10 	. .
	ld a,03fh		;e166	3e 3f 	> ?
le168h:
	ld (de),a			;e168	12 	.
	inc de			;e169	13 	.
	djnz le168h		;e16a	10 fc 	. .
	ld ix,0bef0h		;e16c	dd 21 f0 be 	. ! . .
	call sub_ed38h		;e170	cd 38 ed 	. 8 .
	pop bc			;e173	c1 	.
	jp z,MSG_DIR_NOT_EMPTY		;e174	ca d2 fb 	. . .
le177h:
	call sub_ef6ch		;e177	cd 6c ef 	. l .
	ret nz			;e17a	c0 	.
	ld de,001e4h		;e17b	11 e4 01 	. . .
	add hl,de			;e17e	19 	.
	ld a,(hl)			;e17f	7e 	~
	cp 002h		;e180	fe 02 	. .
	jr z,le19ch		;e182	28 18 	( .
	call sub_f093h		;e184	cd 93 f0 	. . .
	ret nz			;e187	c0 	.
le188h:
	ld bc,(0bedeh)		;e188	ed 4b de be 	. K . .
	ld a,b			;e18c	78 	x
	and a			;e18d	a7 	.
	jr z,le1a7h		;e18e	28 17 	( .
le190h:
	call sub_ef39h		;e190	cd 39 ef 	. 9 .
	ret nz			;e193	c0 	.
	ld hl,(0bedch)		;e194	2a dc be 	* . .
	set 7,(hl)		;e197	cb fe 	. .
	jp sub_efbch		;e199	c3 bc ef 	. . .
le19ch:
	inc hl			;e19c	23 	#
	ld d,(hl)			;e19d	56 	V
	inc hl			;e19e	23 	#
	ld e,(hl)			;e19f	5e 	^
	push de			;e1a0	d5 	.
	call sub_f093h		;e1a1	cd 93 f0 	. . .
	pop bc			;e1a4	c1 	.
	jr le177h		;e1a5	18 d0 	. .
le1a7h:
	ld a,c			;e1a7	79 	y
	cp 081h		;e1a8	fe 81 	. .
	jr nz,le190h		;e1aa	20 e4 	  .
	ld hl,(0bedch)		;e1ac	2a dc be 	* . .
	set 7,(hl)		;e1af	cb fe 	. .
	ld bc,00200h		;e1b1	01 00 02 	. . .
	add hl,bc			;e1b4	09 	.
	set 7,(hl)		;e1b5	cb fe 	. .
	xor a			;e1b7	af 	.
	ret			;e1b8	c9 	.
	ld l,(iy+017h)		;e1b9	fd 6e 17 	. n .
	ld a,(iy+018h)		;e1bc	fd 7e 18 	. ~ .
	or l			;e1bf	b5 	.
	jp nz,lda0fh		;e1c0	c2 0f da 	. . .
	push de			;e1c3	d5 	.
	call sub_e374h		;e1c4	cd 74 e3 	. t .
	call sub_da62h		;e1c7	cd 62 da 	. b .
	call sub_d9a0h		;e1ca	cd a0 d9 	. . .
	pop de			;e1cd	d1 	.
	ret nc			;e1ce	d0 	.
	jp nz,MSG_DISC_NOT_FORMATTED		;e1cf	c2 22 fc 	. " .
	cp 081h		;e1d2	fe 81 	. .
	jp nz,le242h		;e1d4	c2 42 e2 	. B .
le1d7h:
	ld ix,0bef0h		;e1d7	dd 21 f0 be 	. ! . .
	push ix		;e1db	dd e5 	. .
	pop hl			;e1dd	e1 	.
	ld b,010h		;e1de	06 10 	. .
le1e0h:
	ld (hl),03fh		;e1e0	36 3f 	6 ?
	inc hl			;e1e2	23 	#
	djnz le1e0h		;e1e3	10 fb 	. .
	ld (iy+040h),0ffh		;e1e5	fd 36 40 ff 	. 6 @ .
le1e9h:
	call sub_ef8bh		;e1e9	cd 8b ef 	. . .
	jp nc,lda14h		;e1ec	d2 14 da 	. . .
	push hl			;e1ef	e5 	.
	call sub_e32ch		;e1f0	cd 2c e3 	. , .
	pop hl			;e1f3	e1 	.
	ld a,(hl)			;e1f4	7e 	~
	ld (iy+00fh),a		;e1f5	fd 77 0f 	. w .
	inc hl			;e1f8	23 	#
	ld a,(hl)			;e1f9	7e 	~
	ld (iy+010h),a		;e1fa	fd 77 10 	. w .
	inc hl			;e1fd	23 	#
	ld b,010h		;e1fe	06 10 	. .
	call PRINT_STRING		;e200	cd 92 d8 	. . .
	call PRINT_CR_LF		;e203	cd 7d d9 	. } .
	call sub_e26fh		;e206	cd 6f e2 	. o .
	jp nz,lda0ah		;e209	c2 0a da 	. . .
	ld hl,lff43h		;e20c	21 43 ff 	! C .
	call DISPLAY_MSG		;e20f	cd 6a d9 	. j .
	call sub_0f0d0h		;e212	cd d0 f0 	. . .
	push ix		;e215	dd e5 	. .
	pop hl			;e217	e1 	.
	srl h		;e218	cb 3c 	. <
	rr l		;e21a	cb 1d 	. .
	res 7,h		;e21c	cb bc 	. .
	call sub_d8ffh		;e21e	cd ff d8 	. . .
	push ix		;e221	dd e5 	. .
	pop hl			;e223	e1 	.
	bit 0,l		;e224	cb 45 	. E
	jr z,le232h		;e226	28 0a 	( .
	ld a,02eh		;e228	3e 2e 	> .
	call TXT_OUTPUT		;e22a	cd 5a bb 	. Z .
	ld a,035h		;e22d	3e 35 	> 5
	call TXT_OUTPUT		;e22f	cd 5a bb 	. Z .
le232h:
	ld a,020h		;e232	3e 20 	>
	call TXT_OUTPUT		;e234	cd 5a bb 	. Z .
	ld a,04bh		;e237	3e 4b 	> K
	call TXT_OUTPUT		;e239	cd 5a bb 	. Z .
	call PRINT_CR_LF		;e23c	cd 7d d9 	. } .
	jp lda0ah		;e23f	c3 0a da 	. . .
le242h:
	ld (0beeeh),de		;e242	ed 53 ee be 	. S . .
	ld ix,0bef0h		;e246	dd 21 f0 be 	. ! . .
	call IX_STORE_DISC		;e24a	cd 01 e9 	. . .
	;Next bit stows "IN"
	ld (ix+006h),049h		;e24d	dd 36 06 49 	. 6 . I
	ld (ix+007h),0ceh		;e251	dd 36 07 ce 	. 6 . .
	ld hl,0bef0h		;e255	21 f0 be 	! . .
	call EXECUTE_RSX_COMMAND		;e258	cd e8 c3 	. . .
	jp nc,MSG_CANT_FIND_AMSDOS		;e25b	d2 b6 fb 	. . .
	xor a			;e25e	af 	.
	call KL_FAR_PCHL		;e25f	cd 1b 00 	. . .
	ld de,(0beeeh)		;e262	ed 5b ee be 	. [ . .
	call CAS_CATALOG		;e266	cd 9b bc 	. . .
	call RSX_DISK_IN		;e269	cd 29 de 	. ) .
	jp lda0ah		;e26c	c3 0a da 	. . .
sub_e26fh:
	ld b,(iy+005h)		;e26f	fd 46 05 	. F .
	ld c,(iy+006h)		;e272	fd 4e 06 	. N .
	ld a,c			;e275	79 	y
	cp 081h		;e276	fe 81 	. .
	jr nz,le287h		;e278	20 0d 	  .
	ld a,b			;e27a	78 	x
	and a			;e27b	a7 	.
	jr nz,le287h		;e27c	20 09 	  .
	call sub_ef6ch		;e27e	cd 6c ef 	. l .
	ld de,00100h		;e281	11 00 01 	. . .
	add hl,de			;e284	19 	.
	jr le28dh		;e285	18 06 	. .
le287h:
	call sub_ef6ch		;e287	cd 6c ef 	. l .
	jp nc,le30bh		;e28a	d2 0b e3 	. . .
le28dh:
	ld a,(hl)			;e28d	7e 	~
	cp 044h		;e28e	fe 44 	. D
	jp nz,le30bh		;e290	c2 0b e3 	. . .
	inc hl			;e293	23 	#
	ld a,(hl)			;e294	7e 	~
	cp 052h		;e295	fe 52 	. R
	jp nz,le30bh		;e297	c2 0b e3 	. . .
	inc hl			;e29a	23 	#
	inc hl			;e29b	23 	#
	inc hl			;e29c	23 	#
le29dh:
	call sub_ed38h		;e29d	cd 38 ed 	. 8 .
	jp nz,lda14h		;e2a0	c2 14 da 	. . .
	ld a,(hl)			;e2a3	7e 	~
	and 0c0h		;e2a4	e6 c0 	. .
	jr nz,le305h		;e2a6	20 5d 	  ]
	ld a,(hl)			;e2a8	7e 	~
	push af			;e2a9	f5 	.
	call sub_db19h		;e2aa	cd 19 db 	. . .
	ld c,a			;e2ad	4f 	O
	ld b,010h		;e2ae	06 10 	. .
	inc hl			;e2b0	23 	#
	call PRINT_STRING		;e2b1	cd 92 d8 	. . .
	pop af			;e2b4	f1 	.
	cp 02ch		;e2b5	fe 2c 	. ,
	jr nz,le2cah		;e2b7	20 11 	  .
	;Print "Dir" one letter at a time
	ld a,044h		;e2b9	3e 44 	> D
	call TXT_OUTPUT		;e2bb	cd 5a bb 	. Z .
	ld a,069h		;e2be	3e 69 	> i
	call TXT_OUTPUT		;e2c0	cd 5a bb 	. Z .
	ld a,072h		;e2c3	3e 72 	> r
	call TXT_OUTPUT		;e2c5	cd 5a bb 	. Z .
	jr le2e8h		;e2c8	18 1e 	. .
le2cah:
	bit 0,c		;e2ca	cb 41 	. A
	ld a,052h		;e2cc	3e 52 	> R
	call z,SET_A_TO_020H		;e2ce	cc 0d df 	. . .
	call TXT_OUTPUT		;e2d1	cd 5a bb 	. Z .
	bit 1,c		;e2d4	cb 49 	. I
	ld a,057h		;e2d6	3e 57 	> W
	call z,SET_A_TO_020H		;e2d8	cc 0d df 	. . .
	call TXT_OUTPUT		;e2db	cd 5a bb 	. Z .
	bit 2,c		;e2de	cb 51 	. Q
	ld a,058h		;e2e0	3e 58 	> X
	call nz,SET_A_TO_020H		;e2e2	c4 0d df 	. . .
	call TXT_OUTPUT		;e2e5	cd 5a bb 	. Z .
le2e8h:
	push ix		;e2e8	dd e5 	. .
	push hl			;e2ea	e5 	.
	pop ix		;e2eb	dd e1 	. .
	ld a,(ix+00ch)		;e2ed	dd 7e 0c 	. ~ .
	cp 0feh		;e2f0	fe fe 	. .
	ld a,021h		;e2f2	3e 21 	> !
	call z,SET_A_TO_020H		;e2f4	cc 0d df 	. . .
	call TXT_OUTPUT		;e2f7	cd 5a bb 	. Z .
	call NEWLINE_IF_NO_DISPLAY_SPACE		;e2fa	cd ab d8 	. . .
	pop ix		;e2fd	dd e1 	. .
	ld de,0000fh		;e2ff	11 0f 00 	. . .
	add hl,de			;e302	19 	.
	jr le29dh		;e303	18 98 	. .
le305h:
	ld de,RAM_LAM		;e305	11 20 00 	.   .
	add hl,de			;e308	19 	.
	jr le29dh		;e309	18 92 	. .
le30bh:
	call sub_e311h		;e30b	cd 11 e3 	. . .
	jp lda0ah		;e30e	c3 0a da 	. . .
sub_e311h:
	call sub_e32ch		;e311	cd 2c e3 	. , .
	ld hl,lff04h		;e314	21 04 ff 	! . .
	call DISPLAY_MSG		;e317	cd 6a d9 	. j .
	ld a,081h		;e31a	3e 81 	> .
	call sub_d9f7h		;e31c	cd f7 d9 	. . .
	xor a			;e31f	af 	.
	call sub_d9f0h		;e320	cd f0 d9 	. . .
	ld (iy+03fh),0ffh		;e323	fd 36 3f ff 	. 6 ? .
	ld (iy+040h),0ffh		;e327	fd 36 40 ff 	. 6 @ .
	ret			;e32b	c9 	.
sub_e32ch:
	ld hl,lff3bh		;e32c	21 3b ff 	! ; .
	call DISPLAY_MSG		;e32f	cd 6a d9 	. j .
	ld a,(iy+WS_CURRENT_DRIVE_LETTER)		;e332	fd 7e 03 	. ~ .
	cp 008h		;e335	fe 08 	. .
	jr z,le35bh		;e337	28 22 	( "
	add a,041h		;e339	c6 41 	. A
	call TXT_OUTPUT		;e33b	cd 5a bb 	. Z .
	ld a,(iy+WS_CURRENT_DRIVE_LETTER)		;e33e	fd 7e 03 	. ~ .
	ld l,(iy+WS_DRIVE_NUMBER)		;e341	fd 6e 04 	. n .
	cp l			;e344	bd 	.
	jp z,PRINT_CR_LF		;e345	ca 7d d9 	. } .
le348h:
	ld a,028h		;e348	3e 28 	> (
	call TXT_OUTPUT		;e34a	cd 5a bb 	. Z .
	ld a,l			;e34d	7d 	}
	push de			;e34e	d5 	.
	call sub_d90fh		;e34f	cd 0f d9 	. . .
	pop de			;e352	d1 	.
	ld a,029h		;e353	3e 29 	> )
	call TXT_OUTPUT		;e355	cd 5a bb 	. Z .
	jp PRINT_CR_LF		;e358	c3 7d d9 	. } .
le35bh:
	ld a,073h		;e35b	3e 73 	> s
	call TXT_OUTPUT		;e35d	cd 5a bb 	. Z .
	ld a,079h		;e360	3e 79 	> y
	call TXT_OUTPUT		;e362	cd 5a bb 	. Z .
	ld a,073h		;e365	3e 73 	> s
	call TXT_OUTPUT		;e367	cd 5a bb 	. Z .
	ld a,020h		;e36a	3e 20 	>
	call TXT_OUTPUT		;e36c	cd 5a bb 	. Z .
	ld l,(iy+WS_DRIVE_NUMBER)		;e36f	fd 6e 04 	. n .
	jr le348h		;e372	18 d4 	. .
sub_e374h:
	bit 0,(iy+011h)		;e374	fd cb 11 46 	. . . F
	ret z			;e378	c8 	.
	res 0,(iy+011h)		;e379	fd cb 11 86 	. . . .
	jp CAS_OUT_CLOSE		;e37d	c3 8f bc 	. . .
	push de			;e380	d5 	.
	pop ix		;e381	dd e1 	. .
	ld a,b			;e383	78 	x
	ld (0bee0h),de		;e384	ed 53 e0 be 	. S . .
	and a			;e388	a7 	.
	call z,PRINT_ENTER_NAME		;e389	cc be d8 	. . .
	ld (0bee2h),hl		;e38c	22 e2 be 	" . .
	ld (0bee4h),a		;e38f	32 e4 be 	2 . .
	call sub_da62h		;e392	cd 62 da 	. b .
	ld de,0000fh		;e395	11 0f 00 	. . .
	add ix,de		;e398	dd 19 	. .
	ld de,(0bee2h)		;e39a	ed 5b e2 be 	. [ . .
	call sub_da21h		;e39e	cd 21 da 	. ! .
	jp nz,lda14h		;e3a1	c2 14 da 	. . .
	ex de,hl			;e3a4	eb 	.
	call sub_edb6h		;e3a5	cd b6 ed 	. . .
	jp c,MSG_BAD_FILE_NAME		;e3a8	da 8e fb 	. . .
	jp nz,lda14h		;e3ab	c2 14 da 	. . .
	call sub_d9a0h		;e3ae	cd a0 d9 	. . .
	jp nc,lda14h		;e3b1	d2 14 da 	. . .
	jp nz,MSG_DISC_NOT_FORMATTED		;e3b4	c2 22 fc 	. " .
	cp 081h		;e3b7	fe 81 	. .
	jp nz,le4a3h		;e3b9	c2 a3 e4 	. . .
	push ix		;e3bc	dd e5 	. .
	ld hl,(0bc78h)		;e3be	2a 78 bc 	* x .
	push hl			;e3c1	e5 	.
	call RSX_DISK_IN		;e3c2	cd 29 de 	. ) .
	pop hl			;e3c5	e1 	.
	ld (0bc78h),hl		;e3c6	22 78 bc 	" x .
	pop ix		;e3c9	dd e1 	. .
	call sub_ed01h		;e3cb	cd 01 ed 	. . .
	jp nz,lda14h		;e3ce	c2 14 da 	. . .
	call sub_ed38h		;e3d1	cd 38 ed 	. 8 .
	jp nz,lfbf7h		;e3d4	c2 f7 fb 	. . .
	ld a,(hl)			;e3d7	7e 	~
	bit 2,a		;e3d8	cb 57 	. W
	jp nz,MSG_BAD_FILE		;e3da	c2 26 fc 	. & .
	call sub_db19h		;e3dd	cd 19 db 	. . .
	ld e,a			;e3e0	5f 	_
	xor 004h		;e3e1	ee 04 	. .
	and 005h		;e3e3	e6 05 	. .
	jp z,MSG_ACCESS_DENIED		;e3e5	ca a7 fb 	. . .
	ld a,e			;e3e8	7b 	{
	push af			;e3e9	f5 	.
	inc hl			;e3ea	23 	#
	push hl			;e3eb	e5 	.
	push iy		;e3ec	fd e5 	. .
	pop ix		;e3ee	dd e1 	. .
	ld de,001c9h		;e3f0	11 c9 01 	. . .
	add ix,de		;e3f3	dd 19 	. .
	push ix		;e3f5	dd e5 	. .
	pop de			;e3f7	d1 	.
	pop hl			;e3f8	e1 	.
	ld bc,SIDE_CALL_RST_2		;e3f9	01 10 00 	. . .
	ldir		;e3fc	ed b0 	. .
	inc hl			;e3fe	23 	#
	ld bc,00007h		;e3ff	01 07 00 	. . .
	ldir		;e402	ed b0 	. .
	inc de			;e404	13 	.
	inc de			;e405	13 	.
	inc de			;e406	13 	.
	ld bc,00002h		;e407	01 02 00 	. . .
	ldir		;e40a	ed b0 	. .
	inc hl			;e40c	23 	#
	inc hl			;e40d	23 	#
	ld e,(iy+WS_DRIVE_NUMBER)		;e40e	fd 5e 04 	. ^ .
	ld a,(hl)			;e411	7e 	~
	cp 0feh		;e412	fe fe 	. .
	jr nc,le417h		;e414	30 01 	0 .
	ld e,a			;e416	5f 	_
le417h:
	ld (ix-00ch),e		;e417	dd 73 f4 	. s .
	ld a,(ix+013h)		;e41a	dd 7e 13 	. ~ .
	ld (ix+018h),a		;e41d	dd 77 18 	. w .
	ld a,(ix+014h)		;e420	dd 7e 14 	. ~ .
	ld (ix+019h),a		;e423	dd 77 19 	. w .
	ld a,(ix+010h)		;e426	dd 7e 10 	. ~ .
	ld (ix-002h),a		;e429	dd 77 fe 	. w .
	ld a,(ix+011h)		;e42c	dd 7e 11 	. ~ .
	ld (ix-001h),a		;e42f	dd 77 ff 	. w .
	ld (iy+039h),001h		;e432	fd 36 39 01 	. 6 9 .
	pop af			;e436	f1 	.
	ld (ix-007h),a		;e437	dd 77 f9 	. w .
	ld a,(iy+005h)		;e43a	fd 7e 05 	. ~ .
	ld (ix-00bh),a		;e43d	dd 77 f5 	. w .
	ld a,(iy+006h)		;e440	fd 7e 06 	. ~ .
	ld (ix-00ah),a		;e443	dd 77 f6 	. w .
	ld hl,(0bee0h)		;e446	2a e0 be 	* . .
	ld (iy+019h),l		;e449	fd 75 19 	. u .
	ld (iy+01ah),h		;e44c	fd 74 1a 	. t .
	push ix		;e44f	dd e5 	. .
	pop hl			;e451	e1 	.
	ld (iy+017h),l		;e452	fd 75 17 	. u .
	ld (iy+018h),h		;e455	fd 74 18 	. t .
	ld e,(ix+015h)		;e458	dd 5e 15 	. ^ .
	ld d,(ix+016h)		;e45b	dd 56 16 	. V .
	ld c,(ix+013h)		;e45e	dd 4e 13 	. N .
	ld b,(ix+014h)		;e461	dd 46 14 	. F .
	xor a			;e464	af 	.
	ld (iy+01fh),a		;e465	fd 77 1f 	. w .
	ld (iy+020h),a		;e468	fd 77 20 	. w
	call lda0ah		;e46b	cd 0a da 	. . .
	ld a,(ix+012h)		;e46e	dd 7e 12 	. ~ .
	ret			;e471	c9 	.
le472h:
	ld ix,0bef0h		;e472	dd 21 f0 be 	. ! . .
	inc hl			;e476	23 	#
	dec b			;e477	05 	.
le478h:
	ld a,b			;e478	78 	x
	and a			;e479	a7 	.
	jp z,MSG_UNKNOWN_FILE_SYSTEM		;e47a	ca c6 fb 	. . .
	ld a,(hl)			;e47d	7e 	~
	cp 061h		;e47e	fe 61 	. a
	call nc,FUNC_SUBTRACT_32		;e480	d4 9d d9 	. . .
	ld (ix+000h),a		;e483	dd 77 00 	. w .
	inc ix		;e486	dd 23 	. #
	inc hl			;e488	23 	#
	dec b			;e489	05 	.
	cp 02dh		;e48a	fe 2d 	. -
	jr nz,le478h		;e48c	20 ea
	;next bit stows ".IN" 	  .
	ld (ix-001h),02eh		;e48e	dd 36 ff 2e 	. 6 . .
	ld (ix+000h),049h		;e492	dd 36 00 49 	. 6 . I
	ld (ix+001h),0ceh		;e496	dd 36 01 ce 	. 6 . .
	ld (0bee2h),hl		;e49a	22 e2 be 	" . .
	ld a,b			;e49d	78 	x
	ld (0bee4h),a		;e49e	32 e4 be 	2 . .
	jr le4b2h		;e4a1	18 0f 	. .
le4a3h:
	ld ix,0bef0h		;e4a3	dd 21 f0 be 	. ! . .
	call IX_STORE_DISC		;e4a7	cd 01 e9 	. . .
	;Next bit stows "IN"
	ld (ix+006h),049h		;e4aa	dd 36 06 49 	. 6 . I
	ld (ix+007h),0ceh		;e4ae	dd 36 07 ce 	. 6 . .
le4b2h:
	ld hl,0bef0h		;e4b2	21 f0 be 	! . .
	call EXECUTE_RSX_COMMAND		;e4b5	cd e8 c3 	. . .
	jp nc,MSG_UNKNOWN_FILE_SYSTEM		;e4b8	d2 c6 fb 	. . .
	ld (0bf02h),hl		;e4bb	22 02 bf 	" . .
	ld hl,(0bc9ch)		;e4be	2a 9c bc 	* . .
	push hl			;e4c1	e5 	.
	ld a,(CAS_CATALOG)		;e4c2	3a 9b bc 	: . .
	ld l,a			;e4c5	6f 	o
	push hl			;e4c6	e5 	.
	ld hl,(0bf02h)		;e4c7	2a 02 bf 	* . .
	xor a			;e4ca	af 	.
	call KL_FAR_PCHL		;e4cb	cd 1b 00 	. . .
	ld hl,(0bee2h)		;e4ce	2a e2 be 	* . .
	ld a,(0bee4h)		;e4d1	3a e4 be 	: . .
	ld b,a			;e4d4	47 	G
	call sub_dabah		;e4d5	cd ba da 	. . .
	ld de,(0bee0h)		;e4d8	ed 5b e0 be 	. [ . .
	call CAS_IN_OPEN		;e4dc	cd 77 bc 	. w .
	ld (0bf02h),hl		;e4df	22 02 bf 	" . .
	pop hl			;e4e2	e1 	.
	push af			;e4e3	f5 	.
	ld a,0c3h		;e4e4	3e c3 	> .
	ld (CAS_IN_OPEN),a		;e4e6	32 77 bc 	2 w .
	ld a,(iy+04ah)		;e4e9	fd 7e 4a 	. ~ J
	ld (0bc78h),a		;e4ec	32 78 bc 	2 x .
	ld a,(iy+04bh)		;e4ef	fd 7e 4b 	. ~ K
	ld (0bc79h),a		;e4f2	32 79 bc 	2 y .
	ld a,l			;e4f5	7d 	}
	ld (CAS_CATALOG),a		;e4f6	32 9b bc 	2 . .
	pop af			;e4f9	f1 	.
	pop hl			;e4fa	e1 	.
	ld (0bc9ch),hl		;e4fb	22 9c bc 	" . .
	ld hl,(0bf02h)		;e4fe	2a 02 bf 	* . .
	ret			;e501	c9 	.
	ld l,(iy+017h)		;e502	fd 6e 17 	. n .
	ld h,(iy+018h)		;e505	fd 66 18 	. f .
	ld a,l			;e508	7d 	}
	or h			;e509	b4 	.
	jp z,lda0fh		;e50a	ca 0f da 	. . .
	push hl			;e50d	e5 	.
	pop ix		;e50e	dd e1 	. .
	ld a,(ix-00fh)		;e510	dd 7e f1 	. ~ .
	cp 052h		;e513	fe 52 	. R
	call z,sub_eb34h		;e515	cc 34 eb 	. 4 .
	ld l,(iy+017h)		;e518	fd 6e 17 	. n .
	ld h,(iy+018h)		;e51b	fd 66 18 	. f .
	push hl			;e51e	e5 	.
	pop ix		;e51f	dd e1 	. .
	ld a,h			;e521	7c 	|
	or l			;e522	b5 	.
	jp z,lda0ah		;e523	ca 0a da 	. . .
	ld (ix-00fh),043h		;e526	dd 36 f1 43 	. 6 . C
	xor a			;e52a	af 	.
	ld (iy+017h),a		;e52b	fd 77 17 	. w .
	ld (iy+018h),a		;e52e	fd 77 18 	. w .
	ld (iy+039h),000h		;e531	fd 36 39 00 	. 6 9 .
	jp lda0ah		;e535	c3 0a da 	. . .
	ld a,(iy+017h)		;e538	fd 7e 17 	. ~ .
	or (iy+018h)		;e53b	fd b6 18 	. . .
	jp z,le696h		;e53e	ca 96 e6 	. . .
	ld a,(iy+039h)		;e541	fd 7e 39 	. ~ 9
	and a			;e544	a7 	.
	jp z,le696h		;e545	ca 96 e6 	. . .
	push hl			;e548	e5 	.
	push de			;e549	d5 	.
	push bc			;e54a	c5 	.
	ld l,(iy+017h)		;e54b	fd 6e 17 	. n .
	ld h,(iy+018h)		;e54e	fd 66 18 	. f .
	push hl			;e551	e5 	.
	pop ix		;e552	dd e1 	. .
	bit 2,(ix-007h)		;e554	dd cb f9 56 	. . . V
	jr nz,le560h		;e558	20 06 	  .
	call MSG_ACCESS_DENIED		;e55a	cd a7 fb 	. . .
	jp le5feh		;e55d	c3 fe e5 	. . .
le560h:
	ld a,(iy+039h)		;e560	fd 7e 39 	. ~ 9
	cp 001h		;e563	fe 01 	. .
	jr nz,le572h		;e565	20 0b 	  .
	call sub_e604h		;e567	cd 04 e6 	. . .
	jr z,le572h		;e56a	28 06 	( .
	pop bc			;e56c	c1 	.
	pop de			;e56d	d1 	.
	pop hl			;e56e	e1 	.
	jp lda14h		;e56f	c3 14 da 	. . .
le572h:
	ld l,(ix-004h)		;e572	dd 6e fc 	. n .
	ld h,(ix-003h)		;e575	dd 66 fd 	. f .
	ld a,h			;e578	7c 	|
	or l			;e579	b5 	.
	jp z,le5feh		;e57a	ca fe e5 	. . .
	dec hl			;e57d	2b 	+
	ld (ix-004h),l		;e57e	dd 75 fc 	. u .
	ld (ix-003h),h		;e581	dd 74 fd 	. t .
	ld l,(ix-00eh)		;e584	dd 6e f2 	. n .
	ld h,(ix-00dh)		;e587	dd 66 f3 	. f .
	ld a,h			;e58a	7c 	|
	or l			;e58b	b5 	.
	jr nz,le5b6h		;e58c	20 28 	  (
	ld a,(ix-00fh)		;e58e	dd 7e f1 	. ~ .
	cp 052h		;e591	fe 52 	. R
	call z,sub_e5a6h		;e593	cc a6 e5 	. . .
	push ix		;e596	dd e5 	. .
	pop hl			;e598	e1 	.
	ld c,(ix+003h)		;e599	dd 4e 03 	. N .
	ld b,(ix+002h)		;e59c	dd 46 02 	. F .
	call sub_e64fh		;e59f	cd 4f e6 	. O .
	jr nz,le5feh		;e5a2	20 5a 	  Z
	jr le572h		;e5a4	18 cc 	. .
sub_e5a6h:
	bit 2,(iy+03ah)		;e5a6	fd cb 3a 56 	. . : V
	ret z			;e5aa	c8 	.
	call sub_e648h		;e5ab	cd 48 e6 	. H .
	call sub_eac1h		;e5ae	cd c1 ea 	. . .
	res 2,(iy+03ah)		;e5b1	fd cb 3a 96 	. . : .
	ret			;e5b5	c9 	.
le5b6h:
	dec hl			;e5b6	2b 	+
	ld (ix-00eh),l		;e5b7	dd 75 f2 	. u .
	ld (ix-00dh),h		;e5ba	dd 74 f3 	. t .
	ld l,(ix-006h)		;e5bd	dd 6e fa 	. n .
	ld h,(ix-005h)		;e5c0	dd 66 fb 	. f .
	ld a,(hl)			;e5c3	7e 	~
	inc hl			;e5c4	23 	#
	ld (ix-006h),l		;e5c5	dd 75 fa 	. u .
	ld (ix-005h),h		;e5c8	dd 74 fb 	. t .
	ld l,(ix-009h)		;e5cb	dd 6e f7 	. n .
	ld h,(ix-008h)		;e5ce	dd 66 f8 	. f .
	inc hl			;e5d1	23 	#
	ld (ix-009h),l		;e5d2	dd 75 f7 	. u .
	ld (ix-008h),h		;e5d5	dd 74 f8 	. t .
	call sub_e5f0h		;e5d8	cd f0 e5 	. . .
	pop bc			;e5db	c1 	.
	ld d,a			;e5dc	57 	W
	cp 01ah		;e5dd	fe 1a 	. .
	jr z,le5e8h		;e5df	28 07 	( .
	call lda0ah		;e5e1	cd 0a da 	. . .
	ld a,d			;e5e4	7a 	z
	pop de			;e5e5	d1 	.
	pop hl			;e5e6	e1 	.
	ret			;e5e7	c9 	.
le5e8h:
	call lda0fh		;e5e8	cd 0f da 	. . .
	ld a,01ah		;e5eb	3e 1a 	> .
	pop de			;e5ed	d1 	.
	pop hl			;e5ee	e1 	.
	ret			;e5ef	c9 	.
sub_e5f0h:
	ld l,(iy+01fh)		;e5f0	fd 6e 1f 	. n .
	ld h,(iy+020h)		;e5f3	fd 66 20 	. f
	inc hl			;e5f6	23 	#
	ld (iy+020h),h		;e5f7	fd 74 20 	. t
	ld (iy+01fh),l		;e5fa	fd 75 1f 	. u .
	ret			;e5fd	c9 	.
le5feh:
	pop bc			;e5fe	c1 	.
	pop de			;e5ff	d1 	.
	pop hl			;e600	e1 	.
	jp le696h		;e601	c3 96 e6 	. . .
sub_e604h:
	ld l,(iy+017h)		;e604	fd 6e 17 	. n .
	ld h,(iy+018h)		;e607	fd 66 18 	. f .
	ld de,lfff1h		;e60a	11 f1 ff 	. . .
	add hl,de			;e60d	19 	.
	ld e,(iy+019h)		;e60e	fd 5e 19 	. ^ .
	ld d,(iy+01ah)		;e611	fd 56 1a 	. V .
	push de			;e614	d5 	.
	ld bc,0000fh		;e615	01 0f 00 	. . .
	ldir		;e618	ed b0 	. .
	pop hl			;e61a	e1 	.
	ld de,0000fh		;e61b	11 0f 00 	. . .
	add hl,de			;e61e	19 	.
	ld (iy+017h),l		;e61f	fd 75 17 	. u .
	ld (iy+018h),h		;e622	fd 74 18 	. t .
	push hl			;e625	e5 	.
	pop ix		;e626	dd e1 	. .
	ld a,(ix-00bh)		;e628	dd 7e f5 	. ~ .
	ld (ix+000h),a		;e62b	dd 77 00 	. w .
	ld a,(ix-00ah)		;e62e	dd 7e f6 	. ~ .
	ld (ix+001h),a		;e631	dd 77 01 	. w .
	ld (ix-00fh),049h		;e634	dd 36 f1 49 	. 6 . I
	ld (iy+039h),002h		;e638	fd 36 39 02 	. 6 9 .
	ld b,(ix-002h)		;e63c	dd 46 fe 	. F .
	ld c,(ix-001h)		;e63f	dd 4e ff 	. N .
	call sub_e64fh		;e642	cd 4f e6 	. O .
	ret nz			;e645	c0 	.
	xor a			;e646	af 	.
	ret			;e647	c9 	.
sub_e648h:
	ld e,(ix-00ch)		;e648	dd 5e f4 	. ^ .
	ld (iy+WS_DRIVE_NUMBER),e		;e64b	fd 73 04 	. s .
	ret			;e64e	c9 	.
sub_e64fh:
	call sub_e648h		;e64f	cd 48 e6 	. H .
	push ix		;e652	dd e5 	. .
	call sub_ef61h		;e654	cd 61 ef 	. a .
	pop ix		;e657	dd e1 	. .
	ret nz			;e659	c0 	.
	ld (ix-002h),b		;e65a	dd 70 fe 	. p .
	ld (ix-001h),c		;e65d	dd 71 ff 	. q .
	ld hl,00200h		;e660	21 00 02 	! . .
	ld a,(ix+003h)		;e663	dd 7e 03 	. ~ .
	cp 002h		;e666	fe 02 	. .
	jr nc,le670h		;e668	30 06 	0 .
	ld l,(ix+002h)		;e66a	dd 6e 02 	. n .
	ld h,(ix+003h)		;e66d	dd 66 03 	. f .
le670h:
	ld (ix-004h),l		;e670	dd 75 fc 	. u .
	ld (ix-003h),h		;e673	dd 74 fd 	. t .
	push ix		;e676	dd e5 	. .
	pop hl			;e678	e1 	.
	inc hl			;e679	23 	#
	inc hl			;e67a	23 	#
	inc hl			;e67b	23 	#
	inc hl			;e67c	23 	#
	ld (ix-006h),l		;e67d	dd 75 fa 	. u .
	ld (ix-005h),h		;e680	dd 74 fb 	. t .
	ld (ix-009h),000h		;e683	dd 36 f7 00 	. 6 . .
	ld (ix-008h),000h		;e687	dd 36 f8 00 	. 6 . .
	ld hl,001fch		;e68b	21 fc 01 	! . .
	ld (ix-00eh),l		;e68e	dd 75 f2 	. u .
	ld (ix-00dh),h		;e691	dd 74 f3 	. t .
	xor a			;e694	af 	.
	ret			;e695	c9 	.
le696h:
	ld a,00fh		;e696	3e 0f 	> .
	cp 000h		;e698	fe 00 	. .
	ret			;e69a	c9 	.
	ld a,(iy+039h)		;e69b	fd 7e 39 	. ~ 9
	and a			;e69e	a7 	.
	jr z,le696h		;e69f	28 f5 	( .
	push hl			;e6a1	e5 	.
	push bc			;e6a2	c5 	.
	push de			;e6a3	d5 	.
	cp 001h		;e6a4	fe 01 	. .
	call z,sub_e604h		;e6a6	cc 04 e6 	. . .
	pop de			;e6a9	d1 	.
	pop bc			;e6aa	c1 	.
	ld l,(iy+017h)		;e6ab	fd 6e 17 	. n .
	ld h,(iy+018h)		;e6ae	fd 66 18 	. f .
	push hl			;e6b1	e5 	.
	pop ix		;e6b2	dd e1 	. .
	pop hl			;e6b4	e1 	.
	ld a,(ix-004h)		;e6b5	dd 7e fc 	. ~ .
	or (ix-003h)		;e6b8	dd b6 fd 	. . .
	jr z,le696h		;e6bb	28 d9 	( .
	jp lda0ah		;e6bd	c3 0a da 	. . .
	ret			;e6c0	c9 	.
	ld a,(iy+039h)		;e6c1	fd 7e 39 	. ~ 9
	cp 001h		;e6c4	fe 01 	. .
	jp nz,lda0fh		;e6c6	c2 0f da 	. . .
	call sub_e648h		;e6c9	cd 48 e6 	. H .
	ex de,hl			;e6cc	eb 	.
	ld l,(iy+017h)		;e6cd	fd 6e 17 	. n .
	ld h,(iy+018h)		;e6d0	fd 66 18 	. f .
	push hl			;e6d3	e5 	.
	pop ix		;e6d4	dd e1 	. .
le6d6h:
	ld c,(ix-001h)		;e6d6	dd 4e ff 	. N .
	ld b,(ix-002h)		;e6d9	dd 46 fe 	. F .
	push de			;e6dc	d5 	.
	call sub_ef39h		;e6dd	cd 39 ef 	. 9 .
	pop de			;e6e0	d1 	.
	jp nz,lda14h		;e6e1	c2 14 da 	. . .
	inc hl			;e6e4	23 	#
sub_e6e5h:
	inc hl			;e6e5	23 	#
	ld a,(hl)			;e6e6	7e 	~
	ld (ix-002h),a		;e6e7	dd 77 fe 	. w .
	inc hl			;e6ea	23 	#
	ld a,(hl)			;e6eb	7e 	~
	ld (ix-001h),a		;e6ec	dd 77 ff 	. w .
	inc hl			;e6ef	23 	#
	ld bc,001fch		;e6f0	01 fc 01 	. . .
	ld a,(ix-001h)		;e6f3	dd 7e ff 	. ~ .
	cp 002h		;e6f6	fe 02 	. .
	push af			;e6f8	f5 	.
	jr nc,le702h		;e6f9	30 07 	0 .
	ld b,a			;e6fb	47 	G
	ld c,(ix-002h)		;e6fc	dd 4e fe 	. N .
	or c			;e6ff	b1 	.
	jr z,le711h		;e700	28 0f 	( .
le702h:
	ld a,(iy+002h)		;e702	fd 7e 02 	. ~ .
	cp 0c0h		;e705	fe c0 	. .
	jr c,le70fh		;e707	38 06 	8 .
	push bc			;e709	c5 	.
	ld b,07fh		;e70a	06 7f 	. 
	out (c),a		;e70c	ed 79 	. y
	pop bc			;e70e	c1 	.
le70fh:
	ldir		;e70f	ed b0 	. .
le711h:
	pop af			;e711	f1 	.
	jr nc,le6d6h		;e712	30 c2 	0 .
	ld a,(iy+002h)		;e714	fd 7e 02 	. ~ .
	cp 0c0h		;e717	fe c0 	. .
	jr c,le720h		;e719	38 05 	8 .
	ld bc,07fc0h		;e71b	01 c0 7f 	. . 
	out (c),c		;e71e	ed 49 	. I
le720h:
	ld l,(ix+01ah)		;e720	dd 6e 1a 	. n .
	ld h,(ix+01bh)		;e723	dd 66 1b 	. f .
	jp lda0ah		;e726	c3 0a da 	. . .
	push de			;e729	d5 	.
	pop ix		;e72a	dd e1 	. .
	ld (0bee0h),de		;e72c	ed 53 e0 be 	. S . .
	ld (0bee2h),hl		;e730	22 e2 be 	" . .
	ld a,b			;e733	78 	x
	and a			;e734	a7 	.
	jp z,MSG_BAD_FILE_NAME		;e735	ca 8e fb 	. . .
	ld (0bee4h),a		;e738	32 e4 be 	2 . .
	ld de,0000fh		;e73b	11 0f 00 	. . .
	add ix,de		;e73e	dd 19 	. .
	ld a,(iy+01bh)		;e740	fd 7e 1b 	. ~ .
	or (iy+01ch)		;e743	fd b6 1c 	. . .
	jp nz,lda0fh		;e746	c2 0f da 	. . .
	push ix		;e749	dd e5 	. .
	pop de			;e74b	d1 	.
	ld (iy+01dh),e		;e74c	fd 73 1d 	. s .
	ld (iy+01eh),d		;e74f	fd 72 1e 	. r .
	push iy		;e752	fd e5 	. .
	pop ix		;e754	dd e1 	. .
	ld de,001f8h		;e756	11 f8 01 	. . .
	add ix,de		;e759	dd 19 	. .
	call sub_da62h		;e75b	cd 62 da 	. b .
	ld de,(0bee2h)		;e75e	ed 5b e2 be 	. [ . .
	ld a,(0bee4h)		;e762	3a e4 be 	: . .
	ld b,a			;e765	47 	G
	call sub_da21h		;e766	cd 21 da 	. ! .
	jp nz,lda14h		;e769	c2 14 da 	. . .
	ex de,hl			;e76c	eb 	.
	call sub_edb6h		;e76d	cd b6 ed 	. . .
	jp c,MSG_BAD_FILE_NAME		;e770	da 8e fb 	. . .
	jp nz,lda0ah		;e773	c2 0a da 	. . .
	bit 7,e		;e776	cb 7b 	. {
	jp nz,MSG_BAD_FILE_NAME		;e778	c2 8e fb 	. . .
	call sub_d9a0h		;e77b	cd a0 d9 	. . .
	jp nc,lda14h		;e77e	d2 14 da 	. . .
	jp nz,MSG_DISC_NOT_FORMATTED		;e781	c2 22 fc 	. " .
	cp 081h		;e784	fe 81 	. .
	jp nz,le842h		;e786	c2 42 e8 	. B .
	push ix		;e789	dd e5 	. .
	ld hl,(0bc8dh)		;e78b	2a 8d bc 	* . .
	push hl			;e78e	e5 	.
	call RSX_DISK_OUT		;e78f	cd 47 de 	. G .
	pop hl			;e792	e1 	.
	ld (0bc8dh),hl		;e793	22 8d bc 	" . .
	pop ix		;e796	dd e1 	. .
	call sub_ef8bh		;e798	cd 8b ef 	. . .
	ret nz			;e79b	c0 	.
	call sub_ed01h		;e79c	cd 01 ed 	. . .
	ret nz			;e79f	c0 	.
	call sub_ed38h		;e7a0	cd 38 ed 	. 8 .
	jp z,le888h		;e7a3	ca 88 e8 	. . .
le7a6h:
	ld (ix+012h),016h		;e7a6	dd 36 12 16 	. 6 . .
	xor a			;e7aa	af 	.
	ld (ix+013h),a		;e7ab	dd 77 13 	. w .
	ld (ix+014h),a		;e7ae	dd 77 14 	. w .
	ld (ix+018h),a		;e7b1	dd 77 18 	. w .
	ld (ix+019h),a		;e7b4	dd 77 19 	. w .
	ld (ix+017h),0ffh		;e7b7	dd 36 17 ff 	. 6 . .
	ld l,(iy+005h)		;e7bb	fd 6e 05 	. n .
	ld (ix+01ch),l		;e7be	dd 75 1c 	. u .
	ld (ix-00bh),l		;e7c1	dd 75 f5 	. u .
	ld l,(iy+006h)		;e7c4	fd 6e 06 	. n .
	ld (ix+01dh),l		;e7c7	dd 75 1d 	. u .
	ld (ix-00ah),l		;e7ca	dd 75 f6 	. u .
	ld (ix+01eh),a		;e7cd	dd 77 1e 	. w .
	ld (ix+01fh),a		;e7d0	dd 77 1f 	. w .
	ld (ix-006h),a		;e7d3	dd 77 fa 	. w .
	ld (ix-005h),a		;e7d6	dd 77 fb 	. w .
	ld (ix-00eh),0fch		;e7d9	dd 36 f2 fc 	. 6 . .
	ld (ix-00dh),001h		;e7dd	dd 36 f3 01 	. 6 . .
	ld (ix-002h),a		;e7e1	dd 77 fe 	. w .
	ld (ix-001h),a		;e7e4	dd 77 ff 	. w .
	ld (ix-009h),a		;e7e7	dd 77 f7 	. w .
	ld (ix-008h),a		;e7ea	dd 77 f8 	. w .
	ld l,(iy+WS_DRIVE_NUMBER)		;e7ed	fd 6e 04 	. n .
	ld (ix-00ch),l		;e7f0	dd 75 f4 	. u .
	ld (ix-004h),a		;e7f3	dd 77 fc 	. w .
	ld (ix-003h),a		;e7f6	dd 77 fd 	. w .
	ld (iy+03ah),001h		;e7f9	fd 36 3a 01 	. 6 : .
	push ix		;e7fd	dd e5 	. .
	pop hl			;e7ff	e1 	.
	ld (iy+01bh),l		;e800	fd 75 1b 	. u .
	ld (iy+01ch),h		;e803	fd 74 1c 	. t .
	ld (ix-00fh),04fh		;e806	dd 36 f1 4f 	. 6 . O
	jp lda0ah		;e80a	c3 0a da 	. . .
le80dh:
	inc hl			;e80d	23 	#
	dec b			;e80e	05 	.
	ld ix,0bef0h		;e80f	dd 21 f0 be 	. ! . .
le813h:
	ld a,b			;e813	78 	x
	and a			;e814	a7 	.
	jp z,MSG_UNKNOWN_FILE_SYSTEM		;e815	ca c6 fb 	. . .
	ld a,(hl)			;e818	7e 	~
	cp 061h		;e819	fe 61 	. a
	call nc,FUNC_SUBTRACT_32		;e81b	d4 9d d9 	. . .
	ld (ix+000h),a		;e81e	dd 77 00 	. w .
	inc hl			;e821	23 	#
	dec b			;e822	05 	.
	inc ix		;e823	dd 23 	. #
	cp 02dh		;e825	fe 2d 	. -
	jr nz,le813h		;e827	20 ea 	  .
	ld (0bee2h),hl		;e829	22 e2 be 	" . .
	ld a,b			;e82c	78 	x
	ld (0bee4h),a		;e82d	32 e4 be 	2 . .
	;Next bit stows ".OUT" but T is T+0x80
	ld (ix-001h),02eh		;e830	dd 36 ff 2e 	. 6 . .
	ld (ix+000h),04fh		;e834	dd 36 00 4f 	. 6 . O
	ld (ix+001h),055h		;e838	dd 36 01 55 	. 6 . U
	ld (ix+002h),0d4h		;e83c	dd 36 02 d4 	. 6 . .
	jr le855h		;e840	18 13 	. .
le842h:
	ld ix,0bef0h		;e842	dd 21 f0 be 	. ! . .
	;Stores\ DISC into IX
	call IX_STORE_DISC		;e846	cd 01 e9 	. . .
	;Next bit stows "OUT" but T is T+0x80
	ld (ix+006h),04fh		;e849	dd 36 06 4f 	. 6 . O
	ld (ix+007h),055h		;e84d	dd 36 07 55 	. 6 . U
	ld (ix+008h),0d4h		;e851	dd 36 08 d4 	. 6 . .
le855h:
	ld hl,0bef0h		;e855	21 f0 be 	! . .
	call EXECUTE_RSX_COMMAND		;e858	cd e8 c3 	. . .
	jp nc,MSG_UNKNOWN_FILE_SYSTEM		;e85b	d2 c6 fb 	. . .
	xor a			;e85e	af 	.
	call KL_FAR_PCHL		;e85f	cd 1b 00 	. . .
	ld hl,(0bee2h)		;e862	2a e2 be 	* . .
	ld a,(0bee4h)		;e865	3a e4 be 	: . .
	ld b,a			;e868	47 	G
	call sub_dabah		;e869	cd ba da 	. . .
	ret nz			;e86c	c0 	.
	ld de,(0bee0h)		;e86d	ed 5b e0 be 	. [ . .
	call CAS_OUT_OPEN		;e871	cd 8c bc 	. . .
	push af			;e874	f5 	.
	ld a,0c3h		;e875	3e c3 	> .
	ld (CAS_OUT_OPEN),a		;e877	32 8c bc 	2 . .
	ld a,(iy+04ch)		;e87a	fd 7e 4c 	. ~ L
	ld (0bc8dh),a		;e87d	32 8d bc 	2 . .
	ld a,(iy+04dh)		;e880	fd 7e 4d 	. ~ M
	ld (0bc8eh),a		;e883	32 8e bc 	2 . .
	pop af			;e886	f1 	.
	ret			;e887	c9 	.
le888h:
	bit 2,(hl)		;e888	cb 56 	. V
	jp nz,MSG_DIR_ALREADY_EXISTS		;e88a	c2 be fb 	. . .
	ld a,(iy+016h)		;e88d	fd 7e 16 	. ~ .
	cp 002h		;e890	fe 02 	. .
	jr z,le8b7h		;e892	28 23 	( #
	cp 001h		;e894	fe 01 	. .
	jr z,le8f7h		;e896	28 5f 	( _
	ld hl,lff13h		;e898	21 13 ff 	! . .
	call DISPLAY_MSG		;e89b	cd 6a d9 	. j .
	call KM_WAIT_KEY		;e89e	cd 18 bb 	. . .
	;Was A pressed?
	cp 061h		;e8a1	fe 61 	. a
	call nc,FUNC_SUBTRACT_32		;e8a3	d4 9d d9 	. . .
	call TXT_OUTPUT		;e8a6	cd 5a bb 	. Z .
	push af			;e8a9	f5 	.
	call PRINT_CR_LF		;e8aa	cd 7d d9 	. } .
	pop af			;e8ad	f1 	.
	;E key, maybe?
	cp 045h		;e8ae	fe 45 	. E
	jr z,le8f7h		;e8b0	28 45 	( E
	;B key maybe/?
	cp 042h		;e8b2	fe 42 	. B
	jp nz,lda14h		;e8b4	c2 14 da 	. . .
le8b7h:
	ld de,0bee0h		;e8b7	11 e0 be 	. . .
	push ix		;e8ba	dd e5 	. .
	push ix		;e8bc	dd e5 	. .
	push ix		;e8be	dd e5 	. .
	pop hl			;e8c0	e1 	.
	ld bc,SIDE_CALL_RST_2		;e8c1	01 10 00 	. . .
	ldir		;e8c4	ed b0 	. .
	pop hl			;e8c6	e1 	.
	ld bc,SIDE_CALL_RST_2		;e8c7	01 10 00 	. . .
	ldir		;e8ca	ed b0 	. .
	ld a,042h		;e8cc	3e 42 	> B
	ld (0beech),a		;e8ce	32 ec be 	2 . .
	ld a,041h		;e8d1	3e 41 	> A
	ld (0beedh),a		;e8d3	32 ed be 	2 . .
	ld a,04bh		;e8d6	3e 4b 	> K
	ld (0beeeh),a		;e8d8	32 ee be 	2 . .
	ld a,020h		;e8db	3e 20 	>
	ld (0beefh),a		;e8dd	32 ef be 	2 . .
	ld ix,0bee0h		;e8e0	dd 21 e0 be 	. ! . .
	call sub_d6ddh		;e8e4	cd dd d6 	. . .
	call sub_d5e2h		;e8e7	cd e2 d5 	. . .
	pop ix		;e8ea	dd e1 	. .
	ld (iy+040h),0ffh		;e8ec	fd 36 40 ff 	. 6 @ .
	ld (iy+03fh),0ffh		;e8f0	fd 36 3f ff 	. 6 ? .
	jp le7a6h		;e8f4	c3 a6 e7 	. . .
le8f7h:
	push ix		;e8f7	dd e5 	. .
	call sub_d6ddh		;e8f9	cd dd d6 	. . .
	pop ix		;e8fc	dd e1 	. .
	jp le7a6h		;e8fe	c3 a6 e7 	. . .
IX_STORE_DISC:
	ld a,(iy+001h)		;e901	fd 7e 01 	. ~ .
	ld (ix+000h),a		;e904	dd 77 00 	. w .
	;Next file stows "DISC."
	ld (ix+001h),044h		;e907	dd 36 01 44 	. 6 . D
	ld (ix+002h),049h		;e90b	dd 36 02 49 	. 6 . I
	ld (ix+003h),053h		;e90f	dd 36 03 53 	. 6 . S
	ld (ix+004h),043h		;e913	dd 36 04 43 	. 6 . C
	ld (ix+005h),02eh		;e917	dd 36 05 2e 	. 6 . .
	ret			;e91b	c9 	.
le91ch:
	xor a			;e91c	af 	.
	ld (iy+01dh),a		;e91d	fd 77 1d 	. w .
	ld (iy+01eh),a		;e920	fd 77 1e 	. w .
	xor a			;e923	af 	.
	ld (iy+01bh),a		;e924	fd 77 1b 	. w .
	ld (iy+01ch),a		;e927	fd 77 1c 	. w .
	ld (iy+03ah),a		;e92a	fd 77 3a 	. w :
	res 2,(iy+041h)		;e92d	fd cb 41 96 	. . A .
	res 4,(iy+041h)		;e931	fd cb 41 a6 	. . A .
	jp lda0ah		;e935	c3 0a da 	. . .
sub_e938h:
	ld (0bf03h),a		;e938	32 03 bf 	2 . .
	jr le953h		;e93b	18 16 	. .
	ld (0bf03h),a		;e93d	32 03 bf 	2 . .
	ld a,(iy+01ch)		;e940	fd 7e 1c 	. ~ .
	or (iy+01bh)		;e943	fd b6 1b 	. . .
	jp z,lda0fh		;e946	ca 0f da 	. . .
	bit 4,(iy+041h)		;e949	fd cb 41 66 	. . A f
	jr nz,le953h		;e94d	20 04 	  .
	res 2,(iy+041h)		;e94f	fd cb 41 96 	. . A .
le953h:
	push hl			;e953	e5 	.
	ld l,(iy+01bh)		;e954	fd 6e 1b 	. n .
	ld h,(iy+01ch)		;e957	fd 66 1c 	. f .
	push hl			;e95a	e5 	.
	pop ix		;e95b	dd e1 	. .
	push de			;e95d	d5 	.
	push bc			;e95e	c5 	.
	bit 0,(iy+03ah)		;e95f	fd cb 3a 46 	. . : F
	call nz,sub_e97ah		;e963	c4 7a e9 	. z .
	jr nz,le975h		;e966	20 0d 	  .
	call sub_ea38h		;e968	cd 38 ea 	. 8 .
	jr nz,le975h		;e96b	20 08 	  .
	pop bc			;e96d	c1 	.
	pop de			;e96e	d1 	.
	pop hl			;e96f	e1 	.
	ld a,00eh		;e970	3e 0e 	> .
	cp 00fh		;e972	fe 0f 	. .
	ret			;e974	c9 	.
le975h:
	pop bc			;e975	c1 	.
	pop de			;e976	d1 	.
	pop hl			;e977	e1 	.
	cp a			;e978	bf 	.
	ret			;e979	c9 	.
sub_e97ah:
	ld a,(ix-00ch)		;e97a	dd 7e f4 	. ~ .
	ld (iy+WS_DRIVE_NUMBER),a		;e97d	fd 77 04 	. w .
	res 2,(iy+041h)		;e980	fd cb 41 96 	. . A .
	call lf07ch		;e984	cd 7c f0 	. | .
	ret nz			;e987	c0 	.
	ld b,(iy+037h)		;e988	fd 46 37 	. F 7
	ld c,(iy+038h)		;e98b	fd 4e 38 	. N 8
	ld (ix-002h),b		;e98e	dd 70 fe 	. p .
	ld (ix-001h),c		;e991	dd 71 ff 	. q .
	ld a,(ix-00bh)		;e994	dd 7e f5 	. ~ .
	ld (iy+005h),a		;e997	fd 77 05 	. w .
	ld a,(ix-00ah)		;e99a	dd 7e f6 	. ~ .
	ld (iy+006h),a		;e99d	fd 77 06 	. w .
	ld a,(ix+013h)		;e9a0	dd 7e 13 	. ~ .
	or (ix+014h)		;e9a3	dd b6 14 	. . .
	call z,sub_ea2bh		;e9a6	cc 2b ea 	. + .
	call sub_ec09h		;e9a9	cd 09 ec 	. . .
	ret nz			;e9ac	c0 	.
	ld a,(ix-002h)		;e9ad	dd 7e fe 	. ~ .
	ld (hl),a			;e9b0	77 	w
	inc hl			;e9b1	23 	#
	ld a,(ix-001h)		;e9b2	dd 7e ff 	. ~ .
	ld (hl),a			;e9b5	77 	w
	inc hl			;e9b6	23 	#
	push bc			;e9b7	c5 	.
	ex de,hl			;e9b8	eb 	.
	push ix		;e9b9	dd e5 	. .
	pop hl			;e9bb	e1 	.
	ld bc,00012h		;e9bc	01 12 00 	. . .
	add hl,bc			;e9bf	09 	.
	ld bc,00005h		;e9c0	01 05 00 	. . .
	ldir		;e9c3	ed b0 	. .
	inc hl			;e9c5	23 	#
	inc hl			;e9c6	23 	#
	inc hl			;e9c7	23 	#
	ld bc,00002h		;e9c8	01 02 00 	. . .
	ldir		;e9cb	ed b0 	. .
	pop bc			;e9cd	c1 	.
	call sub_eff2h		;e9ce	cd f2 ef 	. . .
	ret nz			;e9d1	c0 	.
	ld l,(iy+01dh)		;e9d2	fd 6e 1d 	. n .
	ld h,(iy+01eh)		;e9d5	fd 66 1e 	. f .
	push hl			;e9d8	e5 	.
	push hl			;e9d9	e5 	.
	push iy		;e9da	fd e5 	. .
	pop hl			;e9dc	e1 	.
	ld de,001e9h		;e9dd	11 e9 01 	. . .
	add hl,de			;e9e0	19 	.
	pop de			;e9e1	d1 	.
	ld bc,0000fh		;e9e2	01 0f 00 	. . .
	ldir		;e9e5	ed b0 	. .
	pop hl			;e9e7	e1 	.
	ld de,0000fh		;e9e8	11 0f 00 	. . .
	add hl,de			;e9eb	19 	.
	ld (iy+01bh),l		;e9ec	fd 75 1b 	. u .
	ld (iy+01ch),h		;e9ef	fd 74 1c 	. t .
	push hl			;e9f2	e5 	.
	pop ix		;e9f3	dd e1 	. .
	ld a,(ix-00bh)		;e9f5	dd 7e f5 	. ~ .
	ld (ix+000h),a		;e9f8	dd 77 00 	. w .
	ld a,(ix-00ah)		;e9fb	dd 7e f6 	. ~ .
	ld (ix+001h),a		;e9fe	dd 77 01 	. w .
	ld (ix+002h),000h		;ea01	dd 36 02 00 	. 6 . .
	ld (ix+003h),000h		;ea05	dd 36 03 00 	. 6 . .
	inc hl			;ea09	23 	#
	inc hl			;ea0a	23 	#
	inc hl			;ea0b	23 	#
	inc hl			;ea0c	23 	#
	ld (ix-006h),l		;ea0d	dd 75 fa 	. u .
	ld (ix-005h),h		;ea10	dd 74 fb 	. t .
	ld (iy+03ah),002h		;ea13	fd 36 3a 02 	. 6 : .
	push ix		;ea17	dd e5 	. .
	pop hl			;ea19	e1 	.
	ld b,(ix-002h)		;ea1a	dd 46 fe 	. F .
	ld c,(ix-001h)		;ea1d	dd 4e ff 	. N .
	bit 2,(iy+041h)		;ea20	fd cb 41 56 	. . A V
	jp nz,lda14h		;ea24	c2 14 da 	. . .
	call sub_efd5h		;ea27	cd d5 ef 	. . .
	ret			;ea2a	c9 	.
sub_ea2bh:
	ld a,(ix+018h)		;ea2b	dd 7e 18 	. ~ .
	ld (ix+013h),a		;ea2e	dd 77 13 	. w .
	ld a,(ix+019h)		;ea31	dd 7e 19 	. ~ .
	ld (ix+014h),a		;ea34	dd 77 14 	. w .
	ret			;ea37	c9 	.
sub_ea38h:
	ld l,(ix-00eh)		;ea38	dd 6e f2 	. n .
	ld h,(ix-00dh)		;ea3b	dd 66 f3 	. f .
	ld a,l			;ea3e	7d 	}
	or h			;ea3f	b4 	.
	jr z,lea9ah		;ea40	28 58 	( X
	dec hl			;ea42	2b 	+
	ld (ix-00eh),l		;ea43	dd 75 f2 	. u .
	ld (ix-00dh),h		;ea46	dd 74 f3 	. t .
	ld l,(ix-006h)		;ea49	dd 6e fa 	. n .
	ld h,(ix-005h)		;ea4c	dd 66 fb 	. f .
	ld a,(0bf03h)		;ea4f	3a 03 bf 	: . .
	ld (hl),a			;ea52	77 	w
	inc hl			;ea53	23 	#
	ld (ix-006h),l		;ea54	dd 75 fa 	. u .
	ld (ix-005h),h		;ea57	dd 74 fb 	. t .
	set 2,(iy+03ah)		;ea5a	fd cb 3a d6 	. . : .
	ld l,(ix-004h)		;ea5e	dd 6e fc 	. n .
	ld h,(ix-003h)		;ea61	dd 66 fd 	. f .
	ld a,h			;ea64	7c 	|
	or l			;ea65	b5 	.
	jr z,lea6fh		;ea66	28 07 	( .
	dec hl			;ea68	2b 	+
	ld (ix-004h),l		;ea69	dd 75 fc 	. u .
	ld (ix-003h),h		;ea6c	dd 74 fd 	. t .
lea6fh:
	ld l,(ix-009h)		;ea6f	dd 6e f7 	. n .
	ld h,(ix-008h)		;ea72	dd 66 f8 	. f .
	inc hl			;ea75	23 	#
	ld (ix-009h),l		;ea76	dd 75 f7 	. u .
	ld (ix-008h),h		;ea79	dd 74 f8 	. t .
	ld a,(ix-00fh)		;ea7c	dd 7e f1 	. ~ .
	cp 052h		;ea7f	fe 52 	. R
	call z,sub_e5f0h		;ea81	cc f0 e5 	. . .
	xor a			;ea84	af 	.
	ret			;ea85	c9 	.
lea86h:
	ld (ix+002h),0fbh		;ea86	dd 36 02 fb 	. 6 . .
	ld (ix+003h),001h		;ea8a	dd 36 03 01 	. 6 . .
	push ix		;ea8e	dd e5 	. .
	pop hl			;ea90	e1 	.
	call sub_efd5h		;ea91	cd d5 ef 	. . .
	call sub_f010h		;ea94	cd 10 f0 	. . .
	jp lda0ah		;ea97	c3 0a da 	. . .
lea9ah:
	ld a,(ix-00ch)		;ea9a	dd 7e f4 	. ~ .
	ld (iy+WS_DRIVE_NUMBER),a		;ea9d	fd 77 04 	. w .
	ld a,(ix-00fh)		;eaa0	dd 7e f1 	. ~ .
	cp 052h		;eaa3	fe 52 	. R
	jr nz,leaceh		;eaa5	20 27 	  '
	ld a,(ix-004h)		;eaa7	dd 7e fc 	. ~ .
	or (ix-003h)		;eaaa	dd b6 fd 	. . .
	jr z,leaceh		;eaad	28 1f 	( .
	call sub_eac1h		;eaaf	cd c1 ea 	. . .
	ld b,(ix+002h)		;eab2	dd 46 02 	. F .
	ld c,(ix+003h)		;eab5	dd 4e 03 	. N .
	push ix		;eab8	dd e5 	. .
	pop hl			;eaba	e1 	.
	call sub_e64fh		;eabb	cd 4f e6 	. O .
	jp sub_ea38h		;eabe	c3 38 ea 	. 8 .
sub_eac1h:
	ld b,(ix-002h)		;eac1	dd 46 fe 	. F .
	ld c,(ix-001h)		;eac4	dd 4e ff 	. N .
	push ix		;eac7	dd e5 	. .
	pop hl			;eac9	e1 	.
	call sub_efd5h		;eaca	cd d5 ef 	. . .
	ret			;eacd	c9 	.
leaceh:
	call lf07ch		;eace	cd 7c f0 	. | .
	ld b,(iy+037h)		;ead1	fd 46 37 	. F 7
	ld c,(iy+038h)		;ead4	fd 4e 38 	. N 8
	jr nz,lea86h		;ead7	20 ad 	  .
	ld (ix+002h),b		;ead9	dd 70 02 	. p .
	ld (ix+003h),c		;eadc	dd 71 03 	. q .
	push bc			;eadf	c5 	.
	call sub_eac1h		;eae0	cd c1 ea 	. . .
	pop bc			;eae3	c1 	.
	ret nz			;eae4	c0 	.
	ld a,(ix-002h)		;eae5	dd 7e fe 	. ~ .
	ld (ix+000h),a		;eae8	dd 77 00 	. w .
	ld a,(ix-001h)		;eaeb	dd 7e ff 	. ~ .
	ld (ix+001h),a		;eaee	dd 77 01 	. w .
	ld b,(ix+002h)		;eaf1	dd 46 02 	. F .
	ld (ix-002h),b		;eaf4	dd 70 fe 	. p .
	ld c,(ix+003h)		;eaf7	dd 4e 03 	. N .
	ld (ix-001h),c		;eafa	dd 71 ff 	. q .
	ld hl,001fch		;eafd	21 fc 01 	! . .
	ld (ix-00eh),l		;eb00	dd 75 f2 	. u .
	ld (ix-00dh),h		;eb03	dd 74 f3 	. t .
	ld (ix-009h),000h		;eb06	dd 36 f7 00 	. 6 . .
	ld (ix-008h),000h		;eb0a	dd 36 f8 00 	. 6 . .
	push ix		;eb0e	dd e5 	. .
	pop hl			;eb10	e1 	.
	ld de,00004h		;eb11	11 04 00 	. . .
	add hl,de			;eb14	19 	.
	ld (ix-006h),l		;eb15	dd 75 fa 	. u .
	ld (ix-005h),h		;eb18	dd 74 fb 	. t .
	ld (ix-004h),000h		;eb1b	dd 36 fc 00 	. 6 . .
	ld (ix-003h),000h		;eb1f	dd 36 fd 00 	. 6 . .
	bit 2,(iy+041h)		;eb23	fd cb 41 56 	. . A V
	jp nz,sub_ea38h		;eb27	c2 38 ea 	. 8 .
	push ix		;eb2a	dd e5 	. .
	pop hl			;eb2c	e1 	.
	call sub_efd5h		;eb2d	cd d5 ef 	. . .
	ret nz			;eb30	c0 	.
	jp sub_ea38h		;eb31	c3 38 ea 	. 8 .
sub_eb34h:
	ld l,(iy+01bh)		;eb34	fd 6e 1b 	. n .
	ld h,(iy+01ch)		;eb37	fd 66 1c 	. f .
	ld a,l			;eb3a	7d 	}
	or h			;eb3b	b4 	.
	jp z,lda0fh		;eb3c	ca 0f da 	. . .
	ld a,(iy+03ah)		;eb3f	fd 7e 3a 	. ~ :
	cp 001h		;eb42	fe 01 	. .
	jp z,le91ch		;eb44	ca 1c e9 	. . .
	push hl			;eb47	e5 	.
	pop ix		;eb48	dd e1 	. .
	call le91ch		;eb4a	cd 1c e9 	. . .
	ld a,(ix-00ch)		;eb4d	dd 7e f4 	. ~ .
	ld (iy+WS_DRIVE_NUMBER),a		;eb50	fd 77 04 	. w .
	ld a,(ix-00fh)		;eb53	dd 7e f1 	. ~ .
	ld (ix-00fh),043h		;eb56	dd 36 f1 43 	. 6 . C
	cp 052h		;eb5a	fe 52 	. R
	call z,sub_eb81h		;eb5c	cc 81 eb 	. . .
	ld a,(ix-004h)		;eb5f	dd 7e fc 	. ~ .
	or (ix-003h)		;eb62	dd b6 fd 	. . .
	jr nz,leb73h		;eb65	20 0c 	  .
	ld l,(ix-009h)		;eb67	dd 6e f7 	. n .
	ld h,(ix-008h)		;eb6a	dd 66 f8 	. f .
	ld (ix+002h),l		;eb6d	dd 75 02 	. u .
	ld (ix+003h),h		;eb70	dd 74 03 	. t .
leb73h:
	call sub_eac1h		;eb73	cd c1 ea 	. . .
	push af			;eb76	f5 	.
	call sub_f010h		;eb77	cd 10 f0 	. . .
	pop af			;eb7a	f1 	.
	jp nz,lda14h		;eb7b	c2 14 da 	. . .
	jp lda0ah		;eb7e	c3 0a da 	. . .
sub_eb81h:
	ld (ix-00fh),049h		;eb81	dd 36 f1 49 	. 6 . I
	ret			;eb85	c9 	.
	ld e,(iy+01bh)		;eb86	fd 5e 1b 	. ^ .
	ld d,(iy+01ch)		;eb89	fd 56 1c 	. V .
	push de			;eb8c	d5 	.
	call sub_eb34h		;eb8d	cd 34 eb 	. 4 .
	pop de			;eb90	d1 	.
	ld (iy+01bh),e		;eb91	fd 73 1b 	. s .
	ld (iy+01ch),d		;eb94	fd 72 1c 	. r .
	ret			;eb97	c9 	.
	ld (0bef0h),hl		;eb98	22 f0 be 	" . .
	ld (0bef2h),a		;eb9b	32 f2 be 	2 . .
	ld l,(iy+01bh)		;eb9e	fd 6e 1b 	. n .
	ld h,(iy+01ch)		;eba1	fd 66 1c 	. f .
	ld a,h			;eba4	7c 	|
	or l			;eba5	b5 	.
	jp z,lda0fh		;eba6	ca 0f da 	. . .
	res 2,(iy+041h)		;eba9	fd cb 41 96 	. . A .
	push hl			;ebad	e5 	.
	pop ix		;ebae	dd e1 	. .
	ld hl,(0bef0h)		;ebb0	2a f0 be 	* . .
	ld a,(0bef2h)		;ebb3	3a f2 be 	: . .
	ld (ix+015h),l		;ebb6	dd 75 15 	. u .
	ld (ix+016h),h		;ebb9	dd 74 16 	. t .
	ld (ix+013h),e		;ebbc	dd 73 13 	. s .
	ld (ix+014h),d		;ebbf	dd 72 14 	. r .
	ld (ix+01ah),c		;ebc2	dd 71 1a 	. q .
	ld (ix+01bh),b		;ebc5	dd 70 1b 	. p .
	ld (ix+012h),a		;ebc8	dd 77 12 	. w .
	ld hl,002ffh		;ebcb	21 ff 02 	! . .
	push iy		;ebce	fd e5 	. .
	pop de			;ebd0	d1 	.
	add hl,de			;ebd1	19 	.
	ld (iy+01dh),l		;ebd2	fd 75 1d 	. u .
	ld (iy+01eh),h		;ebd5	fd 74 1e 	. t .
	ld l,(ix+015h)		;ebd8	dd 6e 15 	. n .
	ld h,(ix+016h)		;ebdb	dd 66 16 	. f .
	ld c,(ix+013h)		;ebde	dd 4e 13 	. N .
	ld b,(ix+014h)		;ebe1	dd 46 14 	. F .
	call RAM_LAM		;ebe4	cd 20 00 	.   .
	call sub_e938h		;ebe7	cd 38 e9 	. 8 .
	jr nc,lec03h		;ebea	30 17 	0 .
	set 2,(iy+041h)		;ebec	fd cb 41 d6 	. . A .
	jr lebfah		;ebf0	18 08 	. .
lebf2h:
	call RAM_LAM		;ebf2	cd 20 00 	.   .
	call sub_e938h		;ebf5	cd 38 e9 	. 8 .
	jr nc,lec03h		;ebf8	30 09 	0 .
lebfah:
	inc hl			;ebfa	23 	#
	dec bc			;ebfb	0b 	.
	ld a,b			;ebfc	78 	x
	or c			;ebfd	b1 	.
	jr nz,lebf2h		;ebfe	20 f2 	  .
	jp lda0ah		;ec00	c3 0a da 	. . .
lec03h:
	call sub_eb34h		;ec03	cd 34 eb 	. 4 .
	jp lda14h		;ec06	c3 14 da 	. . .
sub_ec09h:
	call sub_ed01h		;ec09	cd 01 ed 	. . .
	ret nz			;ec0c	c0 	.
	ld a,000h		;ec0d	3e 00 	> .
	ld (iy+014h),a		;ec0f	fd 77 14 	. w .
	call sub_ed38h		;ec12	cd 38 ed 	. 8 .
	jp z,MSG_REBORN		;ec15	ca e2 fb 	. . .
sub_ec18h:
	call sub_ed01h		;ec18	cd 01 ed 	. . .
	ret nz			;ec1b	c0 	.
lec1ch:
	bit 7,(hl)		;ec1c	cb 7e 	. ~
	jr z,lec6fh		;ec1e	28 4f 	( O
	push ix		;ec20	dd e5 	. .
	ld a,(iy+014h)		;ec22	fd 7e 14 	. ~ .
	ld (hl),a			;ec25	77 	w
	push hl			;ec26	e5 	.
	inc hl			;ec27	23 	#
	push bc			;ec28	c5 	.
	ld b,010h		;ec29	06 10 	. .
lec2bh:
	ld a,(ix+000h)		;ec2b	dd 7e 00 	. ~ .
	ld (hl),a			;ec2e	77 	w
	inc ix		;ec2f	dd 23 	. #
	inc hl			;ec31	23 	#
	djnz lec2bh		;ec32	10 f7 	. .
	pop bc			;ec34	c1 	.
	pop ix		;ec35	dd e1 	. .
	ld a,03fh		;ec37	3e 3f 	> ?
	ld (ix+011h),a		;ec39	dd 77 11 	. w .
	xor a			;ec3c	af 	.
	ld (ix+014h),a		;ec3d	dd 77 14 	. w .
	ld (ix+015h),a		;ec40	dd 77 15 	. w .
	ld (ix+016h),a		;ec43	dd 77 16 	. w .
	ld (ix+017h),a		;ec46	dd 77 17 	. w .
	ld (ix+018h),a		;ec49	dd 77 18 	. w .
	ld (ix+019h),a		;ec4c	dd 77 19 	. w .
	ld (ix+01ah),a		;ec4f	dd 77 1a 	. w .
	ld a,(iy+04eh)		;ec52	fd 7e 4e 	. ~ N
	ld (ix+01bh),a		;ec55	dd 77 1b 	. w .
	ld a,(iy+04fh)		;ec58	fd 7e 4f 	. ~ O
	ld (ix+01ch),a		;ec5b	dd 77 1c 	. w .
	ld (ix+01dh),0feh		;ec5e	dd 36 1d fe 	. 6 . .
	ld a,000h		;ec62	3e 00 	> .
	ld (ix+01eh),a		;ec64	dd 77 1e 	. w .
	ld (ix+01fh),a		;ec67	dd 77 1f 	. w .
	pop ix		;ec6a	dd e1 	. .
	inc hl			;ec6c	23 	#
	xor a			;ec6d	af 	.
	ret			;ec6e	c9 	.
lec6fh:
	ld a,(hl)			;ec6f	7e 	~
	cp 002h		;ec70	fe 02 	. .
	jr z,lec7eh		;ec72	28 0a 	( .
	cp 001h		;ec74	fe 01 	. .
	jr z,lec9eh		;ec76	28 26 	( &
	ld de,RAM_LAM		;ec78	11 20 00 	.   .
	add hl,de			;ec7b	19 	.
	jr lec1ch		;ec7c	18 9e 	. .
lec7eh:
	inc hl			;ec7e	23 	#
	ld b,(hl)			;ec7f	46 	F
	inc hl			;ec80	23 	#
	ld c,(hl)			;ec81	4e 	N
	push bc			;ec82	c5 	.
	call sub_ef6ch		;ec83	cd 6c ef 	. l .
	pop bc			;ec86	c1 	.
	ret nz			;ec87	c0 	.
	ld a,(hl)			;ec88	7e 	~
	cp 044h		;ec89	fe 44 	. D
	jr nz,lec98h		;ec8b	20 0b 	  .
	inc hl			;ec8d	23 	#
	ld a,(hl)			;ec8e	7e 	~
	cp 072h		;ec8f	fe 72 	. r
	jr nz,lec98h		;ec91	20 05 	  .
	inc hl			;ec93	23 	#
	inc hl			;ec94	23 	#
	inc hl			;ec95	23 	#
	jr lec1ch		;ec96	18 84 	. .
lec98h:
	call sub_e311h		;ec98	cd 11 e3 	. . .
	jp lda0fh		;ec9b	c3 0f da 	. . .
lec9eh:
	ld d,(iy+037h)		;ec9e	fd 56 37 	. V 7
	ld e,(iy+038h)		;eca1	fd 5e 38 	. ^ 8
	push de			;eca4	d5 	.
	push hl			;eca5	e5 	.
	push bc			;eca6	c5 	.
	call lf07ch		;eca7	cd 7c f0 	. | .
	jr nz,lecf8h		;ecaa	20 4c 	  L
	pop bc			;ecac	c1 	.
	ld a,0ffh		;ecad	3e ff 	> .
	ld (iy+03fh),a		;ecaf	fd 77 3f 	. w ?
	call sub_ef39h		;ecb2	cd 39 ef 	. 9 .
	pop hl			;ecb5	e1 	.
	ld d,(iy+037h)		;ecb6	fd 56 37 	. V 7
	ld e,(iy+038h)		;ecb9	fd 5e 38 	. ^ 8
	ld (hl),002h		;ecbc	36 02 	6 .
	inc hl			;ecbe	23 	#
	ld (hl),d			;ecbf	72 	r
	inc hl			;ecc0	23 	#
	ld (hl),e			;ecc1	73 	s
	push de			;ecc2	d5 	.
	call sub_efbch		;ecc3	cd bc ef 	. . .
	ld (hl),044h		;ecc6	36 44 	6 D
	inc hl			;ecc8	23 	#
	ld (hl),072h		;ecc9	36 72 	6 r
	inc hl			;eccb	23 	#
	ld (hl),b			;eccc	70 	p
	inc hl			;eccd	23 	#
	ld (hl),c			;ecce	71 	q
	inc hl			;eccf	23 	#
	ld b,0e0h		;ecd0	06 e0 	. .
lecd2h:
	ld (hl),0c0h		;ecd2	36 c0 	6 .
	inc hl			;ecd4	23 	#
	djnz lecd2h		;ecd5	10 fb 	. .
	ld b,000h		;ecd7	06 00 	. .
lecd9h:
	ld (hl),0c0h		;ecd9	36 c0 	6 .
	inc hl			;ecdb	23 	#
	djnz lecd9h		;ecdc	10 fb 	. .
	ld a,001h		;ecde	3e 01 	> .
	ld (hl),a			;ece0	77 	w
	pop bc			;ece1	c1 	.
	call sub_efbch		;ece2	cd bc ef 	. . .
	pop de			;ece5	d1 	.
	ld (iy+037h),d		;ece6	fd 72 37 	. r 7
	ld (iy+038h),e		;ece9	fd 73 38 	. s 8
	ld a,0ffh		;ecec	3e ff 	> .
	ld (iy+040h),a		;ecee	fd 77 40 	. w @
	ld de,00004h		;ecf1	11 04 00 	. . .
	add hl,de			;ecf4	19 	.
	jp lec1ch		;ecf5	c3 1c ec 	. . .
lecf8h:
	pop bc			;ecf8	c1 	.
	pop hl			;ecf9	e1 	.
	pop de			;ecfa	d1 	.
	call MSG_DISC_FULL		;ecfb	cd af fb 	. . .
	jp lda0fh		;ecfe	c3 0f da 	. . .
sub_ed01h:
	ld b,(iy+005h)		;ed01	fd 46 05 	. F .
	ld c,(iy+006h)		;ed04	fd 4e 06 	. N .
	ld a,b			;ed07	78 	x
	and a			;ed08	a7 	.
	jr nz,led10h		;ed09	20 05 	  .
	ld a,c			;ed0b	79 	y
	cp 081h		;ed0c	fe 81 	. .
	jr z,led26h		;ed0e	28 16 	( .
led10h:
	call sub_ef6ch		;ed10	cd 6c ef 	. l .
	jp nz,MSG_CORRUPTED_DISC		;ed13	c2 1a fc 	. . .
led16h:
	ld a,(hl)			;ed16	7e 	~
	cp 044h		;ed17	fe 44 	. D
	jr nz,led32h		;ed19	20 17 	  .
	inc hl			;ed1b	23 	#
	ld a,(hl)			;ed1c	7e 	~
	cp 052h		;ed1d	fe 52 	. R
	jr nz,led32h		;ed1f	20 11 	  .
	inc hl			;ed21	23 	#
	inc hl			;ed22	23 	#
	inc hl			;ed23	23 	#
	xor a			;ed24	af 	.
	ret			;ed25	c9 	.
led26h:
	call sub_ef86h		;ed26	cd 86 ef 	. . .
	jp nz,MSG_CORRUPTED_DISC		;ed29	c2 1a fc 	. . .
	ld de,00100h		;ed2c	11 00 01 	. . .
	add hl,de			;ed2f	19 	.
	jr led16h		;ed30	18 e4 	. .
led32h:
	call MSG_BAD_DIR		;ed32	cd 9b fb 	. . .
	jp lda0fh		;ed35	c3 0f da 	. . .
sub_ed38h:
	bit 7,(hl)		;ed38	cb 7e 	. ~
	jr nz,led8dh		;ed3a	20 51 	  Q
	ld a,(hl)			;ed3c	7e 	~
	cp 001h		;ed3d	fe 01 	. .
	jp z,lda0ah		;ed3f	ca 0a da 	. . .
	cp 002h		;ed42	fe 02 	. .
	jr z,led93h		;ed44	28 4d 	( M
	push hl			;ed46	e5 	.
	push ix		;ed47	dd e5 	. .
	push bc			;ed49	c5 	.
	ld b,010h		;ed4a	06 10 	. .
	inc hl			;ed4c	23 	#
	bit 0,(iy+041h)		;ed4d	fd cb 41 46 	. . A F
	jr nz,led68h		;ed51	20 15 	  .
led53h:
	ld a,(ix+000h)		;ed53	dd 7e 00 	. ~ .
	cp 03fh		;ed56	fe 3f 	. ?
	jr z,led5dh		;ed58	28 03 	( .
	cp (hl)			;ed5a	be 	.
	jr nz,led89h		;ed5b	20 2c 	  ,
led5dh:
	inc hl			;ed5d	23 	#
	inc ix		;ed5e	dd 23 	. #
	djnz led53h		;ed60	10 f1 	. .
	pop bc			;ed62	c1 	.
	pop ix		;ed63	dd e1 	. .
	pop hl			;ed65	e1 	.
	xor a			;ed66	af 	.
	ret			;ed67	c9 	.
led68h:
	ld a,(ix+000h)		;ed68	dd 7e 00 	. ~ .
	cp 03fh		;ed6b	fe 3f 	. ?
	jr z,led7eh		;ed6d	28 0f 	( .
	cp 05bh		;ed6f	fe 5b 	. [
	call nc,FUNC_SUBTRACT_32		;ed71	d4 9d d9 	. . .
	ld c,a			;ed74	4f 	O
	ld a,(hl)			;ed75	7e 	~
	cp 05bh		;ed76	fe 5b 	. [
	call nc,FUNC_SUBTRACT_32		;ed78	d4 9d d9 	. . .
	cp c			;ed7b	b9 	.
	jr nz,led89h		;ed7c	20 0b 	  .
led7eh:
	inc hl			;ed7e	23 	#
	inc ix		;ed7f	dd 23 	. #
	djnz led68h		;ed81	10 e5 	. .
	pop bc			;ed83	c1 	.
	pop ix		;ed84	dd e1 	. .
	pop hl			;ed86	e1 	.
	xor a			;ed87	af 	.
	ret			;ed88	c9 	.
led89h:
	pop bc			;ed89	c1 	.
	pop ix		;ed8a	dd e1 	. .
	pop hl			;ed8c	e1 	.
led8dh:
	ld de,RAM_LAM		;ed8d	11 20 00 	.   .
	add hl,de			;ed90	19 	.
	jr sub_ed38h		;ed91	18 a5 	. .
led93h:
	inc hl			;ed93	23 	#
	ld b,(hl)			;ed94	46 	F
	inc hl			;ed95	23 	#
	ld c,(hl)			;ed96	4e 	N
	push bc			;ed97	c5 	.
	call sub_ef6ch		;ed98	cd 6c ef 	. l .
	pop bc			;ed9b	c1 	.
	ret nz			;ed9c	c0 	.
	ld a,(hl)			;ed9d	7e 	~
	cp 044h		;ed9e	fe 44 	. D
	jp nz,MSG_CORRUPTED_DISC		;eda0	c2 1a fc 	. . .
	inc hl			;eda3	23 	#
	ld a,(hl)			;eda4	7e 	~
	cp 072h		;eda5	fe 72 	. r
	jp nz,MSG_CORRUPTED_DISC		;eda7	c2 1a fc 	. . .
	inc hl			;edaa	23 	#
	inc hl			;edab	23 	#
	inc hl			;edac	23 	#
	jr sub_ed38h		;edad	18 89 	. .
	pop hl			;edaf	e1 	.
	call MSG_BAD_FILE_NAME		;edb0	cd 8e fb 	. . .
	jp lda14h		;edb3	c3 14 da 	. . .
sub_edb6h:
	res 7,e		;edb6	cb bb 	. .
	ld a,b			;edb8	78 	x
	and a			;edb9	a7 	.
	jp z,lda0ah		;edba	ca 0a da 	. . .
	cp 001h		;edbd	fe 01 	. .
	jr z,lede7h		;edbf	28 26 	( &
	inc hl			;edc1	23 	#
	ld a,(hl)			;edc2	7e 	~
	dec hl			;edc3	2b 	+
	cp 03ah		;edc4	fe 3a 	. :
	jr nz,lede7h		;edc6	20 1f 	  .
	ld a,(hl)			;edc8	7e 	~
	cp 061h		;edc9	fe 61 	. a
	call nc,FUNC_SUBTRACT_32		;edcb	d4 9d d9 	. . .
	sub 041h		;edce	d6 41 	. A
	jp c,MSG_BAD_DRIVE		;edd0	da c2 fb 	. . .
	cp 049h		;edd3	fe 49 	. I
	jp nc,MSG_BAD_DRIVE		;edd5	d2 c2 fb 	. . .
	ld (iy+WS_CURRENT_DRIVE_LETTER),a		;edd8	fd 77 03 	. w .
	call lda47h		;eddb	cd 47 da 	. G .
	dec b			;edde	05 	.
	dec b			;eddf	05 	.
	inc hl			;ede0	23 	#
	inc hl			;ede1	23 	#
	ld a,b			;ede2	78 	x
	and a			;ede3	a7 	.
	jp z,lda0ah		;ede4	ca 0a da 	. . .
lede7h:
	call sub_ee7ch		;ede7	cd 7c ee 	. | .
	ret nz			;edea	c0 	.
	ld a,b			;edeb	78 	x
	and a			;edec	a7 	.
	jp z,lda0ah		;eded	ca 0a da 	. . .
	cp 002h		;edf0	fe 02 	. .
	jr nz,ledfch		;edf2	20 08 	  .
	call sub_ef01h		;edf4	cd 01 ef 	. . .
	jr nz,ledfch		;edf7	20 03 	  .
	jp lda0ah		;edf9	c3 0a da 	. . .
ledfch:
	ld c,010h		;edfc	0e 10 	. .
	push ix		;edfe	dd e5 	. .
	ld a,(hl)			;ee00	7e 	~
	inc hl			;ee01	23 	#
	cp 02eh		;ee02	fe 2e 	. .
	jr z,lee1fh		;ee04	28 19 	( .
	dec hl			;ee06	2b 	+
lee07h:
	ld a,(hl)			;ee07	7e 	~
	inc hl			;ee08	23 	#
	cp 021h		;ee09	fe 21 	. !
	jr z,lee29h		;ee0b	28 1c 	( .
	cp 03ah		;ee0d	fe 3a 	. :
	jr z,lee29h		;ee0f	28 18 	( .
	cp 02eh		;ee11	fe 2e 	. .
	jr z,lee50h		;ee13	28 3b 	( ;
	cp 02ah		;ee15	fe 2a 	. *
	jr z,lee61h		;ee17	28 48 	( H
	cp 03fh		;ee19	fe 3f 	. ?
	jr nz,lee1fh		;ee1b	20 02 	  .
	set 7,e		;ee1d	cb fb 	. .
lee1fh:
	ld (ix+000h),a		;ee1f	dd 77 00 	. w .
	inc ix		;ee22	dd 23 	. #
	dec c			;ee24	0d 	.
	ld a,c			;ee25	79 	y
	and a			;ee26	a7 	.
	jr z,lee3dh		;ee27	28 14 	( .
lee29h:
	djnz lee07h		;ee29	10 dc 	. .
	ld b,c			;ee2b	41 	A
	ld a,b			;ee2c	78 	x
	and a			;ee2d	a7 	.
	jr z,lee39h		;ee2e	28 09 	( .
	ld a,020h		;ee30	3e 20 	>
lee32h:
	ld (ix+000h),a		;ee32	dd 77 00 	. w .
	inc ix		;ee35	dd 23 	. #
	djnz lee32h		;ee37	10 f9 	. .
lee39h:
	xor a			;ee39	af 	.
	pop ix		;ee3a	dd e1 	. .
	ret			;ee3c	c9 	.
lee3dh:
	ld a,b			;ee3d	78 	x
	dec a			;ee3e	3d 	=
	and a			;ee3f	a7 	.
	jr z,lee39h		;ee40	28 f7 	( .
	pop ix		;ee42	dd e1 	. .
	jp MSG_BAD_FILE_NAME		;ee44	c3 8e fb 	. . .
lee47h:
	dec b			;ee47	05 	.
	pop ix		;ee48	dd e1 	. .
	ld a,b			;ee4a	78 	x
	and a			;ee4b	a7 	.
	jp nz,MSG_BAD_FILE_NAME		;ee4c	c2 8e fb 	. . .
	ret			;ee4f	c9 	.
lee50h:
	ld a,c			;ee50	79 	y
	cp 004h		;ee51	fe 04 	. .
	jr z,lee29h		;ee53	28 d4 	( .
	and a			;ee55	a7 	.
	jr z,lee47h		;ee56	28 ef 	( .
	ld (ix+000h),020h		;ee58	dd 36 00 20 	. 6 .
	inc ix		;ee5c	dd 23 	. #
	dec c			;ee5e	0d 	.
	jr lee50h		;ee5f	18 ef 	. .
lee61h:
	ld a,c			;ee61	79 	y
	and a			;ee62	a7 	.
	jr z,lee47h		;ee63	28 e2 	( .
	set 7,e		;ee65	cb fb 	. .
	ld (ix+000h),03fh		;ee67	dd 36 00 3f 	. 6 . ?
	inc ix		;ee6b	dd 23 	. #
	dec c			;ee6d	0d 	.
	ld a,c			;ee6e	79 	y
	cp 005h		;ee6f	fe 05 	. .
	jr z,lee75h		;ee71	28 02 	( .
	jr lee61h		;ee73	18 ec 	. .
lee75h:
	ld a,(hl)			;ee75	7e 	~
	cp 02eh		;ee76	fe 2e 	. .
	jr z,lee29h		;ee78	28 af 	( .
	jr lee61h		;ee7a	18 e5 	. .
sub_ee7ch:
	ld a,b			;ee7c	78 	x
	and a			;ee7d	a7 	.
	ret z			;ee7e	c8 	.
	ld a,(hl)			;ee7f	7e 	~
	cp 02fh		;ee80	fe 2f 	. /
	jp z,lef1dh		;ee82	ca 1d ef 	. . .
	ld c,000h		;ee85	0e 00 	. .
	push bc			;ee87	c5 	.
	push hl			;ee88	e5 	.
lee89h:
	ld a,(hl)			;ee89	7e 	~
	cp 02fh		;ee8a	fe 2f 	. /
	jr z,lee96h		;ee8c	28 08 	( .
	inc hl			;ee8e	23 	#
	inc c			;ee8f	0c 	.
	djnz lee89h		;ee90	10 f7 	. .
	pop hl			;ee92	e1 	.
	pop bc			;ee93	c1 	.
	xor a			;ee94	af 	.
	ret			;ee95	c9 	.
lee96h:
	pop de			;ee96	d1 	.
	pop af			;ee97	f1 	.
	push hl			;ee98	e5 	.
	push bc			;ee99	c5 	.
	ex de,hl			;ee9a	eb 	.
	ld b,c			;ee9b	41 	A
	ld a,b			;ee9c	78 	x
	cp 002h		;ee9d	fe 02 	. .
	jr z,leef3h		;ee9f	28 52 	( R
leea1h:
	call ledfch		;eea1	cd fc ed 	. . .
	jr nz,leefch		;eea4	20 56 	  V
	call sub_ed01h		;eea6	cd 01 ed 	. . .
	jr nz,leefch		;eea9	20 51 	  Q
	call sub_ed38h		;eeab	cd 38 ed 	. 8 .
	push af			;eeae	f5 	.
	call nz,lfbf7h		;eeaf	c4 f7 fb 	. . .
	pop af			;eeb2	f1 	.
	jr nz,leecah		;eeb3	20 15 	  .
	call sub_db19h		;eeb5	cd 19 db 	. . .
	xor 004h		;eeb8	ee 04 	. .
	and 005h		;eeba	e6 05 	. .
	jr z,leec5h		;eebc	28 07 	( .
	call sub_eecfh		;eebe	cd cf ee 	. . .
	pop bc			;eec1	c1 	.
	pop hl			;eec2	e1 	.
	jr lef27h		;eec3	18 62 	. b
leec5h:
	pop bc			;eec5	c1 	.
	pop hl			;eec6	e1 	.
	jp MSG_ACCESS_DENIED		;eec7	c3 a7 fb 	. . .
leecah:
	pop bc			;eeca	c1 	.
	pop hl			;eecb	e1 	.
	jp lda0fh		;eecc	c3 0f da 	. . .
sub_eecfh:
	ld de,00012h		;eecf	11 12 00 	. . .
	add hl,de			;eed2	19 	.
	ld a,(hl)			;eed3	7e 	~
	ld (iy+005h),a		;eed4	fd 77 05 	. w .
	inc hl			;eed7	23 	#
	ld a,(hl)			;eed8	7e 	~
	ld (iy+006h),a		;eed9	fd 77 06 	. w .
	ld de,0000ah		;eedc	11 0a 00 	. . .
	add hl,de			;eedf	19 	.
	ld a,(hl)			;eee0	7e 	~
	cp 0feh		;eee1	fe fe 	. .
	call c,sub_eee7h		;eee3	dc e7 ee 	. . .
	ret			;eee6	c9 	.
sub_eee7h:
	ld (0be08h),a		;eee7	32 08 be 	2 . .
	ld (iy+WS_DRIVE_NUMBER),a		;eeea	fd 77 04 	. w .
sub_eeedh:
	ld a,008h		;eeed	3e 08 	> .
	ld (iy+WS_CURRENT_DRIVE_LETTER),a		;eeef	fd 77 03 	. w .
	ret			;eef2	c9 	.
leef3h:
	call sub_ef01h		;eef3	cd 01 ef 	. . .
	jr nz,leea1h		;eef6	20 a9 	  .
	pop bc			;eef8	c1 	.
	pop hl			;eef9	e1 	.
	jr lef27h		;eefa	18 2b 	. +
leefch:
	pop bc			;eefc	c1 	.
	pop hl			;eefd	e1 	.
	jp MSG_BAD_DIR		;eefe	c3 9b fb 	. . .
sub_ef01h:
	ld a,(hl)			;ef01	7e 	~
	cp 02eh		;ef02	fe 2e 	. .
	ret nz			;ef04	c0 	.
	inc hl			;ef05	23 	#
	ld a,(hl)			;ef06	7e 	~
	dec hl			;ef07	2b 	+
	cp 02eh		;ef08	fe 2e 	. .
	ret nz			;ef0a	c0 	.
	call sub_ed01h		;ef0b	cd 01 ed 	. . .
	jr nz,leefch		;ef0e	20 ec 	  .
	dec hl			;ef10	2b 	+
	dec hl			;ef11	2b 	+
	ld a,(hl)			;ef12	7e 	~
	ld (iy+005h),a		;ef13	fd 77 05 	. w .
	inc hl			;ef16	23 	#
	ld a,(hl)			;ef17	7e 	~
	ld (iy+006h),a		;ef18	fd 77 06 	. w .
	xor a			;ef1b	af 	.
	ret			;ef1c	c9 	.
lef1dh:
	ld a,000h		;ef1d	3e 00 	> .
	ld (iy+005h),a		;ef1f	fd 77 05 	. w .
	ld a,081h		;ef22	3e 81 	> .
	ld (iy+006h),a		;ef24	fd 77 06 	. w .
lef27h:
	inc hl			;ef27	23 	#
	dec b			;ef28	05 	.
	ld a,b			;ef29	78 	x
	and a			;ef2a	a7 	.
	jp z,lda0ah		;ef2b	ca 0a da 	. . .
	jp sub_ee7ch		;ef2e	c3 7c ee 	. | .
sub_ef31h:
	ld de,006ebh		;ef31	11 eb 06 	. . .
	push iy		;ef34	fd e5 	. .
	pop hl			;ef36	e1 	.
	add hl,de			;ef37	19 	.
	ret			;ef38	c9 	.
sub_ef39h:
	call sub_ef31h		;ef39	cd 31 ef 	. 1 .
	ld (iy+03dh),b		;ef3c	fd 70 3d 	. p =
	ld (iy+03eh),c		;ef3f	fd 71 3e 	. q >
	ld a,(iy+WS_DRIVE_NUMBER)		;ef42	fd 7e 04 	. ~ .
	ld (iy+03fh),a		;ef45	fd 77 3f 	. w ?
sub_ef48h:
	ld e,(iy+WS_DRIVE_NUMBER)		;ef48	fd 5e 04 	. ^ .
	call sub_f131h		;ef4b	cd 31 f1 	. 1 .
lef4eh:
	push hl			;ef4e	e5 	.
	ld hl,0be80h		;ef4f	21 80 be 	! . .
	ld (hl),084h		;ef52	36 84 	6 .
	ld (0be83h),hl		;ef54	22 83 be 	" . .
	pop hl			;ef57	e1 	.
	ld d,b			;ef58	50 	P
	call sub_d48ch		;ef59	cd 8c d4 	. . .
lef5ch:
	jr nc,lef66h		;ef5c	30 08 	0 .
	cp a			;ef5e	bf 	.
	scf			;ef5f	37 	7
	ret			;ef60	c9 	.
sub_ef61h:
	call sub_f131h		;ef61	cd 31 f1 	. 1 .
	jr lef4eh		;ef64	18 e8 	. .
lef66h:
	ld d,a			;ef66	57 	W
	inc d			;ef67	14 	.
	cp d			;ef68	ba 	.
	scf			;ef69	37 	7
	ccf			;ef6a	3f 	?
	ret			;ef6b	c9 	.
sub_ef6ch:
	ld a,b			;ef6c	78 	x
	cp (iy+03dh)		;ef6d	fd be 3d 	. . =
	jr nz,sub_ef39h		;ef70	20 c7 	  .
	ld a,c			;ef72	79 	y
	cp (iy+03eh)		;ef73	fd be 3e 	. . >
	jr nz,sub_ef39h		;ef76	20 c1 	  .
	ld a,(iy+WS_DRIVE_NUMBER)		;ef78	fd 7e 04 	. ~ .
	cp (iy+03fh)		;ef7b	fd be 3f 	. . ?
	jr nz,sub_ef39h		;ef7e	20 b9 	  .
	call sub_ef31h		;ef80	cd 31 ef 	. 1 .
	xor a			;ef83	af 	.
	scf			;ef84	37 	7
	ret			;ef85	c9 	.
sub_ef86h:
	ld bc,00081h		;ef86	01 81 00 	. . .
	jr sub_ef39h		;ef89	18 ae 	. .
sub_ef8bh:
	ld bc,00081h		;ef8b	01 81 00 	. . .
	call sub_ef31h		;ef8e	cd 31 ef 	. 1 .
	ld de,00200h		;ef91	11 00 02 	. . .
	add hl,de			;ef94	19 	.
	ld a,(iy+WS_DRIVE_NUMBER)		;ef95	fd 7e 04 	. ~ .
	cp (iy+040h)		;ef98	fd be 40 	. . @
	call nz,sub_efach		;ef9b	c4 ac ef 	. . .
	ret nz			;ef9e	c0 	.
	ld a,(hl)			;ef9f	7e 	~
	ld (iy+00fh),a		;efa0	fd 77 0f 	. w .
	inc hl			;efa3	23 	#
	ld a,(hl)			;efa4	7e 	~
	ld (iy+010h),a		;efa5	fd 77 10 	. w .
	dec hl			;efa8	2b 	+
	xor a			;efa9	af 	.
	scf			;efaa	37 	7
	ret			;efab	c9 	.
sub_efach:
	ld a,0ffh		;efac	3e ff 	> .
	ld (iy+040h),a		;efae	fd 77 40 	. w @
	call sub_ef48h		;efb1	cd 48 ef 	. H .
	ret nz			;efb4	c0 	.
	ld e,(iy+WS_DRIVE_NUMBER)		;efb5	fd 5e 04 	. ^ .
	ld (iy+040h),e		;efb8	fd 73 40 	. s @
	ret			;efbb	c9 	.
sub_efbch:
	call sub_ef31h		;efbc	cd 31 ef 	. 1 .
	ld (iy+03dh),b		;efbf	fd 70 3d 	. p =
	ld (iy+03eh),c		;efc2	fd 71 3e 	. q >
	ld e,(iy+WS_DRIVE_NUMBER)		;efc5	fd 5e 04 	. ^ .
	ld (iy+03fh),e		;efc8	fd 73 3f 	. s ?
	call sub_efd5h		;efcb	cd d5 ef 	. . .
	ret z			;efce	c8 	.
	ld a,0ffh		;efcf	3e ff 	> .
	ld (iy+03fh),a		;efd1	fd 77 3f 	. w ?
	ret			;efd4	c9 	.
sub_efd5h:
	ld e,(iy+WS_DRIVE_NUMBER)		;efd5	fd 5e 04 	. ^ .
	call sub_f131h		;efd8	cd 31 f1 	. 1 .
lefdbh:
	push hl			;efdb	e5 	.
	ld hl,0be80h		;efdc	21 80 be 	! . .
	ld (hl),085h		;efdf	36 85 	6 .
	ld (0be83h),hl		;efe1	22 83 be 	" . .
	pop hl			;efe4	e1 	.
	ld d,b			;efe5	50 	P
	call sub_d48ch		;efe6	cd 8c d4 	. . .
	jp lef5ch		;efe9	c3 5c ef 	. \ .
sub_efech:
	ld bc,00081h		;efec	01 81 00 	. . .
	jp sub_efbch		;efef	c3 bc ef 	. . .
sub_eff2h:
	ld a,b			;eff2	78 	x
	and a			;eff3	a7 	.
	jp nz,sub_efbch		;eff4	c2 bc ef 	. . .
	ld a,c			;eff7	79 	y
	cp 081h		;eff8	fe 81 	. .
	jp nz,sub_efbch		;effa	c2 bc ef 	. . .
	call sub_ef31h		;effd	cd 31 ef 	. 1 .
	ld bc,00200h		;f000	01 00 02 	. . .
	add hl,bc			;f003	09 	.
	push hl			;f004	e5 	.
	call sub_ef31h		;f005	cd 31 ef 	. 1 .
	pop de			;f008	d1 	.
	ld bc,00200h		;f009	01 00 02 	. . .
	ldir		;f00c	ed b0 	. .
	jr sub_efech		;f00e	18 dc 	. .
sub_f010h:
	ld a,(iy+040h)		;f010	fd 7e 40 	. ~ @
	cp 0ffh		;f013	fe ff 	. .
	ret z			;f015	c8 	.
	call sub_ef31h		;f016	cd 31 ef 	. 1 .
	ld bc,00200h		;f019	01 00 02 	. . .
	add hl,bc			;f01c	09 	.
	ld bc,00081h		;f01d	01 81 00 	. . .
	ld e,a			;f020	5f 	_
	ld (iy+012h),000h		;f021	fd 36 12 00 	. 6 . .
	jp lefdbh		;f025	c3 db ef 	. . .
lf028h:
	call sub_ef8bh		;f028	cd 8b ef 	. . .
	jp nc,lda0ah		;f02b	d2 0a da 	. . .
	ld de,00012h		;f02e	11 12 00 	. . .
	add hl,de			;f031	19 	.
	ld de,00081h		;f032	11 81 00 	. . .
lf035h:
	ld c,(hl)			;f035	4e 	N
	ld b,008h		;f036	06 08 	. .
lf038h:
	bit 0,c		;f038	cb 41 	. A
	jr z,lf064h		;f03a	28 28 	( (
	inc e			;f03c	1c 	.
	ld a,095h		;f03d	3e 95 	> .
	cp e			;f03f	bb 	.
	jr z,lf04dh		;f040	28 0b 	( .
	ld a,08bh		;f042	3e 8b 	> .
	cp e			;f044	bb 	.
	jr nz,lf056h		;f045	20 0f 	  .
	ld a,(iy+010h)		;f047	fd 7e 10 	. ~ .
	and a			;f04a	a7 	.
	jr nz,lf056h		;f04b	20 09 	  .
lf04dh:
	ld e,081h		;f04d	1e 81 	. .
	inc d			;f04f	14 	.
	ld a,(iy+00fh)		;f050	fd 7e 0f 	. ~ .
	cp d			;f053	ba 	.
	jr z,lf05dh		;f054	28 07 	( .
lf056h:
	sra c		;f056	cb 29 	. )
	djnz lf038h		;f058	10 de 	. .
	inc hl			;f05a	23 	#
	jr lf035h		;f05b	18 d8 	. .
lf05dh:
	call MSG_DISC_FULL		;f05d	cd af fb 	. . .
	xor a			;f060	af 	.
	cp 001h		;f061	fe 01 	. .
	ret			;f063	c9 	.
lf064h:
	ld c,b			;f064	48 	H
	ld b,000h		;f065	06 00 	. .
	push hl			;f067	e5 	.
	ld hl,lf073h		;f068	21 73 f0 	! s .
	add hl,bc			;f06b	09 	.
	ld a,(hl)			;f06c	7e 	~
	pop hl			;f06d	e1 	.
	ld bc,00081h		;f06e	01 81 00 	. . .
	cp a			;f071	bf 	.
	ret			;f072	c9 	.
lf073h:
	;Z80dasm decided this was code, but it wasn't.
	;This was the code.
	; nop			;f073	00 	.
	; add a,b			;f074	80 	.
	; ld b,b			;f075	40 	@
	; jr nz,lf088h		;f076	20 10 	  .
	; ex af,af'			;f078	08 	. '
	; inc b			;f079	04 	.
	; ld (bc),a			;f07a	02 	.
	; ld bc,028cdh		;f07b	01 cd 28 	. . (
	;This data is actually what is used:
	db 00h,80h,40h,20h,10h,08h,04h,02h,01h
	; 										 DAT_ram_f07b                                    XREF[1]:     FUN_ram_f028:f06c(R)
	; ram:f07b 01              undefined1 01h

lf07ch:
	call lf028h
	ret nz			;f07f	c0 	.
	or (hl)			;f080	b6 	.
	ld (hl),a			;f081	77 	w
	ld (iy+037h),d		;f082	fd 72 37 	. r 7
	ld (iy+038h),e		;f085	fd 73 38 	. s 8
lf088h:
	bit 2,(iy+041h)		;f088	fd cb 41 56 	. . A V
	jp nz,lda14h		;f08c	c2 14 da 	. . .
	call sub_f010h		;f08f	cd 10 f0 	. . .
	ret			;f092	c9 	.
sub_f093h:
	push bc			;f093	c5 	.
	call sub_ef31h		;f094	cd 31 ef 	. 1 .
	ld de,00212h		;f097	11 12 02 	. . .
	add hl,de			;f09a	19 	.
	pop de			;f09b	d1 	.
lf09ch:
	ld b,008h		;f09c	06 08 	. .
lf09eh:
	dec e			;f09e	1d 	.
	ld a,e			;f09f	7b 	{
	cp 080h		;f0a0	fe 80 	. .
	jr nz,lf0b4h		;f0a2	20 10 	  .
	dec d			;f0a4	15 	.
	ld a,d			;f0a5	7a 	z
	cp 0ffh		;f0a6	fe ff 	. .
	jr z,lf0b9h		;f0a8	28 0f 	( .
	ld e,08ah		;f0aa	1e 8a 	. .
	ld a,(iy+010h)		;f0ac	fd 7e 10 	. ~ .
	and a			;f0af	a7 	.
	jr z,lf0b4h		;f0b0	28 02 	( .
	ld e,094h		;f0b2	1e 94 	. .
lf0b4h:
	djnz lf09eh		;f0b4	10 e8 	. .
	inc hl			;f0b6	23 	#
	jr lf09ch		;f0b7	18 e3 	. .
lf0b9h:
	ld c,b			;f0b9	48 	H
	ld b,000h		;f0ba	06 00 	. .
	push hl			;f0bc	e5 	.
	ld hl,lf0c7h		;f0bd	21 c7 f0 	! . .
	add hl,bc			;f0c0	09 	.
	ld a,(hl)			;f0c1	7e 	~
	pop hl			;f0c2	e1 	.
	and (hl)			;f0c3	a6 	.
	ld (hl),a			;f0c4	77 	w
	xor a			;f0c5	af 	.
	ret			;f0c6	c9 	.
lf0c7h:
	nop			;f0c7	00 	.
	ld a,a			;f0c8	7f 	
	cp a			;f0c9	bf 	.
	rst 18h			;f0ca	df 	.
	dw  l0f7efh ;z80dasm didn't understand this bit. So here is how this works
	;

	; 007   &0018   FAR CALL (RST 3)
	;     Action: Calls a routine anywhere in RAM or ROM
	;     Entry:  No entry conditions - all  the  registers apart from IY
	;             are passed to the destination routine unaltered
	;     Exit:   IY is preserved, and the other  registers are as set by
	;             the destination routine or are returned unchanged
	;     Notes:  The RST 3 instruction is followed by a two byte in-line
	;             address.  At this address,  there  is  a three byte far
	;             address, which is defined as follows:
	;               bytes 0 and 1 give the address of the routine to be
	;                 called
	;               byte 2 is the ROM select byte which has values as
	;                 follows:
	;                 &00 to &FB-- select the given upper ROM, enable the
	;                 upper ROM and disable the lower ROM
	;                 &FC - no change to the ROM selection, enable the
	;                 upper and lower ROMs
	;                 &FD - no change to the ROM selection, enable the
	;                 upper ROM and disable the lower ROM
	;                 &FE - no change to the ROM selection, disable the
	;                 upper ROM and enable the lower ROM
	;                 &FF - no change to the ROM selection, disable the
	;                 upper and lower ROMs
	;             When it is retumed  from,  the  ROM selection and state
	;             are restored to their settings before the RST 3 command

	;Notes here: TODO
	;These next bytes aren't instructions, but neither are they apparently used AFAIK
	db 0fbh 			;f0cd	fb 	.
	defb 0fdh,0feh

	;Here is what Ghidra thought of this section.
; ram:f0c7 00              ??         00h = nop
; ram:f0c8 7f              ??         7Fh = ld a,a   
; ram:f0c9 bf              ??         BFh = cp a
; ram:f0ca df              ??         DFh	= rst 18h (aka rst 3)
; ram:f0cb ef              ??         EFh = rst l
; ram:f0cc f7              ??         F7h  = rst h
; ram:f0cd fb              ??         FBh  = ??
; ram:f0ce fd              ??         FDh  = ??

sub_0f0d0h:
	call sub_ef8bh
	jp nc,lda0ah		;f0d3	d2 0a da 	. . .
	ld ix,RESET_ENTRY_RST_0		;f0d6	dd 21 00 00 	. ! . .
	ld a,(hl)			;f0da	7e 	~
	ld (iy+00fh),a		;f0db	fd 77 0f 	. w .
	ld (iy+012h),a		;f0de	fd 77 12 	. w .
	inc hl			;f0e1	23 	#
	ld a,(hl)			;f0e2	7e 	~
	ld (iy+010h),a		;f0e3	fd 77 10 	. w .
	ld de,00011h		;f0e6	11 11 00 	. . .
	add hl,de			;f0e9	19 	.
	ld a,(iy+010h)		;f0ea	fd 7e 10 	. ~ .
	and a			;f0ed	a7 	.
	jr z,lf0f7h		;f0ee	28 07 	( .
	ld a,(iy+00fh)		;f0f0	fd 7e 0f 	. ~ .
	add a,a			;f0f3	87 	.
	ld (iy+012h),a		;f0f4	fd 77 12 	. w .
lf0f7h:
	ld de,00081h		;f0f7	11 81 00 	. . .
lf0fah:
	ld c,(hl)			;f0fa	4e 	N
	ld b,008h		;f0fb	06 08 	. .
lf0fdh:
	bit 0,c		;f0fd	cb 41 	. A
	jr nz,lf103h		;f0ff	20 02 	  .
	inc ix		;f101	dd 23 	. #
lf103h:
	inc e			;f103	1c 	.
	ld a,e			;f104	7b 	{
	cp 08bh		;f105	fe 8b 	. .
	jr nz,lf113h		;f107	20 0a 	  .
	inc d			;f109	14 	.
	ld a,d			;f10a	7a 	z
	cp (iy+012h)		;f10b	fd be 12 	. . .
	jp z,lda14h		;f10e	ca 14 da 	. . .
	ld e,081h		;f111	1e 81 	. .
lf113h:
	sra c		;f113	cb 29 	. )
	djnz lf0fdh		;f115	10 e6 	. .
	inc hl			;f117	23 	#
	jr lf0fah		;f118	18 e0 	. .
sub_f11ah:
	;  if ((byte)(&DAT_ram_be00)[param_1 & 0xff] < 200) { return; } return;
	push hl			;f11a	e5 	.
	push de			;f11b	d5 	.
	ld d,000h		;f11c	16 00 	. .
	ld hl,0be00h		;f11e	21 00 be 	! . .
	add hl,de			;f121	19 	.
	ld a,(hl)			;f122	7e 	~
	pop de			;f123	d1 	.
	ld e,a			;f124	5f 	_
	pop hl			;f125	e1 	.
	cp 0c8h		;f126	fe c8 	. .
	jr nc,lf12dh		;f128	30 03 	0 .
	cp a			;f12a	bf 	.
	scf			;f12b	37 	7
	ret			;f12c	c9 	.
lf12dh:
	cp 002h		;f12d	fe 02 	. .
	scf			;f12f	37 	7
	ret			;f130	c9 	.
sub_f131h:
	ld (iy+012h),000h		;f131	fd 36 12 00 	. 6 . .
	ld a,c			;f135	79 	y
	and 01fh		;f136	e6 1f 	. .
	cp 00bh		;f138	fe 0b 	. .
	jp c,lda14h		;f13a	da 14 da 	. . .
	ld (iy+012h),001h		;f13d	fd 36 12 01 	. 6 . .
	cp a			;f141	bf 	.
	ret			;f142	c9 	.
sub_f143h:
	cp 0c8h		;f143	fe c8 	. .
	jr nc,lf16eh		;f145	30 27 	0 '
	ld e,0ffh		;f147	1e ff 	. .
	cp 009h		;f149	fe 09 	. .
	jr c,lf152h		;f14b	38 05 	8 .
	sub 009h		;f14d	d6 09 	. .
	ld e,a			;f14f	5f 	_
	ld a,001h		;f150	3e 01 	> .
lf152h:
	push af			;f152	f5 	.
	cp 008h		;f153	fe 08 	. .
	ld a,e			;f155	7b 	{
	call nz,sub_f174h		;f156	c4 74 f1 	. t .
	pop af			;f159	f1 	.
	ld e,a			;f15a	5f 	_
	cp 008h		;f15b	fe 08 	. .
	jr z,lf167h		;f15d	28 08 	( .
	ld a,(iy+012h)		;f15f	fd 7e 12 	. ~ .
	and a			;f162	a7 	.
	call nz,sub_f169h		;f163	c4 69 f1 	. i .
	ld a,e			;f166	7b 	{
lf167h:
	cp a			;f167	bf 	.
	ret			;f168	c9 	.
sub_f169h:
	ld a,e			;f169	7b 	{
	xor 004h		;f16a	ee 04 	. .
	ld e,a			;f16c	5f 	_
	ret			;f16d	c9 	.
lf16eh:
	ld a,0feh		;f16e	3e fe 	> .
	ld e,a			;f170	5f 	_
	cp 0ffh		;f171	fe ff 	. .
	ret			;f173	c9 	.
sub_f174h:
	push bc			;f174	c5 	.
	ld c,(iy+058h)		;f175	fd 4e 58 	. N X
	ld b,(iy+059h)		;f178	fd 46 59 	. F Y
	out (c),a		;f17b	ed 79 	. y
	cp (iy+03ch)		;f17d	fd be 3c 	. . <
	call nz,sub_f185h		;f180	c4 85 f1 	. . .
	pop bc			;f183	c1 	.
	ret			;f184	c9 	.
sub_f185h:
	cp 0fah		;f185	fe fa 	. .
	ret nc			;f187	d0 	.
	ld (iy+03ch),a		;f188	fd 77 3c 	. w <
	call sub_ca63h		;f18b	cd 63 ca 	. c .
	push hl			;f18e	e5 	.
	ld hl,00081h		;f18f	21 81 00 	! . .
	push iy		;f192	fd e5 	. .
	pop bc			;f194	c1 	.
	add hl,bc			;f195	09 	.
	ld (hl),000h		;f196	36 00 	6 .
	push de			;f198	d5 	.
	call sub_d9a0h		;f199	cd a0 d9 	. . .
	push bc			;f19c	c5 	.
	ld e,001h		;f19d	1e 01 	. .
	call sub_c9deh		;f19f	cd de c9 	. . .
	pop bc			;f1a2	c1 	.
	ld a,b			;f1a3	78 	x
	and a			;f1a4	a7 	.
	call nz,sub_cb51h		;f1a5	c4 51 cb 	. Q .
	pop de			;f1a8	d1 	.
	pop hl			;f1a9	e1 	.
	ret			;f1aa	c9 	.
lf1abh:
	ld (de),a			;f1ab	12 	.
	ret			;f1ac	c9 	.
sub_f1adh:
	ld b,000h		;f1ad	06 00 	. .
	call ld8f7h		;f1af	cd f7 d8 	. . .
	ex de,hl			;f1b2	eb 	.
lf1b3h:
	ld a,05fh		;f1b3	3e 5f 	> _
	call TXT_OUTPUT		;f1b5	cd 5a bb 	. Z .
	call KM_WAIT_CHAR		;f1b8	cd 06 bb 	. . .
	call DELETE_CHAR		;f1bb	cd 8b d9 	. . .
	cp 0fch		;f1be	fe fc 	. .
	jr z,lf1abh		;f1c0	28 e9 	( .
	cp 07fh		;f1c2	fe 7f 	. 
	jr z,lf1deh		;f1c4	28 18 	( .
	jr nc,lf1f5h		;f1c6	30 2d 	0 -
	cp 020h		;f1c8	fe 20 	.
	jr c,lf1ebh		;f1ca	38 1f 	8 .
	ld (de),a			;f1cc	12 	.
	inc de			;f1cd	13 	.
	call TXT_OUTPUT		;f1ce	cd 5a bb 	. Z .
	inc b			;f1d1	04 	.
	ld a,b			;f1d2	78 	x
	cp 04fh		;f1d3	fe 4f 	. O
	jr nz,lf1b3h		;f1d5	20 dc 	  .
	dec b			;f1d7	05 	.
	dec de			;f1d8	1b 	.
	call DELETE_CHAR		;f1d9	cd 8b d9 	. . .
	jr lf1f5h		;f1dc	18 17 	. .
lf1deh:
	ld a,b			;f1de	78 	x
	and a			;f1df	a7 	.
	jr z,lf1f5h		;f1e0	28 13 	( .
	dec b			;f1e2	05 	.
	dec de			;f1e3	1b 	.
	ld a,07fh		;f1e4	3e 7f 	> 
	call DELETE_CHAR		;f1e6	cd 8b d9 	. . .
	jr lf1b3h		;f1e9	18 c8 	. .
lf1ebh:
	cp 00dh		;f1eb	fe 0d 	. .
	jr nz,lf1f5h		;f1ed	20 06 	  .
	ld a,00dh		;f1ef	3e 0d 	> .
	ld (de),a			;f1f1	12 	.
	jp PRINT_CR_LF		;f1f2	c3 7d d9 	. } .
lf1f5h:
	ld a,007h		;f1f5	3e 07 	> .
	call TXT_OUTPUT		;f1f7	cd 5a bb 	. Z .
	jr lf1b3h		;f1fa	18 b7 	. .

;=======================================================================
RSX_CLI:
;=======================================================================
	cp 002h		;f1fc	fe 02 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;f1fe	d2 9f fb 	. . .
	and a			;f201	a7 	.
	jr z,lf222h		;f202	28 1e 	( .
	call ld8f7h		;f204	cd f7 d8 	. . .
	push hl			;f207	e5 	.
	call sub_da80h		;f208	cd 80 da 	. . .
	ld a,b			;f20b	78 	x
	and a			;f20c	a7 	.
	jp z,ld51ah		;f20d	ca 1a d5 	. . .
	cp 050h		;f210	fe 50 	. P
	jp nc,ld51ah		;f212	d2 1a d5 	. . .
	ld c,b			;f215	48 	H
	ld b,000h		;f216	06 00 	. .
	pop de			;f218	d1 	.
	push de			;f219	d5 	.
	ldir		;f21a	ed b0 	. .
	pop hl			;f21c	e1 	.
	ld a,00dh		;f21d	3e 0d 	> .
	ld (de),a			;f21f	12 	.
	jr lf249h		;f220	18 27 	. '
lf222h:
	call sub_f233h		;f222	cd 33 f2 	. 3 .
	call ld8f7h		;f225	cd f7 d8 	. . .
	ld a,(hl)			;f228	7e 	~
	cp 00dh		;f229	fe 0d 	. .
	jp z,DELETE_CHAR		;f22b	ca 8b d9 	. . .
	call sub_f246h		;f22e	cd 46 f2 	. F .
	jr lf222h		;f231	18 ef 	. .
sub_f233h:
	ld a,07ch		;f233	3e 7c 	> |
	call TXT_OUTPUT		;f235	cd 5a bb 	. Z .
	call sub_f1adh		;f238	cd ad f1 	. . .
	cp 0fch		;f23b	fe fc 	. .
	ret nz			;f23d	c0 	.
	ld hl,lfefdh		;f23e	21 fd fe 	! . .
	call DISPLAY_MSG		;f241	cd 6a d9 	. j .
	jr sub_f233h		;f244	18 ed 	. .
sub_f246h:
	call ld8f7h		;f246	cd f7 d8 	. . .
lf249h:
	xor a			;f249	af 	.
	ld (iy+012h),a		;f24a	fd 77 12 	. w .
	ld de,0bee0h		;f24d	11 e0 be 	. . .
	ld a,(hl)			;f250	7e 	~
	cp 02ch		;f251	fe 2c 	. ,
	jp z,MSG_BAD_CHAR		;f253	ca 93 fb 	. . .
	cp 07ch		;f256	fe 7c 	. |
	jr nz,lf25bh		;f258	20 01 	  .
	inc hl			;f25a	23 	#
lf25bh:
	ld a,(hl)			;f25b	7e 	~
	cp 00dh		;f25c	fe 0d 	. .
	jr z,lf27ch		;f25e	28 1c 	( .
	cp 020h		;f260	fe 20 	.
	jp c,MSG_BAD_CHAR		;f262	da 93 fb 	. . .
	cp 061h		;f265	fe 61 	. a
	call nc,FUNC_SUBTRACT_32		;f267	d4 9d d9 	. . .
	cp 05bh		;f26a	fe 5b 	. [
	jp nc,MSG_BAD_CHAR		;f26c	d2 93 fb 	. . .
	cp 02ch		;f26f	fe 2c 	. ,
	jr z,lf27ch		;f271	28 09 	( .
	cp 020h		;f273	fe 20 	.
	jr z,lf27ch		;f275	28 05 	( .
	inc hl			;f277	23 	#
	inc de			;f278	13 	.
	ld (de),a			;f279	12 	.
	jr lf25bh		;f27a	18 df 	. .
lf27ch:
	ld a,(de)			;f27c	1a 	.
	set 7,a		;f27d	cb ff 	. .
	ld (de),a			;f27f	12 	.
	ld a,(hl)			;f280	7e 	~
	cp 00dh		;f281	fe 0d 	. .
	jr nz,lf299h		;f283	20 14 	  .
lf285h:
	ld hl,0bee1h		;f285	21 e1 be 	! . .
	call KL_FIND_COMMAND		;f288	cd d4 bc 	. . .
	jp nc,MSG_UNKNOWN_CMD		;f28b	d2 a3 fb 	. . .
	ld a,(iy+012h)		;f28e	fd 7e 12 	. ~ .
	push iy		;f291	fd e5 	. .
	call KL_FAR_PCHL		;f293	cd 1b 00 	. . .
	pop iy		;f296	fd e1 	. .
	ret			;f298	c9 	.
lf299h:
	inc hl			;f299	23 	#
	push hl			;f29a	e5 	.
	push iy		;f29b	fd e5 	. .
	pop hl			;f29d	e1 	.
	ld de,00277h		;f29e	11 77 02 	. w .
	add hl,de			;f2a1	19 	.
	push hl			;f2a2	e5 	.
	pop ix		;f2a3	dd e1 	. .
	ld de,0028fh		;f2a5	11 8f 02 	. . .
	push iy		;f2a8	fd e5 	. .
	pop hl			;f2aa	e1 	.
	add hl,de			;f2ab	19 	.
	ex de,hl			;f2ac	eb 	.
	pop hl			;f2ad	e1 	.
lf2aeh:
	ld a,(hl)			;f2ae	7e 	~
	cp 026h		;f2af	fe 26 	. &
	jr z,lf32bh		;f2b1	28 78 	( x
	cp 022h		;f2b3	fe 22 	. "
	jr z,lf302h		;f2b5	28 4b 	( K
	cp 02ch		;f2b7	fe 2c 	. ,
	jp z,lf378h		;f2b9	ca 78 f3 	. x .
	cp 020h		;f2bc	fe 20 	.
	jp z,lf378h		;f2be	ca 78 f3 	. x .
	cp 040h		;f2c1	fe 40 	. @
	jp z,lf37ch		;f2c3	ca 7c f3 	. | .
	cp 00dh		;f2c6	fe 0d 	. .
	jp z,MSG_BAD_CHAR		;f2c8	ca 93 fb 	. . .
	cp 030h		;f2cb	fe 30 	. 0
	jr c,lf2d4h		;f2cd	38 05 	8 .
	cp 03ah		;f2cf	fe 3a 	. :
	jp c,lf3aah		;f2d1	da aa f3 	. . .
lf2d4h:
	xor a			;f2d4	af 	.
	ld (0bee0h),a		;f2d5	32 e0 be 	2 . .
lf2d8h:
	ld a,h			;f2d8	7c 	|
	ld (de),a			;f2d9	12 	.
	dec de			;f2da	1b 	.
	ld a,l			;f2db	7d 	}
	ld (de),a			;f2dc	12 	.
	dec de			;f2dd	1b 	.
	dec ix		;f2de	dd 2b 	. +
	dec ix		;f2e0	dd 2b 	. +
	ld (ix+000h),e		;f2e2	dd 73 00 	. s .
	ld (ix+001h),d		;f2e5	dd 72 01 	. r .
	inc (iy+012h)		;f2e8	fd 34 12 	. 4 .
	ld b,000h		;f2eb	06 00 	. .
lf2edh:
	ld a,(hl)			;f2ed	7e 	~
	cp 02ch		;f2ee	fe 2c 	. ,
	jr z,lf30ah		;f2f0	28 18 	( .
	cp 020h		;f2f2	fe 20 	.
	jr z,lf30ah		;f2f4	28 14 	( .
	cp 022h		;f2f6	fe 22 	. "
	jr z,lf310h		;f2f8	28 16 	( .
	cp 00dh		;f2fa	fe 0d 	. .
	jr z,lf310h		;f2fc	28 12 	( .
lf2feh:
	inc b			;f2fe	04 	.
	inc hl			;f2ff	23 	#
	jr lf2edh		;f300	18 eb 	. .
lf302h:
	inc hl			;f302	23 	#
	ld a,001h		;f303	3e 01 	> .
	ld (0bee0h),a		;f305	32 e0 be 	2 . .
	jr lf2d8h		;f308	18 ce 	. .
lf30ah:
	ld a,(0bee0h)		;f30a	3a e0 be 	: . .
	and a			;f30d	a7 	.
	jr nz,lf2feh		;f30e	20 ee 	  .
lf310h:
	ld a,b			;f310	78 	x
	ld (de),a			;f311	12 	.
	dec de			;f312	1b 	.
lf313h:
	ld a,(hl)			;f313	7e 	~
	cp 00dh		;f314	fe 0d 	. .
	jp z,lf285h		;f316	ca 85 f2 	. . .
	inc hl			;f319	23 	#
	cp 022h		;f31a	fe 22 	. "
	jr z,lf313h		;f31c	28 f5 	( .
	cp 02ch		;f31e	fe 2c 	. ,
	jp z,lf2aeh		;f320	ca ae f2 	. . .
	cp 020h		;f323	fe 20 	.
	jp z,lf2aeh		;f325	ca ae f2 	. . .
	jp MSG_BAD_CHAR		;f328	c3 93 fb 	. . .
lf32bh:
	inc hl			;f32b	23 	#
	ex de,hl			;f32c	eb 	.
	push hl			;f32d	e5 	.
	ld hl,RESET_ENTRY_RST_0		;f32e	21 00 00 	! . .
lf331h:
	ld a,(de)			;f331	1a 	.
	cp 061h		;f332	fe 61 	. a
	call nc,FUNC_SUBTRACT_32		;f334	d4 9d d9 	. . .
	cp 030h		;f337	fe 30 	. 0
	jr c,lf35fh		;f339	38 24 	8 $
	cp 047h		;f33b	fe 47 	. G
	jr nc,lf35fh		;f33d	30 20 	0
	cp 03ah		;f33f	fe 3a 	. :
	jr c,lf345h		;f341	38 02 	8 .
	sub 007h		;f343	d6 07 	. .
lf345h:
	add hl,hl			;f345	29 	)
	add hl,hl			;f346	29 	)
	add hl,hl			;f347	29 	)
	add hl,hl			;f348	29 	)
	sub 030h		;f349	d6 30 	. 0
	ld c,a			;f34b	4f 	O
	ld b,000h		;f34c	06 00 	. .
	add hl,bc			;f34e	09 	.
	inc de			;f34f	13 	.
	ld a,(de)			;f350	1a 	.
	cp 00dh		;f351	fe 0d 	. .
	jr z,lf364h		;f353	28 0f 	( .
	cp 02ch		;f355	fe 2c 	. ,
	jr z,lf364h		;f357	28 0b 	( .
	cp 020h		;f359	fe 20 	.
	jr z,lf364h		;f35b	28 07 	( .
	jr lf331h		;f35d	18 d2 	. .
lf35fh:
	pop hl			;f35f	e1 	.
	ex de,hl			;f360	eb 	.
	jp MSG_BAD_CHAR		;f361	c3 93 fb 	. . .
lf364h:
	dec ix		;f364	dd 2b 	. +
	dec ix		;f366	dd 2b 	. +
	ld (ix+000h),l		;f368	dd 75 00 	. u .
	ld (ix+001h),h		;f36b	dd 74 01 	. t .
	inc (iy+012h)		;f36e	fd 34 12 	. 4 .
	pop hl			;f371	e1 	.
	ex de,hl			;f372	eb 	.
	cp 00dh		;f373	fe 0d 	. .
	jp z,lf285h		;f375	ca 85 f2 	. . .
lf378h:
	inc hl			;f378	23 	#
	jp lf2aeh		;f379	c3 ae f2 	. . .
lf37ch:
	ex de,hl			;f37c	eb 	.
	inc de			;f37d	13 	.
	ld a,(de)			;f37e	1a 	.
	inc de			;f37f	13 	.
	cp 031h		;f380	fe 31 	. 1
	jp c,MSG_BAD_CHAR		;f382	da 93 fb 	. . .
	cp 03ah		;f385	fe 3a 	. :
	jp nc,MSG_BAD_CHAR		;f387	d2 93 fb 	. . .
	push hl			;f38a	e5 	.
	sub 031h		;f38b	d6 31 	. 1
	add a,a			;f38d	87 	.
	ld b,000h		;f38e	06 00 	. .
	ld c,a			;f390	4f 	O
	push de			;f391	d5 	.
	ld hl,0bf20h		;f392	21 20 bf 	!   .
	add hl,bc			;f395	09 	.
	ld e,(hl)			;f396	5e 	^
	inc hl			;f397	23 	#
	ld d,(hl)			;f398	56 	V
	ex de,hl			;f399	eb 	.
	pop de			;f39a	d1 	.
	ld a,(de)			;f39b	1a 	.
	cp 00dh		;f39c	fe 0d 	. .
	jr z,lf364h		;f39e	28 c4 	( .
	cp 02ch		;f3a0	fe 2c 	. ,
	jr z,lf364h		;f3a2	28 c0 	( .
	cp 020h		;f3a4	fe 20 	.
	jr z,lf364h		;f3a6	28 bc 	( .
	jr lf35fh		;f3a8	18 b5 	. .
lf3aah:
	ex de,hl			;f3aa	eb 	.
	push hl			;f3ab	e5 	.
	ld hl,RESET_ENTRY_RST_0		;f3ac	21 00 00 	! . .
lf3afh:
	ld a,(de)			;f3af	1a 	.
	cp 030h		;f3b0	fe 30 	. 0
	jr c,lf35fh		;f3b2	38 ab 	8 .
	cp 03ah		;f3b4	fe 3a 	. :
	jr nc,lf35fh		;f3b6	30 a7 	0 .
	sub 030h		;f3b8	d6 30 	. 0
	add hl,hl			;f3ba	29 	)
	push hl			;f3bb	e5 	.
	add hl,hl			;f3bc	29 	)
	add hl,hl			;f3bd	29 	)
	pop bc			;f3be	c1 	.
	add hl,bc			;f3bf	09 	.
	ld b,000h		;f3c0	06 00 	. .
	ld c,a			;f3c2	4f 	O
	add hl,bc			;f3c3	09 	.
	inc de			;f3c4	13 	.
	ld a,(de)			;f3c5	1a 	.
	cp 00dh		;f3c6	fe 0d 	. .
	jr z,lf364h		;f3c8	28 9a 	( .
	cp 02ch		;f3ca	fe 2c 	. ,
	jr z,lf364h		;f3cc	28 96 	( .
	cp 020h		;f3ce	fe 20 	.
	jr z,lf364h		;f3d0	28 92 	( .
	jr lf3afh		;f3d2	18 db 	. .

;=======================================================================
RSX_ROMS:
;=======================================================================
	cp 010h		;f3d4	fe 10 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;f3d6	d2 9f fb 	. . .
	ld b,a			;f3d9	47 	G
	xor a			;f3da	af 	.
	ld (0bebfh),a		;f3db	32 bf be 	2 . .
	ld a,(ix+001h)		;f3de	dd 7e 01 	. ~ .
	and a			;f3e1	a7 	.
	call nz,sub_f48bh		;f3e2	c4 8b f4 	. . .
	ld a,b			;f3e5	78 	x
	and a			;f3e6	a7 	.
	call z,sub_f3fdh		;f3e7	cc fd f3 	. . .
	ld (0bf12h),a		;f3ea	32 12 bf 	2 . .
	ld hl,0bf22h		;f3ed	21 22 bf 	! " .
lf3f0h:
	ld a,(ix+000h)		;f3f0	dd 7e 00 	. ~ .
	ld (hl),a			;f3f3	77 	w
	inc ix		;f3f4	dd 23 	. #
	inc ix		;f3f6	dd 23 	. #
	dec hl			;f3f8	2b 	+
	djnz lf3f0h		;f3f9	10 f5 	. .
	jr lf454h		;f3fb	18 57 	. W
sub_f3fdh:
	ld a,(iy+001h)		;f3fd	fd 7e 01 	. ~ .
	sub 030h		;f400	d6 30 	. 0
	ld ix,0bef2h		;f402	dd 21 f2 be 	. ! . .
	ld (ix+000h),a		;f406	dd 77 00 	. w .
	ld (ix+001h),000h		;f409	dd 36 01 00 	. 6 . .
	ld a,001h		;f40d	3e 01 	> .
	ld b,a			;f40f	47 	G
	ret			;f410	c9 	.

;=======================================================================
RSX_ZAP:
;=======================================================================
	cp 010h		;f411	fe 10 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;f413	d2 9f fb 	. . .
	ld b,010h		;f416	06 10 	. .
	ld hl,0bf13h		;f418	21 13 bf 	!
lf41bh:
	ld (hl),0ffh		;f41b	36 ff 	6 .
	inc hl			;f41d	23 	#
	djnz lf41bh		;f41e	10 fb 	. .
	ld b,a			;f420	47 	G
	xor a			;f421	af 	.
	ld (0bebfh),a		;f422	32 bf be 	2 . .
	ld a,(ix+001h)		;f425	dd 7e 01 	. ~ .
	and a			;f428	a7 	.
	call nz,sub_f48bh		;f429	c4 8b f4 	. . .
	ld a,b			;f42c	78 	x
	and a			;f42d	a7 	.
	jr z,lf450h		;f42e	28 20 	(
	ld a,010h		;f430	3e 10 	> .
	ld (0bf12h),a		;f432	32 12 bf 	2 . .
lf435h:
	ld a,(ix+000h)		;f435	dd 7e 00 	. ~ .
	cp 010h		;f438	fe 10 	. .
	jp nc,MSG_NO_MATCH		;f43a	d2 ab fb 	. . .
	ld e,a			;f43d	5f 	_
	ld d,000h		;f43e	16 00 	. .
	ld hl,0bf13h		;f440	21 13 bf 	! . .
	add hl,de			;f443	19 	.
	ld a,(hl)			;f444	7e 	~
	xor 0ffh		;f445	ee ff 	. .
	ld (hl),a			;f447	77 	w
	inc ix		;f448	dd 23 	. #
	inc ix		;f44a	dd 23 	. #
	djnz lf435h		;f44c	10 e7 	. .
	jr lf454h		;f44e	18 04 	. .
lf450h:
	xor a			;f450	af 	.
	ld (0bf12h),a		;f451	32 12 bf 	2 . .
lf454h:
	call JUMP_RESTORE		;f454	cd 37 bd 	. 7 .
	ld de,0bf00h		;f457	11 00 bf 	. . .
	ld hl,KL_ROM_WALK		;f45a	21 cb bc 	! . .
	ld bc,00003h		;f45d	01 03 00 	. . .
	ldir		;f460	ed b0 	. .
	ld bc,07fc0h		;f462	01 c0 7f 	. . 
	out (c),c		;f465	ed 49 	. I
	call KL_CHOKE_OFF		;f467	cd c8 bc 	. . .
	ld a,d			;f46a	7a 	z
	and 0c0h		;f46b	e6 c0 	. .
	cp 040h		;f46d	fe 40 	. @
	jr z,lf479h		;f46f	28 08 	( .
	ld (0be91h),de		;f471	ed 53 91 be 	. S . .
	ld a,b			;f475	78 	x
	ld (0be90h),a		;f476	32 90 be 	2 . .
lf479h:
	ld de,04000h		;f479	11 00 40 	. . @
	ld hl,lf4b5h		;f47c	21 b5 f4 	! . .
	ld bc,00090h		;f47f	01 90 00 	. . .
	ldir		;f482	ed b0 	. .
	ld c,a			;f484	4f 	O
	ld hl,04000h		;f485	21 00 40 	! . @
	jp MC_START_PROGRAM		;f488	c3 16 bd 	. . .
sub_f48bh:
	ld a,b			;f48b	78 	x
	and a			;f48c	a7 	.
	ret z			;f48d	c8 	.
	dec b			;f48e	05 	.
	push bc			;f48f	c5 	.
	call sub_da80h		;f490	cd 80 da 	. . .
	ld a,b			;f493	78 	x
	pop bc			;f494	c1 	.
	inc ix		;f495	dd 23 	. #
	inc ix		;f497	dd 23 	. #

	ld (0bebfh),a		;f499	32 bf be 	2 . .
	and a			;f49c	a7 	.
	ret z			;f49d	c8 	.
	push bc			;f49e	c5
	ld de,ROM_SELECT_DESELECT_RELOCATED		;f49f	11 c0 be 	. . .
	ld c,a			;f4a2	4f 	O
	ld b,000h		;f4a3	06 00 	. .
	ldir		;f4a5	ed b0 	. .
	ld a,(iy+000h)		;f4a7	fd 7e 00 	. ~ .
	ld (0bebeh),a		;f4aa	32 be be 	2 . .
	ld hl,lf779h		;f4ad	21 79 f7 	! y .
	ld (0bebch),hl		;f4b0	22 bc be 	" . .
	pop bc			;f4b3	c1 	.
	ret			;f4b4	c9 	.
lf4b5h:
	push hl			;f4b5	e5 	.
	push de			;f4b6	d5 	.
	push bc			;f4b7	c5 	.
	ld hl,0403dh		;f4b8	21 3d 40 	! = @
	ld a,0c3h		;f4bb	3e c3 	> .
	ld (KL_ROM_WALK),a		;f4bd	32 cb bc 	2 . .
	ld (0bccch),hl		;f4c0	22 cc bc 	" . .
	ld hl,(0be91h)		;f4c3	2a 91 be 	* . .
	ld (0befdh),hl		;f4c6	22 fd be 	" . .
	ld a,(0be90h)		;f4c9	3a 90 be 	: . .
	ld (0beffh),a		;f4cc	32 ff be 	2 . .
	call 04023h		;f4cf	cd 23 40 	. # @
	pop bc			;f4d2	c1 	.
	pop de			;f4d3	d1 	.
	pop hl			;f4d4	e1 	.
	rst 18h			;f4d5	df 	.
	dw  0BEFDh ;z80dasm didn't understand this bit. So here is how this works
	; 007   &0018   FAR CALL (RST 3)
  ;     Action: Calls a routine anywhere in RAM or ROM
  ;     Entry:  No entry conditions - all  the  registers apart from IY
  ;             are passed to the destination routine unaltered
  ;     Exit:   IY is preserved, and the other  registers are as set by
  ;             the destination routine or are returned unchanged
  ;     Notes:  The RST 3 instruction is followed by a two byte in-line
  ;             address.  At this address,  there  is  a three byte far
  ;             address, which is defined as follows:
  ;               bytes 0 and 1 give the address of the routine to be
  ;                 called
  ;               byte 2 is the ROM select byte which has values as
  ;                 follows:
  ;                 &00 to &FB-- select the given upper ROM, enable the
  ;                 upper ROM and disable the lower ROM
  ;                 &FC - no change to the ROM selection, enable the
  ;                 upper and lower ROMs
  ;                 &FD - no change to the ROM selection, enable the
  ;                 upper ROM and disable the lower ROM
  ;                 &FE - no change to the ROM selection, disable the
  ;                 upper ROM and enable the lower ROM
  ;                 &FF - no change to the ROM selection, disable the
  ;                 upper and lower ROMs
  ;             When it is retumed  from,  the  ROM selection and state
  ;             are restored to their settings before the RST 3 command
			 ;
				; 							 **************************************************************
				; 							 *                          FUNCTION                          *
				; 							 **************************************************************
				; 							 undefined FUN_ram_f4d8()
			 ; undefined         A:1            <RETURN>
				; 							 FUN_ram_f4d8                                    XREF[2]:     FUN_ram_c1f9:c21e(c),
				;																																						ram:c236(c)
DETERMINE_BASIC_VERSION:
	ld C,0x0 ;f4d8 ram:f4d8 0e 00
	;ROM 0 is the basic rom

	call KL_PROBE_ROM		;f4da	cd 15 b9 	. . .

	; 007   &B915   KL PROBE ROM
	;       Action: Gets the class and version of a specified ROM
	;       Entry:  C contains the ROM select address of the required ROM
	;       Exit:   A contains the class of  the  ROM,  H holds the version
	;               number, L holds me  mark  number,  B  and the flags are
	;               corrupt, and all other registers are preserved
	;       Notes:  The ROM class may be one of ine following:
	;                  &00 - a foregroumd ROM
	;                  &01 - a background ROM
	;                  &02 - an extension foreground ROM
	;                  &80 - the built in ROM (ie the BASIC ROM)

	ld a,h			;f4dd	7c 	|
	and a			;f4de	a7 	.
	ret nz			;f4df	c0 	.
	;So if the basic version is not zero, then we quit here, so CPC664/CPC6128 etc. anything not a CPC464
	;Basic 1.0 is CPC464.
	;I think these next bits poke the bios with the adjusted calls for the 464.
	ld a,(0bccfh)		;f4e0	3a cf bc 	: . .
	cp 032h		;f4e3	fe 32 	. 2
	ret nz			;f4e5	c0 	.
	ld a,036h		;f4e6	3e 36 	> 6
	ld (0bccfh),a		;f4e8	32 cf bc 	2 . .
	;0bccf is part of KL INIT BACK &BCCE
	ld a,00eh		;f4eb	3e 0e 	> .
	ld (0b9e0h),a		;f4ed	32 e0 b9 	2 . .
	xor a			;f4f0	af 	.
	ret			;f4f1	c9 	.
	ld a,(0bf12h)		;f4f2	3a 12 bf 	: . .
	and a			;f4f5	a7 	.
	jr z,lf51bh		;f4f6	28 23 	( #
	push ix		;f4f8	dd e5 	. .
	ld ix,0bf22h		;f4fa	dd 21 22 bf 	. ! " .
	ld b,a			;f4fe	47 	G
lf4ffh:
	ld a,(ix+000h)		;f4ff	dd 7e 00 	. ~ .
	and a			;f502	a7 	.
	jr z,lf515h		;f503	28 10 	( .
	ld c,b			;f505	48 	H
	dec c			;f506	0d 	.
	cp 0ffh		;f507	fe ff 	. .
	jr z,lf50ch		;f509	28 01 	( .
	ld c,a			;f50b	4f 	O
lf50ch:
	push bc			;f50c	c5 	.
	push ix		;f50d	dd e5 	. .
	call KL_INIT_BACK		;f50f	cd ce bc 	. . .
	pop ix		;f512	dd e1 	. .
	pop bc			;f514	c1 	.
lf515h:
	dec ix		;f515	dd 2b 	. +
	djnz lf4ffh		;f517	10 e6 	. .
	pop ix		;f519	dd e1 	. .
lf51bh:
	push de			;f51b	d5 	.
	push hl			;f51c	e5 	.
	ld hl,0bf00h		;f51d	21 00 bf 	! . .
	ld de,KL_ROM_WALK		;f520	11 cb bc 	. . .
	ld bc,00003h		;f523	01 03 00 	. . .
	ldir		;f526	ed b0 	. .
	ld a,(0bebfh)		;f528	3a bf be 	: . .
	and a			;f52b	a7 	.
	jr z,lf53eh		;f52c	28 10 	( .
	ld hl,ROM_SELECT_DESELECT_RELOCATED		;f52e	21 c0 be 	! . .

	ld b,08ch		;f531	06 8c 	. .
	ld c,a			;f533	4f 	O
	call KM_SET_EXPAND		;f534	cd 0f bb 	. . .
	rst 18h			;f537	df 	.
	cp h			;f538	bc 	.
	cp (hl)			;f539	be 	.
	xor a			;f53a	af 	.
	ld (0bf13h),a		;f53b	32 13 bf 	2 . .
lf53eh:
	pop hl			;f53e	e1 	.
	pop de			;f53f	d1 	.
	ret			;f540	c9 	.
sub_f541h:
	xor a			;f541	af 	.
	ld (iy+00eh),a		;f542	fd 77 0e 	. w .
	ld (iy+00dh),a		;f545	fd 77 0d 	. w .
	ld de,lf570h		;f548	11 70 f5 	. p .
	push iy		;f54b	fd e5 	. .
	pop hl			;f54d	e1 	.
	ld bc,00086h		;f54e	01 86 00 	. . .
	add hl,bc			;f551	09 	.
	ld c,(iy+000h)		;f552	fd 4e 00 	. N .
	ld b,080h		;f555	06 80 	. .
	call KL_INIT_EVENT		;f557	cd ef bc 	. . .
	ld a,032h		;f55a	3e 32 	> 2
	ld (0bf13h),a		;f55c	32 13 bf 	2 . .
	ld de,00084h		;f55f	11 84 00 	. . .
	push iy		;f562	fd e5 	. .
	pop hl			;f564	e1 	.
	add hl,de			;f565	19 	.
	jp KL_ADD_FRAME_FLY		;f566	c3 da bc 	. . .
	ld a,(iy+00bh)		;f569	fd 7e 0b 	. ~ .
	and a			;f56c	a7 	.
	ret nz			;f56d	c0 	.
	pop hl			;f56e	e1 	.
	ret			;f56f	c9 	.
lf570h:
	ld a,(iy+00eh)		;f570	fd 7e 0e 	. ~ .
	and a			;f573	a7 	.
	ret nz			;f574	c0 	.
	inc a			;f575	3c 	<
	ld (iy+00eh),a		;f576	fd 77 0e 	. w .
	bit 1,(iy+00dh)		;f579	fd cb 0d 4e 	. . . N
	call nz,sub_f6a4h		;f57d	c4 a4 f6 	. . .
	bit 2,(iy+00dh)		;f580	fd cb 0d 56 	. . . V
	call nz,sub_f703h		;f584	c4 03 f7 	. . .
	bit 3,(iy+00dh)		;f587	fd cb 0d 5e 	. . . ^
	call nz,sub_f593h		;f58b	c4 93 f5 	. . .
	xor a			;f58e	af 	.
	ld (iy+00eh),a		;f58f	fd 77 0e 	. w .
	ret			;f592	c9 	.
sub_f593h:
	ld a,(iy+03bh)		;f593	fd 7e 3b 	. ~ ;
	and a			;f596	a7 	.
	jr z,lf5a9h		;f597	28 10 	( .
	ld h,(iy+049h)		;f599	fd 66 49 	. f I
	ld l,(iy+048h)		;f59c	fd 6e 48 	. n H
	dec hl			;f59f	2b 	+
	ld (iy+048h),l		;f5a0	fd 75 48 	. u H
	ld (iy+049h),h		;f5a3	fd 74 49 	. t I
	ld a,h			;f5a6	7c 	|
	or l			;f5a7	b5 	.
	ret nz			;f5a8	c0 	.
lf5a9h:
	ld (iy+03bh),a		;f5a9	fd 77 3b 	. w ;
	bit 4,(iy+041h)		;f5ac	fd cb 41 66 	. . A f
	jr nz,lf5b9h		;f5b0	20 07 	  .
	dec a			;f5b2	3d 	=
	ld (iy+040h),a		;f5b3	fd 77 40 	. w @
	ld (iy+03fh),a		;f5b6	fd 77 3f 	. w ?
lf5b9h:
	ld a,(0be5fh)		;f5b9	3a 5f be 	: _ .
	and a			;f5bc	a7 	.
	jr nz,lf5c9h		;f5bd	20 0a 	  .
	ld bc,0fa7eh		;f5bf	01 7e fa 	. ~ .
	out (c),a		;f5c2	ed 79 	. y
	ld a,0ffh		;f5c4	3e ff 	> .
	call sub_f174h		;f5c6	cd 74 f1 	. t .
lf5c9h:
	res 3,(iy+00dh)		;f5c9	fd cb 0d 9e 	. . . .
	ret			;f5cd	c9 	.

;=======================================================================
RSX_PRBUFF:
;=======================================================================
	ld de,RESET_ENTRY_RST_0		;f5ce	11 00 00 	. . .
	ld hl,03fffh		;f5d1	21 ff 3f 	! . ?
	cp 003h		;f5d4	fe 03 	. .
	jr z,lf5f1h		;f5d6	28 19 	( .
	cp 002h		;f5d8	fe 02 	. .
	jr z,lf5fah		;f5da	28 1e 	( .
	and a			;f5dc	a7 	.
	jr z,lf5ech		;f5dd	28 0d 	( .
	cp 001h		;f5df	fe 01 	. .
	jp nz,MSG_WRONG_PARAMETER_AMT		;f5e1	c2 97 fb 	. . .
	ld a,(ix+000h)		;f5e4	dd 7e 00 	. ~ .
	cp 011h		;f5e7	fe 11 	. .
	ret nc			;f5e9	d0 	.
	jr lf607h		;f5ea	18 1b 	. .
lf5ech:
	ld a,(iy+00bh)		;f5ec	fd 7e 0b 	. ~ .
	jr lf607h		;f5ef	18 16 	. .
lf5f1h:
	ld a,(ix+000h)		;f5f1	dd 7e 00 	. ~ .
	inc ix		;f5f4	dd 23 	. #
	inc ix		;f5f6	dd 23 	. #
	jr lf5fbh		;f5f8	18 01 	. .
lf5fah:
	xor a			;f5fa	af 	.
lf5fbh:
	ld l,(ix+000h)		;f5fb	dd 6e 00 	. n .
	ld h,(ix+001h)		;f5fe	dd 66 01 	. f .
	ld e,(ix+002h)		;f601	dd 5e 02 	. ^ .
	ld d,(ix+003h)		;f604	dd 56 03 	. V .
lf607h:
	ld (iy+00ch),a		;f607	fd 77 0c 	. w .
	ld (iy+050h),e		;f60a	fd 73 50 	. s P
	ld (iy+051h),d		;f60d	fd 72 51 	. r Q
	ld (iy+052h),l		;f610	fd 75 52 	. u R
	ld (iy+053h),h		;f613	fd 74 53 	. t S
	ld (iy+054h),e		;f616	fd 73 54 	. s T
	ld (iy+055h),d		;f619	fd 72 55 	. r U
	ld (iy+056h),e		;f61c	fd 73 56 	. s V
	ld (iy+057h),d		;f61f	fd 72 57 	. r W
	ld b,001h		;f622	06 01 	. .
	ld de,00072h		;f624	11 72 00 	. r .
	push iy		;f627	fd e5 	. .
	pop hl			;f629	e1 	.
	add hl,de			;f62a	19 	.
	ld de,MC_PRINT_CHAR		;f62b	11 2b bd 	. + .
	ld ix,lf643h		;f62e	dd 21 43 f6 	. ! C .
	call MAKE_JP_AT_DE_USING_HL		;f632	cd 74 de 	. t .
	set 1,(iy+00dh)		;f635	fd cb 0d ce 	. . . .
	xor a			;f639	af 	.
	ld (iy+00eh),a		;f63a	fd 77 0e 	. w .
	ld bc,07fc0h		;f63d	01 c0 7f 	. . 
	out (c),c		;f640	ed 49 	. I
	ret			;f642	c9 	.
lf643h:
	ld b,l			;f643	45 	E
	or 0e5h		;f644	f6 e5 	. .
	push de			;f646	d5 	.
	push bc			;f647	c5 	.
	push af			;f648	f5 	.
	ld a,001h		;f649	3e 01 	> .
	ld (iy+00eh),a		;f64b	fd 77 0e 	. w .
	ld l,(iy+056h)		;f64e	fd 6e 56 	. n V
	ld h,(iy+057h)		;f651	fd 66 57 	. f W
	ld a,(iy+00ch)		;f654	fd 7e 0c 	. ~ .
	and a			;f657	a7 	.
	call nz,sub_f6e5h		;f658	c4 e5 f6 	. . .
	pop af			;f65b	f1 	.
	ld (hl),a			;f65c	77 	w
	ld bc,07fc0h		;f65d	01 c0 7f 	. . 
	out (c),c		;f660	ed 49 	. I
	ld e,(iy+056h)		;f662	fd 5e 56 	. ^ V
	ld d,(iy+057h)		;f665	fd 56 57 	. V W
	inc de			;f668	13 	.
	ld l,(iy+052h)		;f669	fd 6e 52 	. n R
	ld h,(iy+053h)		;f66c	fd 66 53 	. f S
	ld a,e			;f66f	7b 	{
	cp l			;f670	bd 	.
	jr nz,lf678h		;f671	20 05 	  .
	ld a,h			;f673	7c 	|
	cp d			;f674	ba 	.
	call z,sub_f695h		;f675	cc 95 f6 	. . .
lf678h:
	ld l,(iy+054h)		;f678	fd 6e 54 	. n T
	ld h,(iy+055h)		;f67b	fd 66 55 	. f U
	ld a,e			;f67e	7b 	{
	cp l			;f67f	bd 	.
	jr nz,lf686h		;f680	20 04 	  .
	ld a,d			;f682	7a 	z
	cp h			;f683	bc 	.
	jr z,lf69ch		;f684	28 16 	( .
lf686h:
	ld (iy+056h),e		;f686	fd 73 56 	. s V
	ld (iy+057h),d		;f689	fd 72 57 	. r W
	xor a			;f68c	af 	.
	ld (iy+00eh),a		;f68d	fd 77 0e 	. w .
	scf			;f690	37 	7
	pop bc			;f691	c1 	.
	pop de			;f692	d1 	.
	pop hl			;f693	e1 	.
	ret			;f694	c9 	.
sub_f695h:
	ld e,(iy+050h)		;f695	fd 5e 50 	. ^ P
	ld d,(iy+051h)		;f698	fd 56 51 	. V Q
	ret			;f69b	c9 	.
lf69ch:
	xor a			;f69c	af 	.
	ld (iy+00eh),a		;f69d	fd 77 0e 	. w .
	pop bc			;f6a0	c1 	.
	pop de			;f6a1	d1 	.
	pop hl			;f6a2	e1 	.
	ret			;f6a3	c9 	.
sub_f6a4h:
	call MC_BUSY_PRINTER		;f6a4	cd 2e bd 	. . .
	ret c			;f6a7	d8 	.
	ld l,(iy+054h)		;f6a8	fd 6e 54 	. n T
	ld h,(iy+055h)		;f6ab	fd 66 55 	. f U
	ld e,(iy+056h)		;f6ae	fd 5e 56 	. ^ V
	ld d,(iy+057h)		;f6b1	fd 56 57 	. V W
	ld a,l			;f6b4	7d 	}
	cp e			;f6b5	bb 	.
	jr nz,lf6bbh		;f6b6	20 03 	  .
	ld a,h			;f6b8	7c 	|
	cp d			;f6b9	ba 	.
	ret z			;f6ba	c8 	.
lf6bbh:
	push hl			;f6bb	e5 	.
	ld a,(iy+00ch)		;f6bc	fd 7e 0c 	. ~ .
	and a			;f6bf	a7 	.
	call nz,sub_f6e5h		;f6c0	c4 e5 f6 	. . .
	ld a,(hl)			;f6c3	7e 	~
	ld bc,07fc0h		;f6c4	01 c0 7f 	. . 
	out (c),c		;f6c7	ed 49 	. I
	ld c,a			;f6c9	4f 	O
	pop de			;f6ca	d1 	.
	inc de			;f6cb	13 	.
	ld l,(iy+052h)		;f6cc	fd 6e 52 	. n R
	ld h,(iy+053h)		;f6cf	fd 66 53 	. f S
	ld a,d			;f6d2	7a 	z
	cp h			;f6d3	bc 	.
	jr nz,lf6dbh		;f6d4	20 05 	  .
	ld a,e			;f6d6	7b 	{
	cp l			;f6d7	bd 	.
	call z,sub_f695h		;f6d8	cc 95 f6 	. . .
lf6dbh:
	ld (iy+054h),e		;f6db	fd 73 54 	. s T
	ld (iy+055h),d		;f6de	fd 72 55 	. r U
	ld a,c			;f6e1	79 	y
	jp MC_SEND_PRINTER		;f6e2	c3 31 bd 	. 1 .
sub_f6e5h:
	ld c,a			;f6e5	4f 	O
	ld a,h			;f6e6	7c 	|
	sra a		;f6e7	cb 2f 	. /
	sra a		;f6e9	cb 2f 	. /
	sra a		;f6eb	cb 2f 	. /
	sra a		;f6ed	cb 2f 	. /
	sra a		;f6ef	cb 2f 	. /
	sra a		;f6f1	cb 2f 	. /
	and 003h		;f6f3	e6 03 	. .
sub_f6f5h:
	add a,c			;f6f5	81 	.
	call sub_fa3eh		;f6f6	cd 3e fa 	. > .
	res 7,h		;f6f9	cb bc 	. .
	set 6,h		;f6fb	cb f4 	. .
	ret			;f6fd	c9 	.
lf6feh:
	res 2,(iy+00dh)		;f6fe	fd cb 0d 96 	. . . .
	ret			;f702	c9 	.
sub_f703h:
	ld l,(iy+046h)		;f703	fd 6e 46 	. n F
	ld h,(iy+047h)		;f706	fd 66 47 	. f G
	ld b,008h		;f709	06 08 	. .
lf70bh:
	ld a,(hl)			;f70b	7e 	~
	and a			;f70c	a7 	.
	jr nz,lf6feh		;f70d	20 ef 	  .
	inc hl			;f70f	23 	#
	djnz lf70bh		;f710	10 f9 	. .
	ld a,(0bf13h)		;f712	3a 13 bf 	: . .
	dec a			;f715	3d 	=
	ld (0bf13h),a		;f716	32 13 bf 	2 . .
	ret nz			;f719	c0 	.
	call lf6feh		;f71a	cd fe f6 	. . .
	call sub_da62h		;f71d	cd 62 da 	. b .
	ret nz			;f720	c0 	.
	cp 008h		;f721	fe 08 	. .
	jr nc,lf751h		;f723	30 2c 	0 ,
	set 0,(iy+00dh)		;f725	fd cb 0d c6 	. . . .
	call sub_cab5h		;f729	cd b5 ca 	. . .
	res 0,(iy+00dh)		;f72c	fd cb 0d 86 	. . . .
	bit 5,a		;f730	cb 6f 	. o
	jr nz,lf751h		;f732	20 1d 	  .
	ld (iy+013h),001h		;f734	fd 36 13 01 	. 6 . .
	call sub_da62h		;f738	cd 62 da 	. b .
	set 0,(iy+00dh)		;f73b	fd cb 0d c6 	. . . .
	call sub_cab5h		;f73f	cd b5 ca 	. . .
	res 0,(iy+00dh)		;f742	fd cb 0d 86 	. . . .
	bit 5,a		;f746	cb 6f 	. o
	call nz,lf751h		;f748	c4 51 f7 	. Q .
	ret nz			;f74b	c0 	.
	ld (iy+013h),000h		;f74c	fd 36 13 00 	. 6 . .
	ret			;f750	c9 	.
lf751h:
	ld hl,lf788h		;f751	21 88 f7 	! . .
	ld de,0bef0h		;f754	11 f0 be 	. . .
	ld bc,SIDE_CALL_RST_2		;f757	01 10 00 	. . .
	ldir		;f75a	ed b0 	. .
	ld ix,0bef0h		;f75c	dd 21 f0 be 	. ! . .
	ld (iy+014h),000h		;f760	fd 36 14 00 	. 6 . .
	set 0,(iy+00dh)		;f764	fd cb 0d c6 	. . . .
	call sub_f798h		;f768	cd 98 f7 	. . .
	push af			;f76b	f5 	.
	res 0,(iy+00dh)		;f76c	fd cb 0d 86 	. . . .
	pop af			;f770	f1 	.
	jp nz,lda14h		;f771	c2 14 da 	. . .
	bit 3,(iy+041h)		;f774	fd cb 41 5e 	. . A ^
	ret z			;f778	c8 	.
lf779h:
	;M key, maybe?
	ld a,04dh		;f779	3e 4d 	> M
	call KM_TEST_KEY		;f77b	cd 1e bb 	. . .
	inc hl			;f77e	23 	#
	ld (hl),0bfh		;f77f	36 bf 	6 .
	inc hl			;f781	23 	#
	inc hl			;f782	23 	#
	ld (hl),07fh		;f783	36 7f 	6 
	jp lda0ah		;f785	c3 0a da 	. . .
lf788h:
	ld b,h			;f788	44 	D
	ld c,c			;f789	49 	I
	ld d,e			;f78a	53 	S
	ld b,e			;f78b	43 	C
	jr nz,$+34		;f78c	20 20
	jr nz,$+34		;f78e	20 20
	jr nz,lf7b2h		;f790	20 20
	jr nz,$+34		;f792	20 20
	ccf			;f794	3f 	?
	ccf			;f795	3f 	?
	ccf			;f796	3f 	?
	ccf			;f797	3f 	?
sub_f798h:
	res 3,(iy+041h)		;f798	fd cb 41 9e 	. . A .
	call sub_d9a0h		;f79c	cd a0 d9 	. . .
	ret nz			;f79f	c0 	.
	and a			;f7a0	a7 	.
	jp z,lda0ah		;f7a1	ca 0a da 	. . .
	cp 081h		;f7a4	fe 81 	. .
	jp nz,lf829h		;f7a6	c2 29 f8 	. ) .
	call sub_ed01h		;f7a9	cd 01 ed 	. . .
	jp nz,lda0fh		;f7ac	c2 0f da 	. . .
lf7afh:
	call sub_ed38h		;f7af	cd 38 ed 	. 8 .
lf7b2h:
	jp nz,lda18h		;f7b2	c2 18 da 	. . .
	call sub_c5ffh		;f7b5	cd ff c5 	. . .
	jp nz,lda0fh		;f7b8	c2 0f da 	. . .
	set 3,(iy+041h)		;f7bb	fd cb 41 de 	. . A .
	ld a,(iy+014h)		;f7bf	fd 7e 14 	. ~ .
	and a			;f7c2	a7 	.
	jp z,lda18h		;f7c3	ca 18 da 	. . .
	push bc			;f7c6	c5 	.
	push hl			;f7c7	e5 	.
	push ix		;f7c8	dd e5 	. .
	cp 002h		;f7ca	fe 02 	. .
	call z,sub_f80bh		;f7cc	cc 0b f8 	. . .
	cp 001h		;f7cf	fe 01 	. .
	call z,sub_f810h		;f7d1	cc 10 f8 	. . .
	cp 003h		;f7d4	fe 03 	. .
	call z,sub_f7e8h		;f7d6	cc e8 f7 	. . .
	pop ix		;f7d9	dd e1 	. .
	pop hl			;f7db	e1 	.
	ld bc,RAM_LAM		;f7dc	01 20 00 	.   .
	add hl,bc			;f7df	09 	.
	pop bc			;f7e0	c1 	.
	push hl			;f7e1	e5 	.
	call sub_ef6ch		;f7e2	cd 6c ef 	. l .
	pop hl			;f7e5	e1 	.
	jr lf7afh		;f7e6	18 c7 	. .
sub_f7e8h:
	ld de,0becfh		;f7e8	11 cf be 	. . .
	inc hl			;f7eb	23 	#
	ld bc,0000ch		;f7ec	01 0c 00 	. . .
l0f7efh: ;Called from an RST 18 (maybe?)
	ldir		;f7ef	ed b0 	. .
	ld a,02eh		;f7f1	3e 2e 	> .
	ld (de),a			;f7f3	12 	.
	inc de			;f7f4	13 	.
	ld bc,00004h		;f7f5	01 04 00 	. . .
	ldir		;f7f8	ed b0 	. .
	ld hl,0becfh		;f7fa	21 cf be 	! . .
	ld b,011h		;f7fd	06 11 	. .
	ld a,(iy+WS_DRIVE_NUMBER)		;f7ff	fd 7e 04 	. ~ .
	push af			;f802	f5 	.
	call sub_cee4h		;f803	cd e4 ce 	. . .
	pop af			;f806	f1 	.
	ld (iy+WS_DRIVE_NUMBER),a		;f807	fd 77 04 	. w .
	ret			;f80a	c9 	.
sub_f80bh:
	call sub_cd6eh		;f80b	cd 6e cd 	. n .
	xor a			;f80e	af 	.
	ret			;f80f	c9 	.
sub_f810h:
	push hl			;f810	e5 	.
	pop ix		;f811	dd e1 	. .
	ld a,(ix+019h)		;f813	dd 7e 19 	. ~ .
	ld (ix+01ch),a		;f816	dd 77 1c 	. w .
	ld a,(ix+01ah)		;f819	dd 7e 1a 	. ~ .
	ld (ix+01dh),a		;f81c	dd 77 1d 	. w .
	inc ix		;f81f	dd 23 	. #
	call sub_db4ch		;f821	cd 4c db 	. L .
	inc ix		;f824	dd 23 	. #
	jp sub_d2c3h		;f826	c3 c3 d2 	. . .
lf829h:
	ld (iy+010h),a		;f829	fd 77 10 	. w .
	ld c,a			;f82c	4f 	O
	ld a,(ix+00eh)		;f82d	dd 7e 0e 	. ~ .
	ld (ix+00ah),a		;f830	dd 77 0a 	. w .
	ld a,(ix+00dh)		;f833	dd 7e 0d 	. ~ .
	ld (ix+009h),a		;f836	dd 77 09 	. w .
	ld a,(ix+00ch)		;f839	dd 7e 0c 	. ~ .
	ld (ix+008h),a		;f83c	dd 77 08 	. w .
	call sub_f868h		;f83f	cd 68 f8 	. h .
	ret z			;f842	c8 	.
	ret nc			;f843	d0 	.
	inc c			;f844	0c 	.
	call sub_f868h		;f845	cd 68 f8 	. h .
	ret z			;f848	c8 	.
	ret nc			;f849	d0 	.
	inc c			;f84a	0c 	.
	call sub_f868h		;f84b	cd 68 f8 	. h .
	ret z			;f84e	c8 	.
	ret nc			;f84f	d0 	.
	inc c			;f850	0c 	.
	call sub_f868h		;f851	cd 68 f8 	. h .
	jp c,lda18h		;f854	da 18 da 	. . .
	ret			;f857	c9 	.
sub_f858h:
	ld b,000h		;f858	06 00 	. .
	ld a,c			;f85a	79 	y
	and 0c0h		;f85b	e6 c0 	. .
	cp 0c0h		;f85d	fe c0 	. .
	call nz,sub_f8e1h		;f85f	c4 e1 f8 	. . .
	cp 040h		;f862	fe 40 	. @
	call z,sub_f8e1h		;f864	cc e1 f8 	. . .
	ret			;f867	c9 	.
sub_f868h:
	call sub_f858h		;f868	cd 58 f8 	. X .
	call sub_ef6ch		;f86b	cd 6c ef 	. l .
	jp nz,lda0fh		;f86e	c2 0f da 	. . .
	ld b,010h		;f871	06 10 	. .
lf873h:
	push ix		;f873	dd e5 	. .
	push hl			;f875	e5 	.
	push bc			;f876	c5 	.
	ld a,(hl)			;f877	7e 	~
	cp (iy+036h)		;f878	fd be 36 	. . 6
	jr nz,lf8d4h		;f87b	20 57 	  W
	inc hl			;f87d	23 	#
	ld b,00bh		;f87e	06 0b 	. .
lf880h:
	ld a,(ix+000h)		;f880	dd 7e 00 	. ~ .
	cp 061h		;f883	fe 61 	. a
	call nc,FUNC_SUBTRACT_32		;f885	d4 9d d9 	. . .
	cp 03fh		;f888	fe 3f 	. ?
	jr z,lf893h		;f88a	28 07 	( .
	ld c,a			;f88c	4f 	O
	ld a,(hl)			;f88d	7e 	~
	and 07fh		;f88e	e6 7f 	. 
	cp c			;f890	b9 	.
	jr nz,lf8d4h		;f891	20 41 	  A
lf893h:
	inc hl			;f893	23 	#
	inc ix		;f894	dd 23 	. #
	djnz lf880h		;f896	10 e8 	. .
	ld a,(hl)			;f898	7e 	~
	and a			;f899	a7 	.
	jr nz,lf8d4h		;f89a	20 38 	  8
	pop bc			;f89c	c1 	.
	pop hl			;f89d	e1 	.
	pop ix		;f89e	dd e1 	. .
	call sub_c5ffh		;f8a0	cd ff c5 	. . .
	jp nz,lda0fh		;f8a3	c2 0f da 	. . .
	set 3,(iy+041h)		;f8a6	fd cb 41 de 	. . A .
	ld a,(iy+014h)		;f8aa	fd 7e 14 	. ~ .
	and a			;f8ad	a7 	.
	jp z,lda18h		;f8ae	ca 18 da 	. . .
	push ix		;f8b1	dd e5 	. .
	push hl			;f8b3	e5 	.
	push bc			;f8b4	c5 	.
	ld e,(iy+WS_DRIVE_NUMBER)		;f8b5	fd 5e 04 	. ^ .
	push de			;f8b8	d5 	.
	cp 002h		;f8b9	fe 02 	. .
	call z,sub_f8e3h		;f8bb	cc e3 f8 	. . .
	cp 001h		;f8be	fe 01 	. .
	call z,sub_f8ebh		;f8c0	cc eb f8 	. . .
	cp 003h		;f8c3	fe 03 	. .
	call z,sub_f8f9h		;f8c5	cc f9 f8 	. . .
	pop de			;f8c8	d1 	.
	ld (iy+WS_DRIVE_NUMBER),e		;f8c9	fd 73 04 	. s .
	pop bc			;f8cc	c1 	.
	push bc			;f8cd	c5 	.
	call sub_f858h		;f8ce	cd 58 f8 	. X .
	call sub_ef6ch		;f8d1	cd 6c ef 	. l .
lf8d4h:
	pop bc			;f8d4	c1 	.
	pop hl			;f8d5	e1 	.
	pop ix		;f8d6	dd e1 	. .
	ld de,RAM_LAM		;f8d8	11 20 00 	.   .
	add hl,de			;f8db	19 	.
	djnz lf873h		;f8dc	10 95 	. .
	jp lda0ah		;f8de	c3 0a da 	. . .
sub_f8e1h:
	inc b			;f8e1	04 	.
	ret			;f8e2	c9 	.
sub_f8e3h:
	call sub_f858h		;f8e3	cd 58 f8 	. X .
	call sub_cd95h		;f8e6	cd 95 cd 	. . .
	xor a			;f8e9	af 	.
	ret			;f8ea	c9 	.
sub_f8ebh:
	ld de,0bf12h		;f8eb	11 12 bf 	. . .
	call sub_f904h		;f8ee	cd 04 f9 	. . .
	call sub_f918h		;f8f1	cd 18 f9 	. . .
	call sub_d1abh		;f8f4	cd ab d1 	. . .
	xor a			;f8f7	af 	.
	ret			;f8f8	c9 	.
sub_f8f9h:
	ld de,0bed4h		;f8f9	11 d4 be 	. . .
	call sub_f904h		;f8fc	cd 04 f9 	. . .
	call sub_cee4h		;f8ff	cd e4 ce 	. . .
	xor a			;f902	af 	.
	ret			;f903	c9 	.
sub_f904h:
	inc hl			;f904	23 	#
	push de			;f905	d5 	.
	ld bc,LOW_JUMP_RST_1		;f906	01 08 00 	. . .
	ldir		;f909	ed b0 	. .
	ld a,02eh		;f90b	3e 2e 	> .
	ld (de),a			;f90d	12 	.
	inc de			;f90e	13 	.
	ld bc,00003h		;f90f	01 03 00 	. . .
	ldir		;f912	ed b0 	. .
	pop hl			;f914	e1 	.
	ld b,00ch		;f915	06 0c 	. .
	ret			;f917	c9 	.
sub_f918h:
	dec hl			;f918	2b 	+
	ld (hl),03ah		;f919	36 3a 	6 :
	dec hl			;f91b	2b 	+
	ld a,(iy+WS_CURRENT_DRIVE_LETTER)		;f91c	fd 7e 03 	. ~ .
	add a,041h		;f91f	c6 41 	. A
	ld (hl),a			;f921	77 	w
	inc b			;f922	04 	.
	inc b			;f923	04 	.
	ret			;f924	c9 	.

;=======================================================================
RSX_ASKRAM:
;=======================================================================
	cp 003h		;f925	fe 03 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;f927	d2 9f fb 	. . .
	ld l,(iy+00bh)		;f92a	fd 6e 0b 	. n .
	ld h,000h		;f92d	26 00 	& .
	cp 001h		;f92f	fe 01 	. .
	jr z,lf93eh		;f931	28 0b 	( .
	and a			;f933	a7 	.
	jp z,MSG_WRONG_PARAMETER_AMT		;f934	ca 97 fb 	. . .
	ld a,(ix+002h)		;f937	dd 7e 02 	. ~ .
lf93ah:
	cp 001h		;f93a	fe 01 	. .
	jr z,lf942h		;f93c	28 04 	( .
lf93eh:
	add hl,hl			;f93e	29 	)
	add hl,hl			;f93f	29 	)
	add hl,hl			;f940	29 	)
	add hl,hl			;f941	29 	)
lf942h:
	ld e,(ix+000h)		;f942	dd 5e 00 	. ^ .
	ld d,(ix+001h)		;f945	dd 56 01 	. V .
	ex de,hl			;f948	eb 	.
	ld (hl),e			;f949	73 	s
	inc hl			;f94a	23 	#
	ld (hl),d			;f94b	72 	r
	ret			;f94c	c9 	.
	ld a,001h		;f94d	3e 01 	> .
	jr lf93ah		;f94f	18 e9 	. .
;=======================================================================
RSX_LOAD:
;=======================================================================
	cp 004h		;f951	fe 04 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;f953	d2 9f fb 	. . .
	and a			;f956	a7 	.
	jp z,MSG_WRONG_PARAMETER_AMT		;f957	ca 97 fb 	. . .
	cp 002h		;f95a	fe 02 	. .
	jr z,lf981h		;f95c	28 23 	( #
	cp 003h		;f95e	fe 03 	. .
	jr z,lf970h		;f960	28 0e 	( .
	call sub_da80h		;f962	cd 80 da 	. . .
	call CAS_IN_OPEN		;f965	cd 77 bc 	. w .
	ret nc			;f968	d0 	.
	ex de,hl			;f969	eb 	.
lf96ah:
	call CAS_IN_DIRECT		;f96a	cd 83 bc 	. . .
	jp CAS_IN_ABANDON		;f96d	c3 7d bc 	. } .
lf970h:
	inc ix		;f970	dd 23 	. #
	inc ix		;f972	dd 23 	. #
	call sub_d1c4h		;f974	cd c4 d1 	. . .
	push hl			;f977	e5 	.
	call sub_da80h		;f978	cd 80 da 	. . .
	call CAS_IN_OPEN		;f97b	cd 77 bc 	. w .
	pop hl			;f97e	e1 	.
	jr lf96ah		;f97f	18 e9 	. .
lf981h:
	call sub_fa37h		;f981	cd 37 fa 	. 7 .
	call sub_da80h		;f984	cd 80 da 	. . .
	call CAS_IN_OPEN		;f987	cd 77 bc 	. w .
	jr nc,lf9bdh		;f98a	30 31 	0 1
	ld hl,04000h		;f98c	21 00 40 	! . @
	call CAS_IN_DIRECT		;f98f	cd 83 bc 	. . .
	call CAS_IN_ABANDON		;f992	cd 7d bc 	. } .
	jp lf9bdh		;f995	c3 bd f9 	. . .

;=======================================================================
RSX_SAVE:
;=======================================================================
	cp 004h		;f998	fe 04 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;f99a	d2 9f fb 	. . .
	cp 002h		;f99d	fe 02 	. .
	jr nz,lf9c7h		;f99f	20 26 	  &
	call sub_fa37h		;f9a1	cd 37 fa 	. 7 .
	call sub_da80h		;f9a4	cd 80 da 	. . .
	call CAS_OUT_OPEN		;f9a7	cd 8c bc 	. . .
	jr nc,lf9bdh		;f9aa	30 11 	0 .
	ld hl,04000h		;f9ac	21 00 40 	! . @
	ld de,04000h		;f9af	11 00 40 	. . @
	ld bc,RESET_ENTRY_RST_0		;f9b2	01 00 00 	. . .
	ld a,002h		;f9b5	3e 02 	> .
	call CAS_OUT_DIRECT		;f9b7	cd 98 bc 	. . .
	call CAS_OUT_CLOSE		;f9ba	cd 8f bc 	. . .
lf9bdh:
	ld bc,07fc0h		;f9bd	01 c0 7f 	. . 
	out (c),c		;f9c0	ed 49 	. I
	xor a			;f9c2	af 	.
	ld (iy+002h),a		;f9c3	fd 77 02 	. w .
	ret			;f9c6	c9 	.
lf9c7h:
	cp 003h		;f9c7	fe 03 	. .
	jp nz,MSG_WRONG_PARAMETER_AMT		;f9c9	c2 97 fb 	. . .
	ld l,(ix+004h)		;f9cc	dd 6e 04 	. n .
	ld h,(ix+005h)		;f9cf	dd 66 05 	. f .
	ld b,(hl)			;f9d2	46 	F
	inc hl			;f9d3	23 	#
	ld e,(hl)			;f9d4	5e 	^
	inc hl			;f9d5	23 	#
	ld d,(hl)			;f9d6	56 	V
	ex de,hl			;f9d7	eb 	.
	push ix		;f9d8	dd e5 	. .
	call CAS_OUT_OPEN		;f9da	cd 8c bc 	. . .
	pop ix		;f9dd	dd e1 	. .
	ret nc			;f9df	d0 	.
	ld l,(ix+002h)		;f9e0	dd 6e 02 	. n .
	ld h,(ix+003h)		;f9e3	dd 66 03 	. f .
	ld e,(ix+000h)		;f9e6	dd 5e 00 	. ^ .
	ld d,(ix+001h)		;f9e9	dd 56 01 	. V .
	ld bc,RESET_ENTRY_RST_0		;f9ec	01 00 00 	. . .
	ld a,002h		;f9ef	3e 02 	> .
	call CAS_OUT_DIRECT		;f9f1	cd 98 bc 	. . .
	ret nc			;f9f4	d0 	.
	call CAS_OUT_CLOSE		;f9f5	cd 8f bc 	. . .
	ret			;f9f8	c9 	.


;=======================================================================
RSX_POKE:
;=======================================================================
	call sub_fa18h		;f9f9	cd 18 fa 	. . .
	ret nz			;f9fc	c0 	.
	call sub_fa3eh		;f9fd	cd 3e fa 	. > .
	ld (hl),c			;fa00	71 	q
	jp lf9bdh		;fa01	c3 bd f9 	. . .

;=======================================================================
RSX_PEEK:
;=======================================================================
	call sub_fa18h		;fa04	cd 18 fa 	. . .
	ret nz			;fa07	c0 	.
	push bc			;fa08	c5 	.
	call sub_fa3eh		;fa09	cd 3e fa 	. > .
	ld a,(hl)			;fa0c	7e 	~
	push af			;fa0d	f5 	.
	call lf9bdh		;fa0e	cd bd f9 	. . .
	pop af			;fa11	f1 	.
	pop hl			;fa12	e1 	.
	ld (hl),a			;fa13	77 	w
	inc hl			;fa14	23 	#
	ld (hl),000h		;fa15	36 00 	6 .
	ret			;fa17	c9 	.
sub_fa18h:
	;Entry: A = amount of parameters
	;First check for 3 parameters

	;4 or more is too many
	cp 004h		;fa18	fe 04 	. .
	jp nc,MSG_TOO_MANY_PARAMETERS		;fa1a	d2 9f fb 	. . .

	;Less than 3 is too few.
	cp 003h		;fa1d	fe 03 	. .
	jp c,MSG_WRONG_PARAMETER_AMT		;fa1f	da 97 fb 	. . .


	ld l,(ix+002h)		;fa22	dd 6e 02 	. n .
	ld h,(ix+003h)		;fa25	dd 66 03 	. f .
	ld c,(ix+000h)		;fa28	dd 4e 00 	. N .
	ld b,(ix+001h)		;fa2b	dd 46 01 	. F .
	res 7,h		;fa2e	cb bc 	. .
	set 6,h		;fa30	cb f4 	. .
	xor a			;fa32	af 	.
	ld a,(ix+004h)		;fa33	dd 7e 04 	. ~ .
	ret			;fa36	c9 	.
sub_fa37h:
	ld a,(ix+000h)		;fa37	dd 7e 00 	. ~ .
	inc ix		;fa3a	dd 23 	. #
	inc ix		;fa3c	dd 23 	. #
sub_fa3eh:
	ld e,a			;fa3e	5f 	_
	ld d,000h		;fa3f	16 00 	. .
	push hl			;fa41	e5 	.
	push bc			;fa42	c5 	.
	ld hl,lfae9h		;fa43	21 e9 fa 	! . .
	add hl,de			;fa46	19 	.
	ld c,(hl)			;fa47	4e 	N
	ld b,07fh		;fa48	06 7f 	. 
	ld (iy+002h),c		;fa4a	fd 71 02 	. q .
	out (c),c		;fa4d	ed 49 	. I
	pop bc			;fa4f	c1 	.
	pop hl			;fa50	e1 	.
	ret			;fa51	c9 	.
lfa52h:
	push bc			;fa52	c5 	.
	push de			;fa53	d5 	.
	push hl			;fa54	e5 	.
	call sub_faa0h		;fa55	cd a0 fa 	. . .
	jr nz,lfa98h		;fa58	20 3e 	  >
	ex de,hl			;fa5a	eb 	.
	pop hl			;fa5b	e1 	.
	push hl			;fa5c	e5 	.
	jr lfa84h		;fa5d	18 25 	. %
lfa5fh:
	push bc			;fa5f	c5 	.
	push de			;fa60	d5 	.
	push hl			;fa61	e5 	.
	call sub_d878h		;fa62	cd 78 d8 	. x .
	call sub_fa7ah		;fa65	cd 7a fa 	. z .
	pop de			;fa68	d1 	.
	push de			;fa69	d5 	.
	ld bc,00200h		;fa6a	01 00 02 	. . .
	ldir		;fa6d	ed b0 	. .
	pop hl			;fa6f	e1 	.
	pop de			;fa70	d1 	.
	pop bc			;fa71	c1 	.
	ret			;fa72	c9 	.
lfa73h:
	ld a,h			;fa73	7c 	|
	and 0c0h		;fa74	e6 c0 	. .
	cp 040h		;fa76	fe 40 	. @
	jr z,lfa5fh		;fa78	28 e5 	( .
sub_fa7ah:
	push bc			;fa7a	c5 	.
	push de			;fa7b	d5 	.
	push hl			;fa7c	e5 	.
	call sub_faa0h		;fa7d	cd a0 fa 	. . .
	jr nz,lfa98h		;fa80	20 16 	  .
	pop de			;fa82	d1 	.
	push de			;fa83	d5 	.
lfa84h:
	di			;fa84	f3 	.
	out (c),c		;fa85	ed 49 	. I
	ld bc,00200h		;fa87	01 00 02 	. . .
	ldir		;fa8a	ed b0 	. .
	ld bc,07fc0h		;fa8c	01 c0 7f 	. . 
	out (c),c		;fa8f	ed 49 	. I
	ei			;fa91	fb 	.
	pop hl			;fa92	e1 	.
	pop de			;fa93	d1 	.
	pop bc			;fa94	c1 	.
	xor a			;fa95	af 	.
	scf			;fa96	37 	7
	ret			;fa97	c9 	.
lfa98h:
	pop hl			;fa98	e1 	.
	pop de			;fa99	d1 	.
	pop bc			;fa9a	c1 	.
	ld a,0d0h		;fa9b	3e d0 	> .
	cp 002h		;fa9d	fe 02 	. .
	ret			;fa9f	c9 	.
sub_faa0h:
	ld h,000h		;faa0	26 00 	& .
	ld l,d			;faa2	6a 	j
	add hl,hl			;faa3	29 	)
	push hl			;faa4	e5 	.
	add hl,hl			;faa5	29 	)
	add hl,hl			;faa6	29 	)
	pop de			;faa7	d1 	.
	add hl,de			;faa8	19 	.
	ld a,c			;faa9	79 	y
	and 00fh		;faaa	e6 0f 	. .
	dec a			;faac	3d 	=
	ld e,a			;faad	5f 	_
	add hl,de			;faae	19 	.
	ld a,l			;faaf	7d 	}
	sra a		;fab0	cb 2f 	. /
	sra a		;fab2	cb 2f 	. /
	sra a		;fab4	cb 2f 	. /
	sra a		;fab6	cb 2f 	. /
	sra a		;fab8	cb 2f 	. /
	and 007h		;faba	e6 07 	. .
	ld e,a			;fabc	5f 	_
	ld a,h			;fabd	7c 	|
	sla a		;fabe	cb 27 	. '
	sla a		;fac0	cb 27 	. '
	sla a		;fac2	cb 27 	. '
	and 038h		;fac4	e6 38 	. 8
	or e			;fac6	b3 	.
	ld e,a			;fac7	5f 	_
	ld h,l			;fac8	65 	e
	ld l,000h		;fac9	2e 00 	. .
	add hl,hl			;facb	29 	)
	ld b,07fh		;facc	06 7f 	. 
	push hl			;face	e5 	.
	ld hl,lfaeah		;facf	21 ea fa 	! . .
	add hl,de			;fad2	19 	.
	ld c,(hl)			;fad3	4e 	N
	pop hl			;fad4	e1 	.
	set 6,h		;fad5	cb f4 	. .
	res 7,h		;fad7	cb bc 	. .
	xor a			;fad9	af 	.
	ret			;fada	c9 	.
	ld a,h			;fadb	7c 	|
	and 03fh		;fadc	e6 3f 	. ?
	ld h,a			;fade	67 	g
	inc e			;fadf	1c 	.
	ld a,e			;fae0	7b 	{
	cp (iy+00bh)		;fae1	fd be 0b 	. . .
	jp nc,MSG_CORRUPTED_DISC		;fae4	d2 1a fc 	. . .
	xor a			;fae7	af 	.
	ret			;fae8	c9 	.
lfae9h:
	db 0c0h
lfaeah:
	;CPC Standard Disk (178k) disk format definition
	db 0c4h,0c5h,0c6h,0c7h,0cch,0cdh,0ceh,0cfh
	db 0d4h,0d5h,0d6h,0d7h,0dch,0ddh,0deh,0dfh
	db 0e4h,0e5h,0e6h,0e7h,0ech,0edh,0eeh,0efh
	db 0f4h,0f5h,0f6h,0f7h,0fch,0fdh,0feh,0ffh
	db 0c0h
;
; Z80dasm assumed that the above was this code. It was not.
; lfae9h:
; 	ret nz			;fae9	c0 	.
; lfaeah:
; 	call nz,sub_c6c5h		;faea	c4 c5 c6 	. . .
; 	rst 0			;faed	c7 	.
; 	call z,0cecdh		;faee	cc cd ce 	. . .
; 	rst 8			;faf1	cf 	.
; 	call nc,sub_d6d5h		;faf2	d4 d5 d6 	. . .
; 	rst 10h			;faf5	d7 	.
; 	call c,sub_deddh		;faf6	dc dd de 	. . .
; 	rst 18h			;faf9	df 	.
; 	call po,sub_e6e5h		;fafa	e4 e5 e6 	. . .
; 	rst 20h			;fafd	e7 	.
; 	call pe,sub_eeedh		;fafe	ec ed ee 	. . .
; 	rst 28h			;fb01	ef 	.
; 	call p,sub_f6f5h		;fb02	f4 f5 f6 	. . .
; 	rst 30h			;fb05	f7 	.
; 	call m,lfefdh		;fb06	fc fd fe 	. . .
; 	rst 38h			;fb09	ff 	.
; 	ret nz			;fb0a	c0 	.
sub_fb0bh:
	ld a,001h		;fb0b	3e 01 	> .
	ld (iy+00bh),a		;fb0d	fd 77 0b 	. w .
	ld ix,04000h		;fb10	dd 21 00 40 	. ! . @
	ld hl,lfaeah+1		;fb14	21 eb fa 	! . .
	ld bc,07fc0h		;fb17	01 c0 7f 	. . 
	out (c),c		;fb1a	ed 49 	. I
	ld e,(ix+000h)		;fb1c	dd 5e 00 	. ^ .
	ld c,0c4h		;fb1f	0e c4 	. .
	out (c),c		;fb21	ed 49 	. I
	ld d,(ix+000h)		;fb23	dd 56 00 	. V .
lfb26h:
	ld c,(hl)			;fb26	4e 	N
	inc hl			;fb27	23 	#
	out (c),c		;fb28	ed 49 	. I
	inc (ix+000h)		;fb2a	dd 34 00 	. 4 .
	ld a,0c0h		;fb2d	3e c0 	> .
	out (c),a		;fb2f	ed 79 	. y
	ld a,(ix+000h)		;fb31	dd 7e 00 	. ~ .
	cp e			;fb34	bb 	.
	jr nz,lfb51h		;fb35	20 1a 	  .
	ld a,0c4h		;fb37	3e c4 	> .
	out (c),a		;fb39	ed 79 	. y
	ld a,(ix+000h)		;fb3b	dd 7e 00 	. ~ .
	cp d			;fb3e	ba 	.
	jr nz,lfb51h		;fb3f	20 10 	  .
	out (c),c		;fb41	ed 49 	. I
	dec (ix+000h)		;fb43	dd 35 00 	. 5 .
	ld a,(iy+00bh)		;fb46	fd 7e 0b 	. ~ .
	inc a			;fb49	3c 	<
	ld (iy+00bh),a		;fb4a	fd 77 0b 	. w .
	cp 020h		;fb4d	fe 20 	.
	jr nz,lfb26h		;fb4f	20 d5 	  .
lfb51h:
	ld c,0c4h		;fb51	0e c4 	. .
	out (c),c		;fb53	ed 49 	. I
	ld (ix+000h),d		;fb55	dd 72 00 	. r .
	ld c,0c0h		;fb58	0e c0 	. .
	out (c),c		;fb5a	ed 49 	. I
	ld (ix+000h),e		;fb5c	dd 73 00 	. s .
	ld a,(iy+00bh)		;fb5f	fd 7e 0b 	. ~ .
	cp 001h		;fb62	fe 01 	. .
	ret nz			;fb64	c0 	.
	xor a			;fb65	af 	.
	ld (iy+00bh),a		;fb66	fd 77 0b 	. w .
	ret			;fb69	c9 	.
ERROR_HANDLER:
	ld b,a			;fb6a	47 	G
	ld (0be6dh),a		;fb6b	32 6d be 	2 m .
	ld hl,RODOS_MSGS_start		;fb6e	21 c0 fc 	! . .
lfb71h:
	ld a,(hl)			;fb71	7e 	~
	inc hl			;fb72	23 	#
	cp 05ch		;fb73	fe 5c 	. \
	jr nz,lfb71h		;fb75	20 fa 	  .
	djnz lfb71h		;fb77	10 f8 	. .
	res 2,(iy+011h)		;fb79	fd cb 11 96 	. . . .
	call DISPLAY_MSG		;fb7d	cd 6a d9 	. j .
	call MAKE_A_BEEP		;fb80	cd 88 fb 	. . .
	ld a,092h		;fb83	3e 92 	> .
	cp 08ch		;fb85	fe 8c 	. .
	ret			;fb87	c9 	.
MAKE_A_BEEP:
	ld a,007h		;fb88	3e 07 	> .
	jp TXT_OUTPUT		;fb8a	c3 5a bb 	. Z .
lfb8dh:
	ret nc			;fb8d	d0 	.
MSG_BAD_FILE_NAME:
	ld a,002h		;fb8e	3e 02 	> .
	jp ERROR_HANDLER		;fb90	c3 6a fb 	. j .
MSG_BAD_CHAR:
	ld a,005h		;fb93	3e 05 	> .
	jr ERROR_HANDLER		;fb95	18 d3 	. .
MSG_WRONG_PARAMETER_AMT:
	ld a,003h		;fb97	3e 03 	> .
	jr ERROR_HANDLER		;fb99	18 cf 	. .
MSG_BAD_DIR:
	ld a,004h		;fb9b	3e 04 	> .
	jr ERROR_HANDLER		;fb9d	18 cb 	. .
MSG_TOO_MANY_PARAMETERS:
	ld a,001h		;fb9f	3e 01 	> .
	jr ERROR_HANDLER		;fba1	18 c7 	. .
MSG_UNKNOWN_CMD:
	ld a,006h		;fba3	3e 06 	> .
	jr ERROR_HANDLER		;fba5	18 c3 	. .
MSG_ACCESS_DENIED:
	ld a,007h		;fba7	3e 07 	> .
	jr ERROR_HANDLER		;fba9	18 bf 	. .
MSG_NO_MATCH:
	ld a,009h		;fbab	3e 09 	> .
	jr ERROR_H_RELAY_0		;fbad	18 6d 	. m
MSG_DISC_FULL:
	call sub_df23h		;fbaf	cd 23 df 	. # .
	ld a,00ah		;fbb2	3e 0a 	> .
	jr ERROR_H_RELAY_0		;fbb4	18 66 	. f
MSG_CANT_FIND_AMSDOS:
	ld a,00bh		;fbb6	3e 0b 	> .
	jr ERROR_H_RELAY_0		;fbb8	18 62 	. b
MSG_CPM_ROM_MISSING:
	ld a,00ch		;fbba	3e 0c 	> .
	jr ERROR_HANDLER		;fbbc	18 ac 	. .
MSG_DIR_ALREADY_EXISTS:
	ld a,00dh		;fbbe	3e 0d 	> .
	jr ERROR_HANDLER		;fbc0	18 a8 	. .
MSG_BAD_DRIVE:
	ld a,00eh		;fbc2	3e 0e 	> .
	jr ERROR_H_RELAY_0		;fbc4	18 56 	. V
MSG_UNKNOWN_FILE_SYSTEM:
	ld a,00fh		;fbc6	3e 0f 	> .
	jr ERROR_H_RELAY_0		;fbc8	18 52 	. R
MSG_INPUT_FILE_NOT_OPEN:
	ld a,010h		;fbca	3e 10 	> .
	jr ERROR_HANDLER		;fbcc	18 9c 	. .
MSG_OUTPUT_FILE_ALEADY_OPEN:
	ld a,011h		;fbce	3e 11 	> .
	jr ERROR_HANDLER		;fbd0	18 98 	. .
MSG_DIR_NOT_EMPTY:
	ld a,018h		;fbd2	3e 18 	> .
	jr ERROR_HANDLER		;fbd4	18 94 	. .
MSG_CANT_LINK_TO_LINKED_FILE:
	ld a,019h		;fbd6	3e 19 	> .
	jr ERROR_HANDLER		;fbd8	18 90 	. .
MSG_BAD_ALIAS_SPECIFIED:
	ld a,01ah		;fbda	3e 1a 	> .
	jr ERROR_HANDLER		;fbdc	18 8c 	. .
MSG_TOO_MANY_ALIASES:
	ld a,01bh		;fbde	3e 1b 	> .
	jr ERROR_HANDLER		;fbe0	18 88 	. .
MSG_REBORN:
	call sub_fc2ah		;fbe2	cd 2a fc 	. * .
	ld a,012h		;fbe5	3e 12 	> .
	jr ERROR_H_RELAY_0		;fbe7	18 33 	. 3
sub_fbe9h:
	push ix		;fbe9	dd e5 	. .
	push hl			;fbeb	e5 	.
	pop ix		;fbec	dd e1 	. .
	call sub_db4ch		;fbee	cd 4c db 	. L .
	call MSG_ACCESS_DENIED		;fbf1	cd a7 fb 	. . .
	pop ix		;fbf4	dd e1 	. .
	ret			;fbf6	c9 	.
lfbf7h:
	push ix		;fbf7	dd e5 	. .
	pop hl			;fbf9	e1 	.
	ld b,010h		;fbfa	06 10 	. .
lfbfch:
	ld a,(hl)			;fbfc	7e 	~
	cp 03fh		;fbfd	fe 3f 	. ?
	jr z,MSG_NO_MATCH		;fbff	28 aa 	( .
	inc hl			;fc01	23 	#
	djnz lfbfch		;fc02	10 f8 	. .
	call sub_fc2ah		;fc04	cd 2a fc 	. * .
	ld a,008h		;fc07	3e 08 	> .
ERROR_H_RELAY_1:
	call ERROR_HANDLER		;fc09	cd 6a fb 	. j .
	jp lda14h		;fc0c	c3 14 da 	. . .
MSG_ALREADY_EXISTS:
	call sub_fc2ah		;fc0f	cd 2a fc 	. * .
	ld a,013h		;fc12	3e 13 	> .
	jr ERROR_H_RELAY_0		;fc14	18 06 	. .
MSG_BAD_FORMAT:
	ld a,014h		;fc16	3e 14 	> .
	jr ERROR_H_RELAY_0		;fc18	18 02 	. .
MSG_CORRUPTED_DISC:
	ld a,015h		;fc1a	3e 15 	> .
ERROR_H_RELAY_0:
	call ERROR_HANDLER		;fc1c	cd 6a fb 	. j .
	jp lda0ah		;fc1f	c3 0a da 	. . .
MSG_DISC_NOT_FORMATTED:
	ld a,016h		;fc22	3e 16 	> .
	jr ERROR_H_RELAY_0		;fc24	18 f6 	. .
MSG_BAD_FILE:
	ld a,017h		;fc26	3e 17 	> .
	jr ERROR_H_RELAY_1		;fc28	18 df 	. .
sub_fc2ah:
	call PRINT_CR_LF		;fc2a	cd 7d d9 	. } .
	jp sub_db4ch		;fc2d	c3 4c db 	. L .
sub_fc30h:
	call sub_e32ch		;fc30	cd 2c e3 	. , .
	ld hl,lff52h		;fc33	21 52 ff 	! R .
	call DISPLAY_MSG		;fc36	cd 6a d9 	. j .
	call sub_fc4bh		;fc39	cd 4b fc 	. K .
	push af			;fc3c	f5 	.
	call PRINT_CR_LF		;fc3d	cd 7d d9 	. } .
	pop af			;fc40	f1 	.
	cp 061h		;fc41	fe 61 	. a
	call nc,FUNC_SUBTRACT_32		;fc43	d4 9d d9 	. . .
	cp 059h		;fc46	fe 59 	. Y
	ret z			;fc48	c8 	.
	pop hl			;fc49	e1 	.
	ret			;fc4a	c9 	.
sub_fc4bh:
	call KM_WAIT_CHAR		;fc4b	cd 06 bb 	. . .
	jp TXT_OUTPUT		;fc4e	c3 5a bb 	. Z .
lfc51h:
	ld a,01ch		;fc51	3e 1c 	> .
	jr lfc67h		;fc53	18 12 	. .
lfc55h:
	ld a,01dh		;fc55	3e 1d 	> .
	jr lfc67h		;fc57	18 0e 	. .
sub_fc59h:
	ld a,01eh		;fc59	3e 1e 	> .
	jr lfc67h		;fc5b	18 0a 	. .
sub_fc5dh:
	ld l,01fh		;fc5d	2e 1f 	. .
	jr ERROR_H_RELAY_PROCESS		;fc5f	18 0e 	. .
sub_fc61h:
	ld a,020h		;fc61	3e 20 	>
	jr lfc67h		;fc63	18 02 	. .
sub_fc65h:
	ld a,021h		;fc65	3e 21 	> !
lfc67h:
	ld l,a			;fc67	6f 	o
	ld a,(DISK_ERROR_MESSAGE_FLAG)		;fc68	3a 78 be 	: x .
	and a			;fc6b	a7 	.
	jp nz,lda18h		;fc6c	c2 18 da 	. . .
ERROR_H_RELAY_PROCESS:
	bit 0,(iy+00dh)		;fc6f	fd cb 0d 46 	. . . F
	jr nz,lfcb7h		;fc73	20 42 	  B
	push hl			;fc75	e5 	.
	call PRINT_CR_LF		;fc76	cd 7d d9 	. } .
	call sub_e32ch		;fc79	cd 2c e3 	. , .
	pop hl			;fc7c	e1 	.
	ld a,l			;fc7d	7d 	}
	call ERROR_HANDLER		;fc7e	cd 6a fb 	. j .
	ld hl,MSG_RETRY_IGNORE_CANCEL		;fc81	21 7b ff 	! { .
	call DISPLAY_MSG		;fc84	cd 6a d9 	. j .
lfc87h:
	call KM_WAIT_CHAR		;fc87	cd 06 bb 	. . .
	push af			;fc8a	f5 	.
	ld a,(iy+WS_DRIVE_NUMBER)		;fc8b	fd 7e 04 	. ~ .
	call sub_f143h		;fc8e	cd 43 f1 	. C .
	pop af			;fc91	f1 	.
	cp 05bh		;fc92	fe 5b 	. [
	call nc,FUNC_SUBTRACT_32		;fc94	d4 9d d9 	. . .
	cp 052h		;fc97	fe 52 	. R
	jr z,lfca5h		;fc99	28 0a 	( .
	cp 049h		;fc9b	fe 49 	. I
	jr z,lfcaeh		;fc9d	28 0f 	( .
	cp 043h		;fc9f	fe 43 	. C
	jr z,lfcb7h		;fca1	28 14 	( .
	jr lfc87h		;fca3	18 e2 	. .
lfca5h:
	call TXT_OUTPUT		;fca5	cd 5a bb 	. Z .
	call PRINT_CR_LF		;fca8	cd 7d d9 	. } .
	jp lda14h		;fcab	c3 14 da 	. . .
lfcaeh:
	call TXT_OUTPUT		;fcae	cd 5a bb 	. Z .
	call PRINT_CR_LF		;fcb1	cd 7d d9 	. } .
	jp lda18h		;fcb4	c3 18 da 	. . .
lfcb7h:
	call TXT_OUTPUT		;fcb7	cd 5a bb 	. Z .
	call PRINT_CR_LF		;fcba	cd 7d d9 	. } .
	jp lda0fh		;fcbd	c3 0f da 	. . .

; BLOCK 'RODOS_MSGS' (start 0xfcc0 end 0xffc7)
RODOS_MSGS_start:
	defb 05ch ; Starts with a slash for some reason (probably because this is error 0)
	defb 'Too many parameters',05ch ;Error 1
	defb 'Bad file name',05ch ;Error 2
	defb 'Wrong number of parameters',05ch ;Error 3
	defb 'Bad dir',05ch ;Error 4
	defb 'Bad character !',05ch ;Error 5
	defb 'Unknown command',05ch ;Error 6
	defb 'Access denied',05ch ;Error 7
	defb ' not found',05ch ;Error 8
	defb 'No match.',05ch ;Error 9
	defb 'Disc full !',05ch ;Error 10
	defb 'AMSDOS ?',05ch ;Error 11
	defb 'Warning *** CPM ROM missing ***',05ch ;Error 12
	defb 'Dir already exists !',05ch ;Error 13
	defb 'Bad drive',05ch ;Error 14
	defb 'Unknown file-system !',05ch ;Error 15
	defb 'Input file not open',05ch ;Error 16
	defb 'Output file already open',05ch ;Error 17
 	defb ' reborn !',05ch ;Error 18
	defb ' already exists !',05ch ;Error 19
	defb 'Bad format specified',05ch ;Error 20
	defb 'Corrupted disc error',05ch ;Error 21
	defb 'Disc not formatted !',05ch ;Error 22
	defb 'Bad file',05ch ;Error 23
	defb 'Directory not empty !',05ch ;Error 24
	defb 'Cant link to a linked file !',05ch ;Error 25
	defb 'Bad alias defined',05ch ;Error 26
	defb 'To many aliases !',05ch ;Error 27
	defb 'Disc read error',05ch ;Error 28
	defb 'Disc write error',05ch ;Error 29
	defb 'Disc tracking error',05ch ;Error 30
	defb 'Disc Missing',05ch ;Error 31
	defb 'Disc fault',05ch ;Error 32
	defb 'Disc write protected',05ch ;Error 33 - Last error message of this table
lfefdh:
	defb 'Escape',05ch
lff04h:
	defb 'Disc changed !',05ch
lff13h:
	defb '{File exists. Erase, Backup, or Quit ?'
	defb 008h		;ff39	08 	.
	defb 000h		;ff3a	00 	.
lff3bh:
	defb 'Drive :',0
lff43h:
	defb '{Bytes free = ',0
lff52h:
	defb '{Disc already formatted, REFORMAT Y/N ?',8,0
MSG_RETRY_IGNORE_CANCEL:
	defb 'Retry, Ignore or Cancel? ',0
VERSION_MSG:
	defb 00fh		;ff95	0f 	.
	defb 002h		;ff96	02 	.
	defb ' RODOS V2.19 '
	defb 0a4h		;ffa4	a4 	.
	defb ' Romantic Robot U.K. Ltd.{{'
	defb 00fh		;ffc0	0f 	.
	defb 001h		;ffc1	01 	.
	defb 000h		;ffc2	00 	.
	defb 000h		;ffc3	00 	.
	defb 000h		;ffc4	00 	.
	defb 000h		;ffc5	00 	.
	defb 000h		;ffc6	00 	.
RODOS_MSGS_end:
; Debug code I added to dump what was happening in buffers
; DUMP_BUFFER:
; 	push hl
; 	push bc
; 	ld hl,POST_BOOT_MSG
; 	;ld hl, 0xbe00 ;XXXX
; 	;ld hl,0x9603
; 	ld b,255
; Bufloop:
; 	ld a,(hl)
; 	push hl
; 	push af
; 	and 0F0h
; 	rrca
; 	rrca
; 	rrca
; 	rrca
; 	call PrintNibble
; 	pop af
; 	and 0Fh
; 	call PrintNibble
; 	ld a,' '
; 	call TXT_OUTPUT
; 	pop hl
; 	inc hl
; 	djnz Bufloop
; 	call KM_WAIT_KEY
; 	pop bc
; 	pop hl
;
; ret
;
; PrintNibble:
;     add a, '0' ; Convert to ASCII
;     cp '9' + 1 ; Check if the result is greater than '9'
;     jr c, PrintCharacter ; Jump to PrintCharacter if less than or equal to '9'
;     add a, 'A' - '9' - 1 ; Adjust for characters 'A' to 'F'
;
; PrintCharacter:
;     call TXT_OUTPUT
; 		call KM_WAIT_KEY
;     ret ; Return from subroutine

; CALL_TESTER:
; 	call lc234h
; 	ret

	nop			;ffc7	00 	.
	nop			;ffc8	00 	.
	nop			;ffc9	00 	.
	nop			;ffca	00 	.
	nop			;ffcb	00 	.
	nop			;ffcc	00 	.
	nop			;ffcd	00 	.
	nop			;ffce	00 	.
	nop			;ffcf	00 	.
	nop			;ffd0	00 	.
	nop			;ffd1	00 	.
	nop			;ffd2	00 	.
	nop			;ffd3	00 	.
	nop			;ffd4	00 	.
	nop			;ffd5	00 	.
	nop			;ffd6	00 	.
	nop			;ffd7	00 	.
	nop			;ffd8	00 	.
	nop			;ffd9	00 	.
	nop			;ffda	00 	.
	nop			;ffdb	00 	.
	nop			;ffdc	00 	.
	nop			;ffdd	00 	.
	nop			;ffde	00 	.
	nop			;ffdf	00 	.
	nop			;ffe0	00 	.
	nop			;ffe1	00 	.
	nop			;ffe2	00 	.
	nop			;ffe3	00 	.
	nop			;ffe4	00 	.
	nop			;ffe5	00 	.
	nop			;ffe6	00 	.
	nop			;ffe7	00 	.
	nop			;ffe8	00 	.
	nop			;ffe9	00 	.
	nop			;ffea	00 	.
	nop			;ffeb	00 	.
	nop			;ffec	00 	.
	nop			;ffed	00 	.
lffeeh:
	nop			;ffee	00 	.
	nop			;ffef	00 	.
	nop			;fff0	00 	.
lfff1h:
nop			;fff1	00 	.
nop			;fff2	00 	.
nop			;fff3	00 	.
nop			;fff4	00 	.
lfff5h:
nop			;fff5	00 	.
nop			;fff6	00 	.
nop			;fff7	00 	.
nop			;fff8	00 	.
lfff9h:
nop			;fff9	00 	.
nop			;fffa	00 	.
nop			;fffb	00 	.
nop			;fffc	00 	.
nop			;fffd	00 	.
nop			;fffe	00 	.
nop			;ffff	00 	.
