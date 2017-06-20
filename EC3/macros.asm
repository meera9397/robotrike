NAME    MACROS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   MACROS                                   ;
;                                  EE/CS 51                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Overall Description: This file holds a function that tests the macros
; 					   written in macros.inc. 
; Revision History
;    11/20/16  Meera Krishnamoorthy wrote code


CGROUP  GROUP   CODE
DGROUP  GROUP   DATA


CODE	SEGMENT PUBLIC 'CODE'


ASSUME  CS:CGROUP, DS: DGROUP
$INCLUDE(MACROS.INC)

VAL0 EQU 00000000B

VAL1 EQU 10001111B
VAL2 EQU 11001111B

; TestMacros
;
; Description: This functions tests the macro functions in macros.inc
;
; Operation: This is done by calling each macro function in macros.inc with
; 			 various constants, checking the registers after they 
; 			 are used to see if they have changed them appropriately.  
;
; Arguments: None.
; Return Value: None.
;
; Local Variables: None.
; Shared Variables: None.
; Global Variables: None.
;
; Input: None.
; Output: None.
;
; Error Handling: None.
;
; Algorithms: None.
; Data Structures:
;
; Registers Changed: None.
; Stack Depth: None.
;
; Limitations: None.
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/21/16
;

TestMacros      PROC        NEAR
        PUBLIC      TestMacros

MOV BX, VAL1

TestClear:
	%CLR(BX) ; should clear BX (set to 0)

TestSetBit:
Test1SB:
	MOV BX, VAL0
    %SETBIT(BX, 3) ; should change BX from 0 to 00001000B
Test2SB:
	MOV BX, VAL1
    %SETBIT(BX, 6) ; should change BX from 10001111 to 11001111 (8f to cf)

TestClearBit:
Test1ClB:
	MOV BX, 8
	%CLRBIT(BX, 3) ; should change BX from 8 to 00000000B
Test2ClB:
	MOV BX, VAL2
    %CLRBIT(BX, 6) ; should change BX from 11000111 to 10000111 (cf to 8f)

TestComBit:
Test1CB:
	MOV BX, VAL1
    %COMBIT(BX, 6) ; should change BX from 10001111 to 11001111 (8f to cf)
Test2CB: 
	MOV BX, VAL2
    %COMBIT(BX, 6) ; should change BX from 11000111 to 10000111 (cf to 8f)

TestTestBit:
Test1TB:
	MOV BX, VAL1
    %TESTBIT(BX, 6) ; bit not set, so should set zero flag
Test2TB:
	MOV BX, VAL2
    %TESTBIT(BX, 6) ; bit set, so should reset zero flag

TestXLATW:
TestX1: 
	MOV AX, 0
	MOV BX, offset(MotorForcesX_Table)
	%XLATW ; AX should store 7FFFH
TestX2:
	MOV AX, 2
	MOV BX, offset(MotorForcesX_Table)
	%XLATW ; AX should store C000H
	
; 00FF3CH: I2CON register
TestReadPCB:
    %READPCB(0FF3CH) ; read in INT 2 CTRL register

TestWritePCB:
    %WRITEPCB(0FF3CH, 6) ; Int 2 CTRL register should be 6



TestMacros	ENDP

; MotorForcesX_Table (used to test xlatw)
;
; Description:      These are the components of horizontal
;                   force given to each motor.
;
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/12/16

MotorForcesX_Table       LABEL   WORD
        PUBLIC  MotorForcesX_Table

DW          7FFFH        ; F_1_x
DW          0C000H       ; F_2_x
DW          0C000H       ; F_3_x

CODE    ENDS


;the data segment

DATA    SEGMENT PUBLIC  'DATA'


DATA    ENDS


END
