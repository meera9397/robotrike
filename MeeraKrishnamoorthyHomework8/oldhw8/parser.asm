        NAME  Parser

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   Parser                                   ;
;                                                                            ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the main functions for parsing commands sent to the
; RoboTrike, namely a function to parse each character sent to it using
; a state machine (which leads the function to call other functions
; to help with the parsing).
; 
; Table of Contents:
; 1) ParseBegin
;
; Revision History:
;     11/24/16  Meera Krishnamoorthy    wrote functions
;     11/25/16  Meera Krishnamoorthy    debugged
;

CGROUP  GROUP   CODE
DGROUP  GROUP   DATA


CODE	SEGMENT PUBLIC 'CODE'


ASSUME  CS:CGROUP, DS: DGROUP

; local include file
$INCLUDE(PARSER.INC)

; used functions
EXTRN   ChangeSpeed:NEAR
EXTRN   ChangeRelSpeed:NEAR
EXTRN   ChangeDir:NEAR
EXTRN   ChangeTurrAng:NEAR
EXTRN   ChangeTurrElvAng:NEAR
EXTRN   ChangeLaser:NEAR

; ParseBegin
;
; Description: This function initializes all shared variables used in
;              the parser functions.
;
; Operation: This is done by zeroing the shared variables command, sign,
;            and number. Command will eventually store the current
;            motor function to execute, sign will store if the number
;            following the command is negative, positive, or unsigned,
;            and number will contain the actual number following the command.
;            The state is initialized to INIT, the first state in the state
;            table.
;
; Arguments:
; Return Value:
;
; Local Variables: None.
; Shared Variables: state: shared variable that describes the current state
;                          that the state machine is in
;                   command: part of passed in string that describes
;                            what motor function to eventually call
;                   signvar: set based on presence of negative/ positive sign in
;                         passed in ASCII string
;                   number: (would follow a S, V, D, T, or E). is the number
;                           to set as new/relative speed or angle of the
;                           motors/ turret, depending on the value of command.
;
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling: None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Used:
; Stack Depth:      None.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/24/16

ParseBegin		    PROC    NEAR
                  PUBLIC  ParseBegin

MOV command, 0
MOV number, 0
MOV signvar, 0
MOV state, INIT

RET

ParseBegin		ENDP


; ParseSerialChar
;
; Description:      This function is passed a character (c) which
;                   is presumed to be from serial input. The character
;                   should be processed as a serial command. The character
;                   (c) is passed by value in AL. The function returns
;                   the status of parsing operation in AX. Zero
;                   is returned if there are no parsing errors due to the
;                   passed character and a non zero value is returned if
;                   there is a parsing error due to the passed character.
;
; Operation:        Uses a state machine to translate the character.
;
; Arguments:        AL: character c to be processed as a serial command
; Return Value:     AX: returns the status of the parsing operation
;                   (0 if there are no parsing errors and non-zero
;                   value if there is a parsing error due to the passed
;                   character)
;
; Local Variables:  None.
; Shared Variables: state: shared variable that describes the current state
;                          that the state machine is in
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   If an error occurs, a non zero value is passed in AX.
;
; Algorithms:       State Machine.
; Data Structures:  None.
;
; Registers Used:
; Stack Depth:      1 word.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/24/16

ParseSerialChar		PROC    NEAR
                  PUBLIC  ParseSerialChar

FindToken:				; get next input for state machine
    MOV CL, state
    ; input is in AL
	CALL	GetSerialToken		; and get the token type and value
	MOV	DH, AH			        ; token type is saved in DH
	MOV	CH, AL                  ; token value is saved in CH

ComputeTransition: ; figure out what transition to do
	MOV	AL, NUM_TOKEN_TYPES	; find row in the table
	MUL	CL			        ; AX is start of row for current state
	ADD	AL, DH			    ; get the actual transition
	ADC	AH, 0			    ; propagate low byte carry into high byte

	IMUL	BX, AX, SIZE TRANSITION_ENTRY   ; now convert to table offset

DoActions:				; do the actions (don't affect regs)
	MOV	AL, CH			; get token value back for actions
	CALL CS:StateTable[BX].ACTION1	; do the action associated with state

DoTransition:				;now go to next state
    MOV	CL, CS:StateTable[BX].NEXTSTATE
    MOV state, CL
CheckOverflowError:
    CMP AX, OVERFLOW_ERROR_CONST
    JE EndParseSerialChar

CheckError:
    CMP state, ERROR_STATE ; check if next state is the error state
    JE ReportError ; if it is, need to set AX to non-zero value
    MOV AX, 0 ; if there is no error, AX is cleared
    JMP EndParseSerialChar

ReportError:
    MOV AX, ERROR_CONST ; if there is an error, AX is set to a non-zero value
                        ; in ERROR_CONST

EndParseSerialChar:				;done parsing floating-point, return with value
    RET

ParseSerialChar		ENDP


; SaveCommand
;
; Description: This function takes in DH as an argument. DH contains the
; token type of the passed in character. This function then stores the token
; type in the shared variable "command", to be used later to determine what
; motor function to set.  This function is only called when a valid command
; is to be saved (to set motor speed/angle, turret angle/ elevation angle, and
; the laser status).
;
; Operation: This is done by setting the shared variable "command" to DH,
; which stores the token type of the passed in character.
;
;
; Arguments: DH: the token type of the passed in character
; Return Value: None.
;
; Local Variables: None.
; Shared Variables: command: part of passed in string that describes
;                   what motor function to eventually call
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Used:
; Stack Depth:      None.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/24/16

SaveCommand		    PROC    NEAR
                  PUBLIC  SaveCommand

MOV command, DH
RET

SaveCommand		ENDP



; SetSign
;
; Description: This function takes CH as an argument. CH is the token value
; of the passed in character.  This function is only called if DH (token
; type) is TOKEN_SIGN. Then, this function uses the value of CH (specifically,
; whether CH is a positive or negative sign) to determine how to set the
; shared variable "sign".
;
; Operation: If this function is called with the token value being a + sign,
; we set the "sign" variable to NEG_SIGN. If the function is called
; with the token being a - sign, we set the "sign" variable to POS_SIGN.
;
; Arguments: DH: the token type of the passed in character
;            CH: the token value of the passed in character
; Return Value: None.
;
; Local Variables: None.
; Shared Variables: signvar: set based on presence of negative/ positive sign in
;                         passed in ASCII string
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Used:
; Stack Depth:      None.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/24/16

SetSign		    PROC    NEAR
                  PUBLIC  SetSign
CheckNeg:
  CMP CH, NEG_SIGN_CONST ; checks if token value is "-"
  JNE CheckPos           ; if not, check if token value is "+"
                         ; if it is, will set shared variable sign
                         ; to the constant NEG_SIGN_CONST

SetNeg:
  MOV signvar, NEG_SIGN_CONST ; sets shared variable sign to indicate
                           ; number is negative
  JMP EndSetSign           ; jumps to end of function

CheckPos:
  CMP CH, POS_SIGN_CONST ; checks if token value is "+" (error checking)
  JNE EndSetSign         ; if not, mistakenly entered function

SetPos:
  MOV signvar, POS_SIGN_CONST ; sets shared variable sign to indicate
                           ; number is positive (as opposed to unsigned,
                           ; in which "sign" would equal 0)
EndSetSign:
  RET

SetSign		ENDP


; AddDigit
;
; Description: This function takes CH as an argument. CH is the token value
; of the passed in character.  This function is only called if DH (token
; type) is TOKEN_DIGIT. Then, this function adds CH (a digit) to the
; shared variable "number", after multiplying the old value of "number"
; by 10.
;
; Operation: This is done by multiplying the old number by 10, and adding
; the new digit to it. Then, it sets the shared variable number to that
; calculated value.
; ex calculation:
; if total number is 245, parseSerialChar will send in 2 first.
; 2 will be in CH.
; the initialized number is 0, so
; 10 * number = 0
; number + CH = 2
; now number = 2
; then, 4 will be sent in CH
; 10 * number = 20
; number + CH = 24
; now, number = 24
; then, 5 will be sent in CH
; 10 * number = 240
; number + CH = 245
; now, number = 245! (just like it was supposed to be)
;
;
; Arguments: CH: the token value of the passed in character
; Return Value: None.
;
; Local Variables: None.
; Shared Variables: number: (would follow a S, V, D, T, or E). is the number
;                           to set as new/relative speed or angle of the
;                           motors/ turret, depending on the value of command.
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Used:
; Stack Depth:      None.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/24/16

AddDigit		    PROC    NEAR
                  PUBLIC  AddDigit

Mul10: 
    MOV BX, 10
    MOV AX, number
    MUL BX ; multiply current number by 10 to make room for new digit (which
       ; should be lower in powers of 10 than the rest of the number)

AddNewDigit: 
    PUSH CX
    XCHG CH, CL
    MOV CH, 0
    ADD AX, CX ; add new digit to number
    POP CX
    MOV number, AX ; move AX into number (have calculated new value of number)

EndAddDigit:
    RET
    
AddDigit		ENDP


; ExecuteCommand
;
; Description: This function is called in the last state of the state machine.
; It sets up the registers appropriately and then uses a call table to call
; the appropriate functions to call the motor functions to complete the
; parsing of the serial command.
;
; Operation: This is done by setting AX to the number to change the direction/
; speed of the motor or the angle/ elevation angle of the turret. BX
; is set to the command, and CX is set to the sign of the number.
;
; Arguments: None.
; Return Value: AX: returns whether an overflow error occurred.
;
; Local Variables: None.
; Shared Variables: number: (would follow a S, V, D, T, or E). is the number
;                           to set as new/relative speed or angle of the
;                           motors/ turret, depending on the value of command.
;                   command: part of passed in string that describes
;                            what motor function to eventually call
;                   signvar: set based on presence of negative/ positive sign in
;                         passed in ASCII string
;
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Used:
; Stack Depth:      None.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/24/16

ExecuteCommand		    PROC    NEAR
                  PUBLIC  ExecuteCommand

; set up registers to have num, command, and sign so tha called functions
; can access them

ClearReg:
    MOV BX, 0
    MOV CX, 0

CallAppropriateFunc: 
    MOV AX, number
    MOV BL, command
    MOV CL, signvar
    Call CS:CommandCallTable[BX]

    ; AX will be set if an overflow error occurred
    ; This is dealt with ParseSerialChar
EndExecuteCommand: 
    RET

ExecuteCommand		ENDP



; doNOP
;
; Description: This is a function that does nothing (for placeholder states).
;
; Operation: This function simply returns.
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
; Data Structures: None.
;
;
; Registers Changed: None.
; Stack Depth: None.
;
; Limitations: None.
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/24/16
;

doNOP      PROC        NEAR
        PUBLIC      doNOP

RET

doNOP	ENDP



; _____________________________________________________________________________
; CommandCallTable
;
; Description:      This table associates the "command" shared variable
;                   with functions that execute tha command.
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/24/16

CommandCallTable       LABEL   WORD
        PUBLIC  CommandCallTable

DW        offset(ChangeSpeed)   ; command = TOKEN_SPEED
                                ; call function to deal with that

DW        offset(ChangeRelSpeed) ; command = TOKEN_RELSPEED,
                                 ; call function to deal with that

DW        offset(ChangeDir)     ; command = TOKEN_DIR,
                                ; call function to deal with that

DW        offset(ChangeTurrAng)  ; command = TOKEN_TURRANG
                                 ; call function to deal with that

DW        offset(ChangeTurrElvAng)  ; command = TOKEN_ELVTURRANG,
                                    ; call function to deal with that

DW        offset(ChangeLaser)  ; command = TOKEN_FIRELASER
                               ; call function to deal with that

DW        offset(ChangeLaser)  ; command = TOKEN_OFFLASER,
                               ; call function to deal with that

; StateTable
;
; Description:      This is the state transition table for the state machine.
;                   Each entry consists of the next state and actions for that
;                   transition.  The rows are associated with the current
;                   state and the columns with the input type.
;
; Author:           Glen George
; Last Modified:    Feb. 26, 2003


TRANSITION_ENTRY        STRUC           ;structure used to define table
    NEXTSTATE   DB      ?               ;the next state for the transition
    ACTION1     DW      ?               ;first action for the transition
TRANSITION_ENTRY        ENDS


;define a macro to make table a little more readable
;macro just does an offset of the action routine entries to build the STRUC
%*DEFINE(TRANSITION(nxtst, act1))  (
    TRANSITION_ENTRY< %nxtst, OFFSET(%act1) >
)


StateTable	LABEL	TRANSITION_ENTRY

	;Current State = INIT                                Input Token Type
	%TRANSITION(VALID_COMMAND, SaveCommand)	             ;TOKEN_SPEED
	%TRANSITION(VALID_COMMAND, SaveCommand)		         ;TOKEN_RELSPEED
	%TRANSITION(VALID_COMMAND, SaveCommand)	             ;TOKEN_DIR
	%TRANSITION(VALID_COMMAND, SaveCommand)		         ;TOKEN_TURRANG
	%TRANSITION(VALID_COMMAND, SaveCommand)		         ;TOKEN_ELVTURRANG
	%TRANSITION(LASER, SaveCommand)		                 ;TOKEN_FIRELASER
    %TRANSITION(LASER, SaveCommand)		                 ;TOKEN_OFFLASER
    %TRANSITION(ERROR_STATE, ParseBegin)		         ;TOKEN_SIGN
    %TRANSITION(ERROR_STATE, ParseBegin)		         ;TOKEN_DIGIT
    %TRANSITION(ERROR_STATE, ParseBegin)		         ;TOKEN_CR
    %TRANSITION(ERROR_STATE, ParseBegin)		         ;TOKEN_OTHER

	;Current State = VALID_COMMAND                       Input Token Type
    %TRANSITION(ERROR_STATE, ParseBegin)                 ;TOKEN_SPEED
	%TRANSITION(ERROR_STATE, ParseBegin)		         ;TOKEN_RELSPEED
	%TRANSITION(ERROR_STATE, ParseBegin)	             ;TOKEN_DIR
	%TRANSITION(ERROR_STATE, ParseBegin)		         ;TOKEN_TURRANG
	%TRANSITION(ERROR_STATE, ParseBegin)		         ;TOKEN_ELVTURRANG
	%TRANSITION(ERROR_STATE, ParseBegin)		         ;TOKEN_FIRELASER
    %TRANSITION(ERROR_STATE, ParseBegin)			     ;TOKEN_OFFLASER
    %TRANSITION(SIGN, SetSign)		                     ;TOKEN_SIGN
    %TRANSITION(ADD_DIGIT, AddDigit)		             ;TOKEN_DIGIT
    %TRANSITION(ERROR_STATE, ParseBegin)		         ;TOKEN_CR
    %TRANSITION(ERROR_STATE, ParseBegin)			     ;TOKEN_OTHER

    ;Current State = SIGN                                Input Token Type
    %TRANSITION(ERROR_STATE, ParseBegin)                 ;TOKEN_SPEED
	%TRANSITION(ERROR_STATE, ParseBegin)		         ;TOKEN_RELSPEED
	%TRANSITION(ERROR_STATE, ParseBegin)	             ;TOKEN_DIR
	%TRANSITION(ERROR_STATE, ParseBegin)		         ;TOKEN_TURRANG
	%TRANSITION(ERROR_STATE, ParseBegin)		         ;TOKEN_ELVTURRANG
	%TRANSITION(ERROR_STATE, ParseBegin)		         ;TOKEN_FIRELASER
    %TRANSITION(ERROR_STATE, ParseBegin)			     ;TOKEN_OFFLASER
    %TRANSITION(ERROR_STATE, ParseBegin)		         ;TOKEN_SIGN
    %TRANSITION(ADD_DIGIT, AddDigit)		             ;TOKEN_DIGIT
    %TRANSITION(ERROR_STATE, ParseBegin)		         ;TOKEN_CR
    %TRANSITION(ERROR_STATE, ParseBegin)			     ;TOKEN_OTHER

    ;Current State = ADD_DIGIT                          Input Token Type
    %TRANSITION(ERROR_STATE, ParseBegin)                ;TOKEN_SPEED
	%TRANSITION(ERROR_STATE, ParseBegin)		        ;TOKEN_RELSPEED
	%TRANSITION(ERROR_STATE, ParseBegin)	            ;TOKEN_DIR
	%TRANSITION(ERROR_STATE, ParseBegin)		        ;TOKEN_TURRANG
	%TRANSITION(ERROR_STATE, ParseBegin)		        ;TOKEN_ELVTURRANG
	%TRANSITION(ERROR_STATE, ParseBegin)		        ;TOKEN_FIRELASER
    %TRANSITION(ERROR_STATE, ParseBegin)			    ;TOKEN_OFFLASER
    %TRANSITION(ERROR_STATE, ParseBegin)		        ;TOKEN_SIGN
    %TRANSITION(ADD_DIGIT, AddDigit)		            ;TOKEN_DIGIT
    %TRANSITION(EXECUTE_COMMAND, ExecuteCommand)		;TOKEN_CR
    %TRANSITION(ERROR_STATE, ParseBegin)			    ;TOKEN_OTHER

    ;Current State = EXECUTE_COMMAND                    Input Token Type
    %TRANSITION(INIT, doNOP)                            ;TOKEN_SPEED
	%TRANSITION(INIT, doNOP)		                    ;TOKEN_RELSPEED
	%TRANSITION(INIT, doNOP)	                        ;TOKEN_DIR
	%TRANSITION(INIT, doNOP)		                    ;TOKEN_TURRANG
	%TRANSITION(INIT, doNOP)		                    ;TOKEN_ELVTURRANG
	%TRANSITION(INIT, doNOP)		                    ;TOKEN_FIRELASER
    %TRANSITION(INIT, doNOP)			                ;TOKEN_OFFLASER
    %TRANSITION(ERROR_STATE, ParseBegin)		        ;TOKEN_SIGN
    %TRANSITION(ERROR_STATE, ParseBegin)		        ;TOKEN_DIGIT
    %TRANSITION(ERROR_STATE, ParseBegin)                ;TOKEN_CR
    %TRANSITION(ERROR_STATE, ParseBegin)			    ;TOKEN_OTHER
    
    ;Current State = LASER                              Input Token Type
    %TRANSITION(ERROR_STATE, ParseBegin)                ;TOKEN_SPEED
	%TRANSITION(ERROR_STATE, ParseBegin)		        ;TOKEN_RELSPEED
	%TRANSITION(ERROR_STATE, ParseBegin)	            ;TOKEN_DIR
	%TRANSITION(ERROR_STATE, ParseBegin)		        ;TOKEN_TURRANG
	%TRANSITION(ERROR_STATE, ParseBegin)		        ;TOKEN_ELVTURRANG
	%TRANSITION(ERROR_STATE, ParseBegin)		        ;TOKEN_FIRELASER
    %TRANSITION(ERROR_STATE, ParseBegin)			    ;TOKEN_OFFLASER
    %TRANSITION(ERROR_STATE, ParseBegin)		        ;TOKEN_SIGN
    %TRANSITION(ERROR_STATE, ParseBegin)		        ;TOKEN_DIGIT
    %TRANSITION(EXECUTE_COMMAND, ExecuteCommand)        ;TOKEN_CR
    %TRANSITION(ERROR_STATE, ParseBegin)			    ;TOKEN_OTHER

    ;Current State = ERROR_STATE                         Input Token Type
    %TRANSITION(INIT, doNOP)                            ;TOKEN_SPEED
	%TRANSITION(INIT, doNOP)		                    ;TOKEN_RELSPEED
	%TRANSITION(INIT, doNOP)	                        ;TOKEN_DIR
	%TRANSITION(INIT, doNOP)		                    ;TOKEN_TURRANG
	%TRANSITION(INIT, doNOP)		                    ;TOKEN_ELVTURRANG
	%TRANSITION(INIT, doNOP)		                    ;TOKEN_FIRELASER
    %TRANSITION(INIT, doNOP)			                ;TOKEN_OFFLASER
    %TRANSITION(INIT, doNOP)	                        ;TOKEN_SIGN
    %TRANSITION(INIT, doNOP)		                    ;TOKEN_DIGIT
    %TRANSITION(INIT, doNOP)		                    ;TOKEN_CR
    %TRANSITION(INIT, doNOP)			                ;TOKEN_OTHER


; GetSerialToken
;
; Description:      This procedure returns the token class and token value for
;                   the passed character.  The character is truncated to
;                   7-bits.
;
; Operation:        Looks up the passed character in two tables, one for token
;                   types or classes, the other for token values.
;
; Arguments:        AL - character to look up.
; Return Value:     AL - token value for the character.
;                   AH - token type or class for the character.
;
; Local Variables:  BX - table pointer, points at lookup tables.
; Shared Variables: None.
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       Table lookup.
; Data Structures:  Two tables, one containing token values and the other
;                   containing token types.
;
; Registers Used:   AX, BX.
; Stack Depth:      0 words.
;
; Author:           Glen George
; Last Modified:    Feb. 26, 2003

GetSerialToken	PROC    NEAR


InitGetFPToken: ; setup for lookups
	AND	AL, TOKEN_MASK	; strip unused bits (high bit)
	MOV	AH, AL			    ; and preserve value in AH


TokenTypeLookup: ; get the token type
    MOV   BX, OFFSET(TokenTypeTable)  ; BX points at table
	XLAT	CS:TokenTypeTable	          ; have token type in AL
	XCHG	AH, AL		  	              ; token type in AH, character in AL

TokenValueLookup:	; get the token value
  MOV   BX, OFFSET(TokenValueTable)  ; BX points at table
	XLAT	CS:TokenValueTable	         ; have token value in AL


EndGetFPToken:                     	;done looking up type and value
  RET


GetSerialToken	ENDP




; Token Tables
;
; Description:      This creates the tables of token types and token values.
;                   Each entry corresponds to the token type and the token
;                   value for a character.  Macros are used to actually build
;                   two separate tables - TokenTypeTable for token types and
;                   TokenValueTable for token values.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/24/16

%*DEFINE(TABLE)  (
        %TABENT(TOKEN_OTHER, 0)		;<null>
        %TABENT(TOKEN_OTHER, 1)		;SOH
        %TABENT(TOKEN_OTHER, 2)		;STX
        %TABENT(TOKEN_OTHER, 3)		;ETX
        %TABENT(TOKEN_OTHER, 4)		;EOT
        %TABENT(TOKEN_OTHER, 5)		;ENQ
        %TABENT(TOKEN_OTHER, 6)		;ACK
        %TABENT(TOKEN_OTHER, 7)		;BEL
        %TABENT(TOKEN_OTHER, 8)		;backspace
        %TABENT(TOKEN_OTHER, 9)		;TAB
        %TABENT(TOKEN_OTHER, 10)	;new line
        %TABENT(TOKEN_OTHER, 11)	;vertical tab
        %TABENT(TOKEN_OTHER, 12)	;form feed
        %TABENT(TOKEN_CR, 13)	    ;carriage return
        %TABENT(TOKEN_OTHER, 14)	;SO
        %TABENT(TOKEN_OTHER, 15)	;SI
        %TABENT(TOKEN_OTHER, 16)	;DLE
        %TABENT(TOKEN_OTHER, 17)	;DC1
        %TABENT(TOKEN_OTHER, 18)	;DC2
        %TABENT(TOKEN_OTHER, 19)	;DC3
        %TABENT(TOKEN_OTHER, 20)	;DC4
        %TABENT(TOKEN_OTHER, 21)	;NAK
        %TABENT(TOKEN_OTHER, 22)	;SYN
        %TABENT(TOKEN_OTHER, 23)	;ETB
        %TABENT(TOKEN_OTHER, 24)	;CAN
        %TABENT(TOKEN_OTHER, 25)	;EM
        %TABENT(TOKEN_OTHER, 26)	;SUB
        %TABENT(TOKEN_OTHER, 27)	;escape
        %TABENT(TOKEN_OTHER, 28)	;FS
        %TABENT(TOKEN_OTHER, 29)	;GS
        %TABENT(TOKEN_OTHER, 30)	;AS
        %TABENT(TOKEN_OTHER, 31)	;US
        %TABENT(TOKEN_OTHER, ' ')	;space
        %TABENT(TOKEN_OTHER, '!')	;!
        %TABENT(TOKEN_OTHER, '"')	;"
        %TABENT(TOKEN_OTHER, '#')	;#
        %TABENT(TOKEN_OTHER, '$')	;$
        %TABENT(TOKEN_OTHER, 37)	;percent
        %TABENT(TOKEN_OTHER, '&')	;&
        %TABENT(TOKEN_OTHER, 39)	;'
        %TABENT(TOKEN_OTHER, 40)	;open paren
        %TABENT(TOKEN_OTHER, 41)	;close paren
        %TABENT(TOKEN_OTHER, '*')	;*
        %TABENT(TOKEN_SIGN, +1)		;+  (positive sign)
        %TABENT(TOKEN_OTHER, 44)	;,
        %TABENT(TOKEN_SIGN, -1)		;-  (negative sign)
        %TABENT(TOKEN_OTHER, 0)		;.  (decimal point)
        %TABENT(TOKEN_OTHER, '/')	;/
        %TABENT(TOKEN_DIGIT, 0)		;0  (digit)
        %TABENT(TOKEN_DIGIT, 1)		;1  (digit)
        %TABENT(TOKEN_DIGIT, 2)		;2  (digit)
        %TABENT(TOKEN_DIGIT, 3)		;3  (digit)
        %TABENT(TOKEN_DIGIT, 4)		;4  (digit)
        %TABENT(TOKEN_DIGIT, 5)		;5  (digit)
        %TABENT(TOKEN_DIGIT, 6)		;6  (digit)
        %TABENT(TOKEN_DIGIT, 7)		;7  (digit)
        %TABENT(TOKEN_DIGIT, 8)		;8  (digit)
        %TABENT(TOKEN_DIGIT, 9)		;9  (digit)
        %TABENT(TOKEN_OTHER, ':')	;:
        %TABENT(TOKEN_OTHER, ';')	;;
        %TABENT(TOKEN_OTHER, '<')	;<
        %TABENT(TOKEN_OTHER, '=')	;=
        %TABENT(TOKEN_OTHER, '>')	;>
        %TABENT(TOKEN_OTHER, '?')	;?
        %TABENT(TOKEN_OTHER, '@')	;@
        %TABENT(TOKEN_OTHER, 'A')	;A
        %TABENT(TOKEN_OTHER, 'B')	;B
        %TABENT(TOKEN_OTHER, 'C')	;C
        %TABENT(TOKEN_DIR, 'D')	        ;D (direction change)
        %TABENT(TOKEN_ELVTURRANG, 'E')  ;E (elevate turret angle)
        %TABENT(TOKEN_FIRELASER, 'F')	;F (fire laser)
        %TABENT(TOKEN_OTHER, 'G')	;G
        %TABENT(TOKEN_OTHER, 'H')	;H
        %TABENT(TOKEN_OTHER, 'I')	;I
        %TABENT(TOKEN_OTHER, 'J')	;J
        %TABENT(TOKEN_OTHER, 'K')	;K
        %TABENT(TOKEN_OTHER, 'L')	;L
        %TABENT(TOKEN_OTHER, 'M')	;M
        %TABENT(TOKEN_OTHER, 'N')	;N
        %TABENT(TOKEN_OFFLASER, 'O')	;O (turn off laser)
        %TABENT(TOKEN_OTHER, 'P')	;P
        %TABENT(TOKEN_OTHER, 'Q')	;Q
        %TABENT(TOKEN_OTHER, 'R')	;R
        %TABENT(TOKEN_SPEED, 'S')	    ;S (change speed)
        %TABENT(TOKEN_TURRANG, 'T')	;T
        %TABENT(TOKEN_OTHER, 'U')	;U
        %TABENT(TOKEN_RELSPEED, 'V')	;V (change rel speed)
        %TABENT(TOKEN_OTHER, 'W')	;W
        %TABENT(TOKEN_OTHER, 'X')	;X
        %TABENT(TOKEN_OTHER, 'Y')	;Y
        %TABENT(TOKEN_OTHER, 'Z')	;Z
        %TABENT(TOKEN_OTHER, '[')	;[
        %TABENT(TOKEN_OTHER, '\')	;\
        %TABENT(TOKEN_OTHER, ']')	;]
        %TABENT(TOKEN_OTHER, '^')	;^
        %TABENT(TOKEN_OTHER, '_')	;_
        %TABENT(TOKEN_OTHER, '`')	;`
        %TABENT(TOKEN_OTHER, 'a')	;a
        %TABENT(TOKEN_OTHER, 'b')	;b
        %TABENT(TOKEN_OTHER, 'c')	;c
        %TABENT(TOKEN_DIR, 'd')	        ;d (direction change)
        %TABENT(TOKEN_ELVTURRANG, 'e')	;e (elevate turret angle)
        %TABENT(TOKEN_FIRELASER, 'f')	;f (fire laser)
        %TABENT(TOKEN_OTHER, 'g')	;g
        %TABENT(TOKEN_OTHER, 'h')	;h
        %TABENT(TOKEN_OTHER, 'i')	;i
        %TABENT(TOKEN_OTHER, 'j')	;j
        %TABENT(TOKEN_OTHER, 'k')	;k
        %TABENT(TOKEN_OTHER, 'l')	;l
        %TABENT(TOKEN_OTHER, 'm')	;m
        %TABENT(TOKEN_OTHER, 'n')	;n
        %TABENT(TOKEN_OFFLASER, 'o')	;o (turn off laser)
        %TABENT(TOKEN_OTHER, 'p')	;p
        %TABENT(TOKEN_OTHER, 'q')	;q
        %TABENT(TOKEN_OTHER, 'r')	;r
        %TABENT(TOKEN_SPEED, 's')	    ;s (change speed)
        %TABENT(TOKEN_TURRANG, 't')	;t
        %TABENT(TOKEN_OTHER, 'u')	;u
        %TABENT(TOKEN_RELSPEED, 'v')	;v (change rel speed)
        %TABENT(TOKEN_OTHER, 'w')	;w
        %TABENT(TOKEN_OTHER, 'x')	;x
        %TABENT(TOKEN_OTHER, 'y')	;y
        %TABENT(TOKEN_OTHER, 'z')	;z
        %TABENT(TOKEN_OTHER, '{')	;{
        %TABENT(TOKEN_OTHER, '|')	;|
        %TABENT(TOKEN_OTHER, '}')	;}
        %TABENT(TOKEN_OTHER, '~')	;~
        %TABENT(TOKEN_OTHER, 127)	;rubout
)

; token type table - uses first byte of macro table entry
%*DEFINE(TABENT(tokentype, tokenvalue))  (
        DB      %tokentype
)

TokenTypeTable	LABEL   BYTE
        %TABLE


; token value table - uses second byte of macro table entry
%*DEFINE(TABENT(tokentype, tokenvalue))  (
        DB      %tokenvalue
)

TokenValueTable	LABEL       BYTE
        %TABLE


CODE    ENDS

;the data segment

DATA    SEGMENT PUBLIC  'DATA'

state       DB        ?
command     DB        ?
signvar     DB        ?
number      DW        ?

DATA    ENDS


END
