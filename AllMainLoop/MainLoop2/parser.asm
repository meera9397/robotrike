        NAME  Parser

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   Parser                                   ;
;                   functions to parse serial char for motor/                ;
;                             turret/ laser input                            ;
;                                 EE/CS 51                                   ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the main functions for parsing commands sent to the
; RoboTrike, namely a function to parse each character sent to it using
; a state machine (which leads the function to call other functions
; to help with the parsing).
;
; Table of Contents:
; 1) InitParser: initializes shared variables used in parser functions
; 2) ParseSerialChar: parses sent in character based on state
;                     machine, and sends result of parsing in AX
; 3) SetSign: stores value of sign character passed into parse serial char
; 4) SaveCommand: saves token type/ value of character when it's called
; 4) AddDigit: calculates number sent into ParseSerialChar (one character at a 
; 			   time, by storing sent in digits one at a time)
; 5) ExecuteCommand: executing command stored in shared variable command
;                    using the sign and number passed into ParseSerialChar
; 6) doNOP: plaseholder function for state machine for states in which no
;           action should occur
; 7) ChangeSpeed: called in ExecuteCommand if command is S. changes absolute
;                 speed of RoboTrike
; 8) ChangeRelSpeed: called in ExecuteCommand if command is V. changes relative
;                    speed of RoboTrike
; 9) ChangeDir: called in ExecuteCommand if command is D. changes relative
;               direction of RoboTrike
; 10) ChangeTurrAng: called in ExecuteCommand if command is T. changes absolute/
;                    relative turret angle of RoboTrike
; 11) ChangeTurrElvAng: called in ExecuteCommand if command is E. changes
;                       turret elevation of RoboTrike.
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
EXTRN   SetMotorSpeed:NEAR
EXTRN   GetMotorSpeed:NEAR
EXTRN   GetMotorDirection:NEAR
EXTRN   SetLaser:NEAR

;EXTRN   GetTurretAngle:NEAR
GetTurretAngle		    PROC    NEAR
                  PUBLIC  GetTurretAngle
RET

GetTurretAngle		ENDP

;EXTRN   SetRelTurretAngle:NEAR
SetRelTurretAngle		    PROC    NEAR
                  PUBLIC  SetRelTurretAngle
RET

SetRelTurretAngle		ENDP

;EXTRN   SetTurretAngle:NEAR
SetTurretAngle		    PROC    NEAR
                  PUBLIC  SetTurretAngle
RET

SetTurretAngle		ENDP

;EXTRN   SetTurretElevation:NEAR
SetTurretElevation		    PROC    NEAR
                  PUBLIC  SetTurretElevation
RET

SetTurretElevation		ENDP

; InitParser
;
; Description: This function initializes all shared variables used in
;              the parser functions.
;
; Operation: This is done by zeroing the shared variables command, sign,
;            and number. Command will eventually store the current
;            motor/turret/laser function to execute, sign will store a value
; 			 to represent if the number following the command is negative, 
;            positive, or unsigned, number will contain the actual number 
; 		     following the command,and the overflowflag will be set if an 
;            overflow error occurred, and cleared if no overflow error occurred.
;            The state is initialized to INIT, the first state in the state
;            table.
;
; Arguments: None.
; Return Value: None.
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
;                   overflowflag: represents status of overflow error.
;                           set if an overflow error occurred,
;                           cleared if no overflow error occurred.
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

InitParser		    PROC    NEAR
                  PUBLIC  InitParser
; initializes shared variables
; all should be 0 at the beginning (set to various values as the
; state machine progresses, but should be nothing initially)
MOV command, 0
MOV number, 0
MOV signvar, 0
MOV overflowflag, 0

MOV state, INIT ; starts off state variable in initial state, INIT

RET

InitParser		ENDP


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
; Operation:        Uses a state machine to convert each character into
;                   part of a command to change the motor/turret/laser
;                   interface.
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
;                   overflowflag: represents status of overflow error.
;                           set if an overflow error occurred,
;                           cleared if no overflow error occurred.
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

FindToken:				; get input for state machine
    MOV CL, state ; move state into register to use later to figure out the
                  ; transition to do
    ; input is in AL
	  CALL	GetSerialToken		; get the token type and value of passed
                            ; in character (each character corresponds
                            ; to a specific token type/ value that
                            ; determines its function in the state machine)

	  MOV	DH, AH			        ; token type is saved in DH
 	  MOV	CH, AL              ; token value is saved in CH

ComputeTransition: ; figure out what transition to do
	MOV	AL, NUM_TOKEN_TYPES	; find row in the table
	MUL	CL			        ; AX is start of row for current state
	ADD	AL, DH			    ; get the actual transition by adding the token type
                      ; to the start of the row (the columns in the row
                      ; are ordered by token type)
	ADC	AH, 0			      ; propagate low byte carry into high byte

	IMUL	BX, AX, SIZE TRANSITION_ENTRY   ; now convert to table offset and
                                        ; store result in BX (so
                                        ; can index state table)

DoActions:				; do the action that corresponds with the state
	MOV	AL, CH			; store token value in AL to use in actions
  PUSH BX         ; save index to next state in case it is changed in
                  ; the action
	CALL CS:StateTable[BX].ACTION1	; do the action associated with state

DoTransition:	;now go to next state
    POP BX ; get index to next state
    MOV	CL, CS:StateTable[BX].NEXTSTATE ; put value of next state in state
                                        ; (in order to do this, have
                                        ; to store value in CL intermittently)
    MOV state, CL

CheckOverflowError:
    CMP overflowflag, OVERFLOW_ERROR_CONST ; overflow flag is set if an
                                           ; action performed resulted
                                           ; in an overflow error
    JE ReportError ; if so, it is reported as an error

CheckError:
    CMP state, ERROR_STATE ; check if next state is the error state
    JE ReportError ; if it is, need to set AX to non-zero value
    MOV AX, 0 ; if there is no error, AX is cleared, and can move to end of
              ; function
    JMP EndParseSerialChar

ReportError:
    MOV state, ERROR_STATE ; this is done so that overflow errors cause
                           ; the state machine to enter the error
                           ; state (where variables are reinitialized)
    MOV AX, ERROR_CONST ; if there is an error, AX is set to a random non-zero
                        ; value (ERROR_CONST)

EndParseSerialChar:				;done parsing character, return with status of
                          ; parsing in AX
    RET

ParseSerialChar		ENDP


; SaveCommand
;
; Description: This function takes in DH as an argument. DH contains the
; token type of the passed in character. This function then stores the token
; type in the shared variable "command", to be used later to determine what
; motor/turret/laser function to set.  This function is only called when a
; valid command is to be saved (to set motor speed/angle, turret angle/
; elevation angle, and the laser status).
;
; Operation: This is done by setting the shared variable "command" to DH,
; which stores the token type of the passed in character.
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

MOV command, DH ; saves command (originally in DH) in command shared variable
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
; we set the "sign" variable to POS_SIGN. If the function is called
; with the token being a - sign, we set the "signvar" variable to NEG_SIGN. If
; no sign, we leave the sign shared variable signvar to be its initialized value.
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
; calculated value. When multiplying by 10, there is a chance that
; an overflow error could occur. To check if an overflow error could
; occur, the value of number before it's multiplied by 10 is compared
; to DIG_OVERFLOW, a constant that represents the smallest number
; that could be multiplied by 10 and not overflow. If number is greater than
; this number, it will cause an overflow error, and so the overflow
; flag is set.
;
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
;                   overflowflag: represents status of overflow error.
;                           set if an overflow error occurred,
;                           cleared if no overflow error occurred.
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

MOV overflowflag, 0 ; ensure overflowflag is 0 so that if there is no
                    ; overflow in these digit calculations, the overflow
                    ; flag is not set
   ; overview of calculation:
   ; multiply number by base, then add new digit to number
MulBase: ; multiplying number by base
    ; set up registers for calculations
    MOV BX, BASE
    MOV AX, number
    CMP AX, DIG_OVERFLOW ; check if multiplying AX by base will cause
                         ; an overflow (this DIG_OVERFLOW constant
                         ; is the max value that AX can be / 10)
    JG DigOverflow ; is so, call a DigOverflow
    MUL BX ; id nor, multiply current number by base to make room for new digit (which
            ; should be lower in powers of base than the rest of the number)

AddNewDigit: ; new digit is stored in CH (in token value, because the
             ; value of each digit token is their decimal prepresentation)
			 ; this part of the function adds new digit to number

    PUSH CX ; to add this value to the number, save original value of CX (for
            ; use by ParseSerialChar after calling this function)
    XCHG CH, CL ; then, move part of CX that we want to add to number into
                ; CL
    MOV CH, 0  ; and clear out CH
    ADD AX, CX ; now, can cadd new digit to number (because new digit is in CX)
               ; if overflow occurred, this will bed caught in the functions
               ; that set various aspects of the motor/turret/laser
    POP CX ; can get original value of CX register back now that new number
           ; has been calculated
    MOV number, AX ; move new calculated number into shared variable number for
                  ; storage
    JMP EndAddDigit ; done with calculation!

DigOverflow: ; called if overflow in multiplying BX has occurred
    MOV overflowflag, OVERFLOW_ERROR_CONST ; set overflow flag (to
                                           ; indicate an overflow happened)
    JMP EndChangeRelSpeed


EndAddDigit:
    RET

AddDigit		ENDP


; ExecuteCommand
;
; Description: This function is called in the last state of the state machine.
; It sets up the registers appropriately and then uses a call table to call
; the appropriate functions to call the motor/turret/laser functions to complete
; parsing of the serial command.
;
; Operation: This is done by setting AX to the number to change the direction/
; speed of the motor or the angle/ elevation angle of the turret or
; 0 if the laser is being chnaged. BX is set to the command,
; and CX is set to the sign of the number.
;
; Arguments: None.
; Return Value: None.
;
; Local Variables: None.
; Shared Variables: number: (would follow a S, V, D, T, or E). is the number
;                           to set as new/relative speed or angle of the
;                           motors/ turret, depending on the value of command.
;                   command: part of passed in string that describes
;                            what motor function to eventually call
;                   signvar: set based on presence of negative/ positive sign in
;                         passed in ASCII string
;                   overflowflag: represents status of overflow error.
;                           set if an overflow error occurred,
;                           cleared if no overflow error occurred.
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

ClearStuff:
    MOV overflowflag, 0 ; this can be set in the various calls to
                        ; motor/ turret functions. make sure it is not
                        ; set before the call (so that if there is no
                        ; overflow, the flag is 0)
    MOV BX, 0 ; clear out these registers because only storing things
              ; in lower bytes, and don't want any confusion
              ; with calculations
    MOV CX, 0

CallAppropriateFunc:
    MOV AX, number ; setting up registers (functions are called knowing
				   ; that number, command, and signvar are in these registers)
    MOV BL, command
    MOV CL, signvar
    SHL BL, 1 ; shift BL by 1 (multiply by 2) to access call table because the
              ; command call table is a word table and thus the indices
              ; go by 2 instead of by 1.
              ; The command table is indexed based on command (which
              ; is stored in BL, so we are using BL to access the
              ; command table)
    Call CS:CommandCallTable[BX] ; get action corresponding with commdn

    ; The overflow flag will be set if an overflow error occurred
    ; This is dealt with ParseSerialChar
EndExecuteCommand:
    Call InitParser ; re initialize all shared variables
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

; ChangeSpeed
;
; Description: This function takes in three values. One, in AX, is the
; new speed. In BX is the command corresponding to the function (which
; in this case, is S). In CX is the sign of the number, which should be 0,
; representing that the number is unsigned. The only important value in
; this function is AX, the new speed. This value is set to be the new 
; speed of the RoboTrike. 
;
; Operation: This function changes the speed of the RoboTrike, assuming
; the number in AX is the absolute speed. This is done by calling SetMotorSpeed 
; with the speed in AX and a value for the angle in BX that will cause the
; angle to not be changed (since SetMotorSpeed changes both the speed and 
; the angle). This is done by setting the angle to NO_CHANGE_ANGLE.
;
; Arguments: AX: number to set as speed of RoboTrike
;            BX: command corresponding to action about to perform
;            CX: sign of value in AX
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
;
; Registers Changed: None.
; Stack Depth: None.
;
; Limitations: None.
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/24/16
;

ChangeSpeed      PROC        NEAR
        PUBLIC      ChangeSpeed
SetNewSpeed:
  MOV BX, NO_CHANGE_ANGLE ; sets angle to a value so that the angle
                          ; is not changed
  Call SetMotorSpeed ; changes speed (speed to change to is in AX)

EndChangeSpeed:
  RET

ChangeSpeed	ENDP


; ChangeRelSpeed
;
; Description: This function takes in three values. One, in AX, is the
; offset of speed (the value to be added/subtracted from the old
; speed of the RoboTrike). In BX is the command corresponding to the function
; (in this case, V). In CX is the sign of the number, which will determine
; whether to add or subtract the offset of the speed from the old speed.
; The passed in value in AX is assumed to be a 16 bit signed integer
; (so it's magnitude should only have 15 bits). If the magnitude 
; of this number is greater than 15 bits, an overflow error has occurred (
; the only exception to this rule is if the magnitude of the number is
; UPPER_BOUND and the sign is negative because the bound is inclusive
; on that side).
; If no overflow error occurred, the offset of the speed is added/ subtracted 
; to the old speed, and then this value is checked to see if it exceeds the 
; speed bounds (is less than 0 or equal to the speed that when set to the motor 
; speed, does not change the motor speed). If it does, caps are set on the speed 
; so that an appropriate value can be sent to SetMotorSpeed. 
;
; Operation: This function changes the speed of the RoboTrike, assuming
; the number in AX is the relative speed. This function adds or subtracts
; the passed in speed (in AX) to the original speed of the RoboTrike based
; the value of CX (the sign of the number). It ensures that the passed in
; speed is at most 15 bits (except for if the magnitude of the number
; is UPPER_BOUND and sign = -), and if it is not, the overflow flag is set.
; Then it checks that the total speed is within bounds (greater than 0 and 
; greater than or equal to NO_CHANGE_SPEED).
; If the speed is not within those bounds, it is capped
; at the maximum value (NO_CHANGE_SPEED - 1) or minimum value (0). Finally,
; SetMotorSpeed is called with a value in BX to prevent the direction
; from changing, and the appropriate speed in AX.
;
; Arguments: None.
; Return Value: None.
;
; Local Variables: None.
; Arguments: AX: number to add/subtract to current speed of RoboTrike
;            BX: command corresponding to action about to perform
;            CX: sign of value in AX
;
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

ChangeRelSpeed      PROC        NEAR
        PUBLIC      ChangeRelSpeed


GetOldSpeed:
  PUSH CX ; save sign of speed addition
  MOV DX, AX ; move addition of speed into DX
  Call GetMotorSpeed ; moves old speed into AX
  POP CX  ; get back sign of speed addition

InitChangeRelSpeed:
  CMP CX, NEG_SIGN_CONST ; check if the addition to speed should be negative
  JE TestSubSpeed ; if so, subtract DX from old speed

AddSpeed: ;addition to speed is positive, so add speed
  TEST DX, DX ; check that DX is at most 15 bits (because argument
			  ; needs to be a 16 bit signed integer, so magnitude at
			  ; most 15 bits)
  JS RelSpeedOE ; if highest bit is not set, sign flag will not be set
                ; if it is, overflow error will be recorded
  ; if no overflow, continue
  ADD AX, DX ; add DX (new speed) to old speed
  JC SetMaxCap ; if carry flag is set, make sure to set max cap (because
			   ; value is too large now)
  CMP AX, NO_CHANGE_SPEED ; check that speed is not set to the value
                          ; that would ensure the speed is not changed
  JE SetMaxCap ; if it is, set speed to be that value - 1
  JMP SetNewRelSpeed ; if speed is within bounds, can set speed

TestSubSpeed:
  CMP DX, UPPER_BOUND ; if the relative speed is -UPPER_BOUND,
                            ; is okay for first bit to be signed (because
                            ; the bound is inclusive on the lower half)
                            ; thus this special case needs to be tested for
  JE SubSpeed ; if it is this special case, can jump to subtracting the speed
  TEST DX, DX ; now, check that DX is at most 15 bits (if not special
              ; value UPPER_BOUND)
  JS RelSpeedOE ; if highest bit is not set, sign flag will not be set
                ; if it is, overflow error will be recorded

SubSpeed:
  SUB AX, DX ; subtract DX from old speed
  JC SetMinCap ; if carry, negative value, set cap
  JMP SetNewRelSpeed ; if no carry, can set new speed

SetMaxCap:
    MOV AX, NO_CHANGE_SPEED - 1 ; cap AX at maximum speed value
    JMP SetNewRelSpeed ; can set new speed now

SetMinCap:
    MOV AX, 0 ; cap AX at minimum speed value (0)
    JMP SetNewRelSpeed ; can set new speed now

RelSpeedOE: ; reporting overflow error by setting value of overflowflag to a certain
            ; value
  MOV overflowflag, OVERFLOW_ERROR_CONST
  JMP EndChangeRelSpeed

SetNewRelSpeed:
  MOV BX, NO_CHANGE_ANGLE ; sets angle to a value so that the angle
                          ; is not changed
  Call SetMotorSpeed ; changes speed

EndChangeRelSpeed:
  RET

ChangeRelSpeed	ENDP


; ChangeDir
;
; Description: This function takes in three values. One, in AX, is the
; offset of angle (the value to be added/subtracted from the old
; angle of the RoboTrike). In BX is the command corresponding to the function
; (in this case, D). In CX is the sign of the number, which will determine
; whether to add or subtract the offset of the angle from the old angle.
; The passed in value in AX is assumed to be a 16 bit signed integer
; (so it's magnitude should only have 15 bits). If the magnitude 
; of this number is greater than 15 bits, an overflow error has occurred (
; the only exception to this rule is if the magnitude of the number is
; UPPER_BOUND and the sign is negative because the bound is inclusive
; on that side). If no argument overflow, 
; the offset of the angle is added/ subtracted to the old angle, and then
; this value is checked to see if it overflows. If it does, AX is to
; show that an overflow error occurred. If there is no overflow, SetMotorSpeed
; is called (which also changes the angle).
;
; Operation: This function changes the direction of the RoboTrike, assuming
; the number in AX is the relative angle. This function adds or subtracts
; the passed in angle (in AX) to the original angle of the RoboTrike based
; the value of CX (the sign of the number). First, it ensures that the passed in
; direction is at most 15 bits (excluding a special value for when
; the sign is set), and if it is not, the overflow flag is set. Then,
; the new angle is calculated (added or subtracted from the old angle)
; and it is MOD-ed with NEW_ANGLE_BOUND to fit the angle bounds before
; being returned.
;
; Arguments: AX: number to add/subtract to current direction of RoboTrike
;            BX: command corresponding to action about to perform
;            CX: sign of value in AX
;
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
;
; Registers Changed: None.
; Stack Depth: None.
;
; Limitations: None.
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/24/16
;

ChangeDir      PROC        NEAR
        PUBLIC      ChangeDir

GetOldDir:
  PUSH CX ; save sign of number
  MOV DX, AX ; move offset of angle into DX
  Call GetMotorDirection ; moves old angle into AX
  POP CX  ; get back sign of offset of angle

SpecialDivCase:
  CMP CX, NEG_SIGN_CONST ; check if negative sign (relative direction should
                         ; have negative sign attached to it)
  JNE CheckDirOverflow
  CMP DX, UPPER_BOUND ; if the relative direction is -UPPER_BOUND,
                            ; is okay for first bit to be signed (because
                            ; the bound is inclusive on the lower half)
                            ; thus this special case needs to be tested for
  JE SubDir ; if this is the case, can jump to calculating new direction

CheckDirOverflow: ; reporting overflow error by setting value of AX to a certain
       ; value
  TEST DX, DX ; check if input is at most 15 bits
  JS DirOE ; if highest bit is not set, sign flag will not be set
           ; if sign flag is set, overflow

InitChangeDir:
  CMP CX, NEG_SIGN_CONST ; check if the addition to angle should be negative
  JE SubDir ; if so, subtract DX from old angle

AddDir:
  ADD AX, DX ; if addition to angle should be positive, add DX to old angle
  ; the following steps mod AX with NEW_ANGLE_BOUND to ensure that
  ; AX is < NEW_ANGLE_BOUND
  MOV DX, 0 ; clear DX
  MOV BX, NEW_ANGLE_BOUND
  DIV BX ; divide angle by NEW_ANGLE_BOUND
  XCHG DX, AX ; put remainder in AX (AX = AX mod NEW_ANGLE_BOUND)
  JMP SetNewDir ; can set new direction now

SubDir:
  SUB AX, DX ; subtracts DX from old angle
  CWD ; clears DX for signed division
  MOV BX, NEW_ANGLE_BOUND
  IDIV BX ; divides angle by NEW_ANGLE_BOUND
  XCHG DX, AX ; put remainder in AX (AX = AX mod NEW_ANGLE_BOUND)
  JMP SetNewDir ; can set new direction now

DirOE: ; reporting overflow error by setting value of overflowflag to a certain
            ; value
  MOV overflowflag, OVERFLOW_ERROR_CONST
  JMP EndChangeDir

SetNewDir:
  MOV BX, AX ; move angle into BX (how angle is set in SetMotorSpeed)
  MOV AX, NO_CHANGE_SPEED ; sets speed to a value so that the speed
                          ; is not changed
  Call SetMotorSpeed ; changes angle

EndChangeDir:
  RET


ChangeDir	ENDP


; ChangeTurrAng
;
; Description: This function takes in three values. One, in AX, is the
; offset of angle (the value to be added/subtracted from the old
; angle of the turret). In BX is the command corresponding to the function
; (in this case, T). In CX is the sign of the number, which will determine
; whether to add or subtract the offset of the angle from the old angle.
; The passed in value in AX is assumed to be a 16 bit signed integer
; (so it's magnitude should only have 15 bits). If the magnitude 
; of this number is greater than 15 bits, an overflow error has occurred (
; the only exception to this rule is if the magnitude of the number is
; UPPER_BOUND and the sign is negative because the bound is inclusive
; on that side). If no argument overflow, 
; this function determines whether the relative angle or the
; absolute angle should be set based on the sign shared variable signvar. If
; the relative angle is set, SetRelTurretAngle is called with AX set as the
; relative angle. If the absolute angle is set, SetTurretAngle is called
; with AX as the absolute angle.
;
; Operation: First, this function checks that the passed in value (AX)
; turret angle is at most 15 bits (excluding a special value for when
; the sign is set), and if it is not, the overflow flag is set.
; Then, the sign shared variable (stored in CX) is checked
; to perform the rest of the calculations. If the
; sign shared variable is NEG_SIGN_CONST, that means the relative
; angle of the turret should be set to be - AX. If the sign shared variable is
; POS_SIGN_CONST,that means the relative angle of the turretshould be set to be
; AX. And finally, if the sign shared variable is set to NO_SIGN_CONST,
; that means that the absolute angle should be set to be AX.
;
; Arguments: AX: number to add/subtract to current direction of RoboTrike
;            BX: command corresponding to action about to perform
;            CX: sign of value in AX
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
;
; Registers Changed: None.
; Stack Depth: None.
;
; Limitations:
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/24/16
;

ChangeTurrAng      PROC        NEAR
        PUBLIC      ChangeTurrAng

SpecialTurrCase:
  CMP CX, NEG_SIGN_CONST ; check if negative sign (relative direction should
                         ; have negative sign attached to it)
  JNE CheckTurrOverflow
  CMP AX, UPPER_BOUND ; if the turret angle is is -UPPER_BOUND,
                            ; is okay for first bit to be signed (because
                            ; the bound is inclusive on the lower half)
                            ; thus this special case needs to be tested for
  JE SubTurrAng ; if this is the case, can jump to calculating new turret
                ; angle

CheckTurrOverflow:
  TEST AX, AX ; make sure input is at most 15 bits
  JS TurrAngOE ; if highest bit is not set, sign flag will not be set
                ; if sign flag is set, overflow

InitChangeTurrAng:
  CMP CX, NEG_SIGN_CONST ; check if the addition to angle should be negative
  JE SubTurrAng ; if so, set AX to be - AX and call function to set
                ; relative turret angle
  CMP CX, POS_SIGN_CONST ; check if the addition to angle should be positive
  JE AddTurrAng ; if so, can just set relative turret angle with AX as
                ; argument, since AX is positive
  CMP CX, NO_SIGN_CONST ; check if no sign --> means that we want to set
                        ; absolute value of turret angle
  JE SetAbsTurrAng ; if so, call function to set absolute angle

AddTurrAng:
  Call SetRelTurretAngle ; can set relative turret angle with AX as
                         ; relative turret angle to set
  JMP EndChangeTurrAng

SubTurrAng:
  MOV DX, 0
  SUB DX, AX
  MOV AX, DX ; set AX = -AX
  Call SetRelTurretAngle ; can set relative turret angle with -AX as
                         ; relative turret angle to set
  JMP EndChangeTurrAng

SetAbsTurrAng:
  Call SetTurretAngle ; sets absolute turret angle to AX
  JMP EndChangeTurrAng

TurrAngOE: ; called if overflow error occurred
  MOV overflowflag, OVERFLOW_ERROR_CONST
  JMP EndChangeTurrAng


EndChangeTurrAng:
  RET

ChangeTurrAng	ENDP


; ChangeTurrElvAng
;
; Description: This function takes in three values. One, in AX, is the
; offset of elevation angle (the value to be added/subtracted from the old
; elevation angle of the turret). In BX is the command corresponding to the
; function (in this case, E). In CX is the sign of the number, which will
; determine if the elevation angle is negative or positive. This function checks
; if AX is within the appropriate bounds of turret elevation angles. If not,
; it sends an overflow error. If it is, this function changes the
; turret elevation angle, taking in to account a negative angle if the
; sign shared variable indicates the number is signed.
;
; Operation: This function checks if AX is greater than TURR_ELV_BOUND. If
; it is, then the function changes AX to indicate an overflow error
; has occurred. If not, then the turret elevation angle can be changed.
; This function then checks the sign shared variable. If it is NEG_SIGN_CONST,
; we make AX negative. Then we call SetTurretElevation with the elevation angle
; to set in AX.
;
; Arguments: AX: number to add/subtract to current direction of RoboTrike
;            BX: command corresponding to action about to perform
;            CX: sign of value in AX
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
;
; Registers Changed: None.
; Stack Depth: None.
;
; Limitations:
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/24/16
;

ChangeTurrElvAng      PROC        NEAR
        PUBLIC      ChangeTurrElvAng

InitTurrElvAng: ; ensures that AX falls within the appropriate bounds
  CMP AX, TURR_ELV_BOUND ; if greater than bound, call overflow error
  JG TurrElvAngOE

CheckSign: ; if not, check sign
  CMP CX, NEG_SIGN_CONST ; if sign register is set to be NEG_SIGN_CONST,
                         ; want to pass in negative number to set turret
                         ; elevation function
  JNE SetTurrElvAng ; if not, pass in positive value
  MOV DX, 0 ; convert AX to - AX to pass in negative number to set turret
            ; elevation angle
  SUB DX, AX
  MOV AX, DX ; set AX = -AX

SetTurrElvAng:
  Call SetTurretElevation ; sets turret elevation angle to whatever is in AX
  JMP EndTurrElvAng

TurrElvAngOE: ; reporting overflow error by setting value of AX to a certain
         ; value
  MOV overflowflag, OVERFLOW_ERROR_CONST

EndTurrElvAng:
  RET

ChangeTurrElvAng	ENDP


; ChangeLaser
;
; Description: This function sets the laser based on the value of BX (the
; command of the action to perform).
;
; Operation: If the command variable is TOKEN_FIRELASER, SetLaser is called
; with 1 in AX to turn the laser on. If the command variable is TOKEN_OFFLASER,
; SetLaser is called with 0 in AX to turn the laser off.
;
; Arguments: BX: command corresponding to action about to perform
;
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
; Registers Changed: None.
; Stack Depth: None.
;
; Limitations: None.
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/24/16
;

ChangeLaser      PROC        NEAR
        PUBLIC      ChangeLaser

InitChangeLaser:
  SHR BX, 1 ; bx was shifted left to index the command call table
            ; so shift back
  CMP BX, TOKEN_FIRELASER ; compare command with the token for
                          ; firing laser
  JE FireLaser ; if same, fire laser
  CMP BX, TOKEN_OFFLASER ; compare command with the token for
                         ; turning laser off laser
  JE OffLaser; if same, turn laser off

FireLaser:
  MOV AX, 1 ; set laser status to nonzero value (indicating want laser on)
  JMP SetLaserStatus ; call function to turn laser on

OffLaser:
  MOV AX, 0 ; set laser status to zero, indicating want laser off

SetLaserStatus:
  Call SetLaser ; call function to set laser with status in AX

EndChangeLaser:
  RET

ChangeLaser	ENDP

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
    %TRANSITION(INIT, InitParser)		                     ;TOKEN_SIGN
    %TRANSITION(INIT, InitParser)		                     ;TOKEN_DIGIT
    %TRANSITION(INIT, InitParser)		                     ;TOKEN_CR
    %TRANSITION(INIT, InitParser)		                     ;TOKEN_IGNORE
    %TRANSITION(INIT, InitParser)		                     ;TOKEN_OTHER

	;Current State = VALID_COMMAND                       Input Token Type
    %TRANSITION(ERROR_STATE, InitParser)                 ;TOKEN_SPEED
	%TRANSITION(ERROR_STATE, InitParser)		         ;TOKEN_RELSPEED
	%TRANSITION(ERROR_STATE, InitParser)	             ;TOKEN_DIR
	%TRANSITION(ERROR_STATE, InitParser)		         ;TOKEN_TURRANG
	%TRANSITION(ERROR_STATE, InitParser)		         ;TOKEN_ELVTURRANG
	%TRANSITION(ERROR_STATE, InitParser)		         ;TOKEN_FIRELASER
    %TRANSITION(ERROR_STATE, InitParser)			     ;TOKEN_OFFLASER
    %TRANSITION(SIGN, SetSign)		                     ;TOKEN_SIGN
    %TRANSITION(ADD_DIGIT, AddDigit)		             ;TOKEN_DIGIT
    %TRANSITION(ERROR_STATE, InitParser)		         ;TOKEN_CR
    %TRANSITION(VALID_COMMAND, doNOP)		             ;TOKEN_IGNORE
    %TRANSITION(ERROR_STATE, InitParser)			     ;TOKEN_OTHER

    ;Current State = SIGN                                Input Token Type
    %TRANSITION(ERROR_STATE, InitParser)                 ;TOKEN_SPEED
	%TRANSITION(ERROR_STATE, InitParser)		         ;TOKEN_RELSPEED
	%TRANSITION(ERROR_STATE, InitParser)	             ;TOKEN_DIR
	%TRANSITION(ERROR_STATE, InitParser)		         ;TOKEN_TURRANG
	%TRANSITION(ERROR_STATE, InitParser)		         ;TOKEN_ELVTURRANG
	%TRANSITION(ERROR_STATE, InitParser)		         ;TOKEN_FIRELASER
    %TRANSITION(ERROR_STATE, InitParser)			     ;TOKEN_OFFLASER
    %TRANSITION(ERROR_STATE, InitParser)		         ;TOKEN_SIGN
    %TRANSITION(ADD_DIGIT, AddDigit)		             ;TOKEN_DIGIT
    %TRANSITION(ERROR_STATE, InitParser)		         ;TOKEN_CR
    %TRANSITION(SIGN, doNOP)		                     ;TOKEN_IGNORE
    %TRANSITION(ERROR_STATE, InitParser)			     ;TOKEN_OTHER

    ;Current State = ADD_DIGIT                          Input Token Type
    %TRANSITION(ERROR_STATE, InitParser)                ;TOKEN_SPEED
	%TRANSITION(ERROR_STATE, InitParser)		        ;TOKEN_RELSPEED
	%TRANSITION(ERROR_STATE, InitParser)	            ;TOKEN_DIR
	%TRANSITION(ERROR_STATE, InitParser)		        ;TOKEN_TURRANG
	%TRANSITION(ERROR_STATE, InitParser)		        ;TOKEN_ELVTURRANG
	%TRANSITION(ERROR_STATE, InitParser)		        ;TOKEN_FIRELASER
    %TRANSITION(ERROR_STATE, InitParser)			    ;TOKEN_OFFLASER
    %TRANSITION(ERROR_STATE, InitParser)		        ;TOKEN_SIGN
    %TRANSITION(ADD_DIGIT, AddDigit)		            ;TOKEN_DIGIT
    %TRANSITION(INIT, ExecuteCommand)		            ;TOKEN_CR
    %TRANSITION(ADD_DIGIT, doNOP)		                ;TOKEN_IGNORE
    %TRANSITION(ERROR_STATE, InitParser)			    ;TOKEN_OTHER


    ;Current State = LASER                              Input Token Type
    %TRANSITION(ERROR_STATE, InitParser)                ;TOKEN_SPEED
	%TRANSITION(ERROR_STATE, InitParser)		        ;TOKEN_RELSPEED
	%TRANSITION(ERROR_STATE, InitParser)	            ;TOKEN_DIR
	%TRANSITION(ERROR_STATE, InitParser)		        ;TOKEN_TURRANG
	%TRANSITION(ERROR_STATE, InitParser)		        ;TOKEN_ELVTURRANG
	%TRANSITION(ERROR_STATE, InitParser)		        ;TOKEN_FIRELASER
    %TRANSITION(ERROR_STATE, InitParser)			    ;TOKEN_OFFLASER
    %TRANSITION(ERROR_STATE, InitParser)		        ;TOKEN_SIGN
    %TRANSITION(ERROR_STATE, InitParser)		        ;TOKEN_DIGIT
    %TRANSITION(INIT, ExecuteCommand)                   ;TOKEN_CR
    %TRANSITION(LASER, doNOP)		                    ;TOKEN_IGNORE
    %TRANSITION(ERROR_STATE, InitParser)			    ;TOKEN_OTHER

    ;Current State = ERROR_STATE                         Input Token Type
    %TRANSITION(INIT, InitParser)                        ;TOKEN_SPEED
	%TRANSITION(INIT, InitParser)		                    ;TOKEN_RELSPEED
	%TRANSITION(INIT, InitParser)	                        ;TOKEN_DIR
	%TRANSITION(INIT, InitParser)		                    ;TOKEN_TURRANG
	%TRANSITION(INIT, InitParser)		                    ;TOKEN_ELVTURRANG
	%TRANSITION(INIT, InitParser)		                    ;TOKEN_FIRELASER
    %TRANSITION(INIT, InitParser)			                ;TOKEN_OFFLASER
    %TRANSITION(INIT, InitParser)	                        ;TOKEN_SIGN
    %TRANSITION(INIT, InitParser)		                    ;TOKEN_DIGIT
    %TRANSITION(INIT, InitParser)		                    ;TOKEN_CR
    %TRANSITION(INIT, InitParser)		                    ;TOKEN_IGNORE
    %TRANSITION(INIT, InitParser)			                ;TOKEN_OTHER


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


InitGetFPToken:				;setup for lookups
	AND	AL, TOKEN_MASK		;strip unused bits (high bit)
	MOV	AH, AL			;and preserve value in AH


TokenTypeLookup:                        ;get the token type
    MOV     BX, OFFSET(TokenTypeTable)  ;BX points at table
	XLAT	CS:TokenTypeTable	;have token type in AL
	XCHG	AH, AL			;token type in AH, character in AL

TokenValueLookup:			;get the token value
    MOV     BX, OFFSET(TokenValueTable)  ;BX points at table
	XLAT	CS:TokenValueTable	;have token value in AL


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
        %TABENT(TOKEN_IGNORE, 9)		;TAB
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
        %TABENT(TOKEN_IGNORE, ' ')	;space
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
overflowflag    DW    ?
DATA    ENDS


END
