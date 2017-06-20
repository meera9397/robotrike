;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                PARSER.INC                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the assembly language definitions for the serial command
; parser.  Mainly these are the state and token type definitions.
;
; Revision History:
;     11/24/16    Meera Krishnamoorthy          initial revision
;



; State definitions
;    note that these MUST match the layout of the transition table

INIT	            EQU	0		; initial state
VALID_COMMAND	    EQU	1		; check if valid first character
SIGN	            EQU 2		; save sign
ADD_DIGIT	        EQU	3		; add digits (create number)
;EXECUTE_COMMAND	EQU	4		; get command to execute
LASER               EQU 4       ; to do if laser
ERROR_STATE		    EQU	5		; state to go to if any errors


NUM_STATES	        EQU	6		; number of states


; Token Type Definitions
;    note that these MUST match the layout of the transition table

TOKEN_SPEED       EQU 0 ; to set absolute speed
TOKEN_RELSPEED    EQU 1 ; to set relative speed
TOKEN_DIR         EQU 2 ; to set direction
TOKEN_TURRANG     EQU 3 ; to set turret angle
TOKEN_ELVTURRANG  EQU 4 ; to set turret elevation angle
TOKEN_FIRELASER   EQU 5 ; to fire laser
TOKEN_OFFLASER    EQU 6 ; to turn off laser
TOKEN_SIGN        EQU 7 ; to indicate number is signed
TOKEN_DIGIT       EQU 8 ; to indicate number
TOKEN_CR          EQU 9 ; to indicate end of entire command (carriage return)
TOKEN_IGNORE      EQU 10 ; to indicate to stay in state (do nothing)
TOKEN_OTHER       EQU 11 ; to indicate error (should never appear)

NUM_TOKEN_TYPES	EQU	12		;number of token types

; Sign-related constants
NEG_SIGN_CONST         EQU 0ffH ; represent that number is negative
                                ; (- before number)
NO_SIGN_CONST          EQU 0 ; represent that no sign was present in command
                             ; (went from s, v, e, d, t to number)
POS_SIGN_CONST         EQU 1 ; represent that number is positive 
                             ; (+ before number)

; Digit constants
BASE                   EQU 10 ; base of number system we are using

; Error related constants
ERROR_CONST            EQU  123 ; generic non zero value to set AX to
                                ; if there is an error
OVERFLOW_ERROR_CONST   EQU  29 ; random constant to indicate overflow error
                               ; occurred

; setting speed/ angle for motor and turret constants
DIG_OVERFLOW           EQU 3276 ; max number that can be multiplied by 10
                                ; and not overflow
UPPER_BOUND            EQU 32768 ; largest value that can be set as relative
                                 ; speed/angle for motor/ turret
NO_CHANGE_ANGLE        EQU -32768 ; value that when angle is set to this, does
                                  ; not change angle
NO_CHANGE_SPEED        EQU 65535 ; value that when speed is set to this, does
                                  ; not change speed
TURR_ELV_BOUND         EQU 60 ; absolute maximum value for turret elevation
NEW_ANGLE_BOUND        EQU 360 ; after calculate new direction for motor,
                               ; want this to be bound for direction
                               ; before changing value in motor

TOKEN_MASK	EQU	01111111B	;mask high bit of token
