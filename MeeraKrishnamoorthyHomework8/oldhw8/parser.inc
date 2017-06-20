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
EXECUTE_COMMAND	    EQU	4		; get command to execute
LASER               EQU 5       ; to do if laser
ERROR_STATE		    EQU	6		; state to go to if any errors


NUM_STATES	        EQU	7		; number of states


; Token Type Definitions
;    note that these MUST match the layout of the transition table

TOKEN_SPEED       EQU 0
TOKEN_RELSPEED    EQU 1
TOKEN_DIR         EQU 2
TOKEN_TURRANG     EQU 3
TOKEN_ELVTURRANG  EQU 4
TOKEN_FIRELASER   EQU 5
TOKEN_OFFLASER    EQU 6
TOKEN_SIGN        EQU 7
TOKEN_DIGIT       EQU 8
TOKEN_CR          EQU 9
TOKEN_OTHER       EQU 10

NUM_TOKEN_TYPES	EQU	11		;number of token types

; Sign-related constants
NEG_SIGN_CONST         EQU -1
NO_SIGN_CONST          EQU 0
POS_SIGN_CONST         EQU 1

; Error related constants
ERROR_CONST            EQU  123 ; generic non zero value to set AX to
                                ; if there is an error
OVERFLOW_ERROR_CONST   EQU  29 ; random constant to indicate overflow error
                               ; occurred

; setting speed/ angle for motor and turret constants
NO_CHANGE_ANGLE        EQU -32768
NO_CHANGE_SPEED        EQU 65535
TURR_ELV_BOUND         EQU 60

TOKEN_MASK	EQU	01111111B	;mask high bit of token
