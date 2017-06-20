; MOTORS.INC
;
; This is the include file that holds all the constants necessary
; to run the functions in motors.asm, which have to do with turning
; the motors on/ off, and setting the angle/ speed of the motor.

NUM_MOTORS            EQU     3 ; the number of motors
MAX_SPEED             EQU     65535 ; if this speed is inputted
                                    ; the robotrike should not change speed
MIN_ANGLE             EQU     -32768 ; if this angle is inputted
                                    ; the robotrike should not change angle
MAX_ANGLE             EQU     360    ; the upper bound that the angle
                                     ; stored should be
MAX_PULSE_WIDTH       EQU     128    ; maximum length of duty cycle counter

BYTE_TO_WIDTH_CONV    EQU     1   ; amount a number needs to be shifted left
                                  ; or right to be converted from indexing
                                  ; a byte table/array to indexing a word
                                  ; table/ array 
MOVE_BACKWARDS        EQU     1   ; constant stored that indicates that 
                                  ; motor is moving backwards (even though
                                  ; same as BYTE_TO_WIDTH_CONV, can 
                                  ; change this constant, so have a separate
                                  ; constant for this)
NUM_SIGNED_BITS       EQU     2   ; number of signed bits to be removed from
                                  ; three signed numbers being multiplied
NUM_BITS              EQU     8   ; number of bits the parallel port receives
                                  ; from this function

CONTROL_PORT          EQU     0183H ; address of control port
MOTOR_PORT            EQU     0181H ; address of port to write to motors


INIT_SIGNAL           EQU     10000000B ; signal indicating we want B to be output
                                        ; -- this signal is written to the
                                        ; control port.
                                        ; also serves as laser output if
                                        ; laser is on 
