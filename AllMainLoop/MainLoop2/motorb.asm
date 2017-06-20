NAME  MOTORBITS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  MOTORBITS                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the bit patterns to tell you if the motor is on/off
; and the direction of the motors.

; Revision History:
; Meera Krishnamoorthy  11/12/16  create table


;setup code group and start the code segment
CGROUP  GROUP   CODE

CODE    SEGMENT PUBLIC 'CODE'


; MotorOnOff
;
; Description:      This table is meant to write to bits 1, 3, 5 of Port B. 
; 					Bit 1 corresponds to Motor 1, bit 3 corresponds to Motor 2
; 					and Bit 5 corresponds to Motor 3. A '1' in the bit values
; 					indicates that that motor is on. 
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/12/16

MotorOnOff       LABEL   BYTE
        PUBLIC  MotorOnOff

DB          00000010B        ; motor 1 is on
DB          00001000B        ; motor 2 is on
DB          00100000B        ; motor 3 is on

; MotorDirection
;
; Description:      This table is meant to write to bits 0, 2, 4 of Port B. 
; 					Bit 0 corresponds to Motor 1, bit 2 corresponds to Motor 2
; 					and Bit 4 corresponds to Motor 3. A '1' in the bit values
; 					indicates that that motor is moving backwards. 
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/12/16
MotorDirection       LABEL   BYTE
        PUBLIC  MotorDirection

DB          00000001B        ; motor 1 is moving backwards
DB          00000100B        ; motor 2 is moving backwards
DB          00010000B        ; motor 3 is moving backwards


CODE    ENDS



END
