NAME  FORCETABLE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  FORCETABLE                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the horizontal/ vertical forces associated with each motor.
; Revision History:
; Meera Krishnamoorthy  11/12/16  create table


;setup code group and start the code segment
CGROUP  GROUP   CODE

CODE    SEGMENT PUBLIC 'CODE'


; MotorForcesX_Table
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

; MotorForcesY_Table
;
; Description:      These are the components of vertical
;                   force given to each motor.
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/12/16
MotorForcesY_Table       LABEL   WORD
        PUBLIC  MotorForcesY_Table

DW          0000H        ; F_1_y
DW          9127H        ; F_2_y
DW          6ED9H        ; F_3_y

CODE    ENDS



END
