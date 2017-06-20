NAME    PHELP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                           Parser Helper Functions                          ;
;              Helper functions to aid in parsing serial input               ;
;                                  EE/CS 51                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Overall Description:
;
; Table of Contents
;
; Revision History:
;    11/24/16  Meera Krishnamoorthy   wrote code


CGROUP  GROUP   CODE


CODE	SEGMENT PUBLIC 'CODE'


ASSUME  CS:CGROUP

$INCLUDE(PARSER.INC)

EXTRN   SetMotorSpeed:NEAR
EXTRN   GetMotorSpeed:NEAR
EXTRN   GetMotorDirection:NEAR

EXTRN   GetTurretAngle:NEAR
EXTRN   SetRelTurretAngle:NEAR
EXTRN   SetTurretAngle:NEAR
EXTRN   SetTurretElevation:NEAR

EXTRN   SetLaser:NEAR

; ChangeSpeed
;
; Description: This function takes in three values. One, in AX, is the
; new speed. In BX is the command corresponding to the function (which
; in this case, is S). In CX is the sign of the number, which should be 0,
; representing that the number is unsigned. The only important value in
; this function is AX, the new speed. This value is checked to ensure
; it fits bounds, and then is set to be the new speed of the RoboTrike by
; calling SetMotorSpeed with the speed in AX and a value for the angle in
; BX that will not change the angle (since SetMotorSpeed changes both
; the speed and the angle).
;
; Operation: This function changes the speed of the RoboTrike, assuming
; the number in AX is the absolute speed. Before it calls SetMotorSpeed,
; a function that changes the motor speed, it ensures the speed it
; passes in AX (which is how SetMotorSpeed should be called) is less
; than the limit (called NO_CHANGE_SPEED).
;
; Operation: This is done by setting the angle to NO_CHANGE_ANGLE, so that
; the angle will not change while changing the speed. Then, this function
; calls SetMotorSpeed.
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

CheckSpeedLim:
  CMP AX, NO_CHANGE_SPEED ; make sure that speed is not greater than
                          ; upper limit (NO_CHANGE_SPEED)
  JG SpeedOE ; if it is, set AX to be overflow_error_const

SpeedOE:
  MOV AX, OVERFLOW_ERROR_CONST ; set AX to be a value representing that an
                               ; overflow error occurred
  JMP EndChangeSpeed

SetNewSpeed:
  MOV BX, NO_CHANGE_ANGLE ; sets angle to a value so that the angle
                          ; is not changed
  Call SetMotorSpeed ; changes speed
  MOV AX, 0 ; to make sure a non zero value isn't recorded in AX to 
            ; make it seem like there was an overflow error

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
; The offset of the speed is added/ subtracted to the old speed, and then
; this value is checked to see if it overflows. If it does, AX is to
; show that an overflow error occurred. If there is no overflow, SetMotorSpeed
; is called.
;
; Operation: This function changes the speed of the RoboTrike, assuming
; the number in AX is the relative speed. This function adds or subtracts
; the passed in speed (in AX) to the original speed of the RoboTrike based
; the value of CX (the sign of the number). Then it compares this value
; to 0 and the maximum speed (NO_CHANGE_SPEED) to ensure that the speed
; lies within appropriate bounds (cannot be negative or too large). If
; the speed is within those bounds, the new speed is set to the original
; speed +- AX. If not, the speed is not set to a new value and AX
; is passed back with an error value.
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
  PUSH CX ; save sign of number
  MOV DX, AX ; move addition of speed into DX
  Call GetMotorSpeed ; moves old speed into AX
  POP CX  ; get back sign of number

InitChangeRelSpeed:
  CMP CX, NEG_SIGN_CONST ; check if the addition to speed should be negative
  JE SubSpeed ; if so, subtract DX from old speed

AddSpeed:
  ADD AX, DX ; if addition to speed should be positive, add DX to old speed
  JMP CheckSpeedLim1 ; now check that new speed value is between bounds

SubSpeed:
  SUB AX, DX ; subtract DX from old speed

CheckSpeedLim1:
  CMP AX, 0 ; make sure that speed is not less than
            ; lower limit (0)
  JL RelSpeedOE ; if is less than lower limit, report overflow error
                ; if is, check next limit

CheckSpeedLim2:
  CMP AX, NO_CHANGE_SPEED ; make sure that speed is not greater than
                          ; upper limit (NO_CHANGE_SPEED)
  JLE SetNewRelSpeed      ; if is less than upper limit, can set new speed
                          ; if is greater than upper limit, need to ReportError
                          ; overflow error

RelSpeedOE: ; reporting overflow error by setting value of AX to a certain
            ; value
  MOV AX, OVERFLOW_ERROR_CONST
  JMP EndChangeRelSpeed

SetNewRelSpeed:
  MOV BX, NO_CHANGE_ANGLE ; sets angle to a value so that the angle
                          ; is not changed
  Call SetMotorSpeed ; changes speed
  MOV AX, 0 ; to make sure a non zero value isn't recorded in AX to 
            ; make it seem like there was an overflow error
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
; The offset of the angle is added/ subtracted to the old angle, and then
; this value is checked to see if it overflows. If it does, AX is to
; show that an overflow error occurred. If there is no overflow, SetMotorSpeed
; is called (which also changes the angle).
;
; Operation: This function changes the direction of the RoboTrike, assuming
; the number in AX is the relative angle. This function adds or subtracts
; the passed in angle (in AX) to the original angle of the RoboTrike based
; the value of CX (the sign of the number). Then it compares this value
; to the minimum angle (NO_CHANGE_ANGLE) and the maximum angle NOT(NO_CHANGE_ANGLE)
; to ensure that the angle lies within appropriate bounds (cannot be too negative
; or too positive). If the angle is within those bounds, the new speed is
; set to the original speed +- AX. If not, the speed is not set to a new
; value and AX is passed back with an error value.
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

InitChangeDir:
  CMP CX, NEG_SIGN_CONST ; check if the addition to angle should be negative
  JE SubDir ; if so, subtract DX from old angle

AddDir:
  ADD AX, DX ; if addition to angle should be positive, add DX to old angle
  JMP CheckDirLim1 ; check that direction value is within bounds

SubDir:
  SUB AX, DX ; subtracts DX from old angle

CheckDirLim1:
  CMP AX, NO_CHANGE_ANGLE  ; make sure that angle is not less than
                           ; lower limit (NO_CHANGE_ANGLE)
  JL DirOE

CheckDirLim2:
  CMP AX, NOT(NO_CHANGE_ANGLE) ; make sure that angle is not greater than
                               ; upper limit NOT(NO_CHANGE_ANGLE)
  JG DirOE

DirOE: ; reporting overflow error by setting value of AX to a certain
       ; value
  MOV AX, OVERFLOW_ERROR_CONST
  JMP EndChangeDir

SetNewDir:
  MOV BX, AX ; move angle into BX (how angle is set in SetMotorSpeed)
  MOV AX, NO_CHANGE_SPEED ; sets speed to a value so that the speed
                          ; is not changed
  Call SetMotorSpeed ; changes angle
  MOV AX, 0 ; to make sure a non zero value isn't recorded in AX to 
            ; make it seem like there was an overflow error

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
; This function determines whether the relative angle or the
; absolute angle should be set based on the sign shared variable. If
; the relative angle is set, SetRelTurretAngle is called with AX set as the
; relative angle. If the absolute angle is set, SetTurretAngle is called
; with AX as the absolute angle.
;
; Operation: This is done by checking the sign shared variable. If the
; sign shared variable is NEG_SIGN_CONST, that means the relative
; angle of the turret should be set to be - AX. If the sign shared variable is
; POS_SIGN_CONST,that means the relative angle of the turretshould be set to be
; AX. And finally, if the sign shared variable is set to NO_SIGN_CONST,
; that means that the absolute angle should be set to be AX.
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
  ; with turret angle in AX
  Call SetRelTurretAngle
  JMP EndChangeTurrAng

SubTurrAng:
  MOV DX, 0
  SUB DX, AX
  MOV AX, DX ; set AX = -AX
  Call SetRelTurretAngle ; so that the angle is subtracted from original
                         ; angle
  JMP EndChangeTurrAng

SetAbsTurrAng:
  Call SetTurretAngle ; sets absolute turret angle
  MOV AX, 0 ; to make sure a non zero value isn't recorded in AX to 
            ; make it seem like there was an overflow error

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
  CMP AX, TURR_ELV_BOUND
  JG TurrElvAngOE

CheckSign:
  CMP CX, NEG_SIGN_CONST
  JNE SetTurrElvAng
  MOV DX, 0
  SUB DX, AX
  MOV AX, DX ; set AX = -AX

SetTurrElvAng:
  Call SetTurretElevation ; sets turret elevation angle to whatever is in AX
  MOV AX, 0 ; to make sure a non zero value isn't recorded in AX to 
            ; make it seem like there was an overflow error
  JMP EndTurrElvAng

TurrElvAngOE: ; reporting overflow error by setting value of AX to a certain
         ; value
  MOV AX, OVERFLOW_ERROR_CONST

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
; Arguments: BX: sign of value in AX
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
  CMP BX, TOKEN_FIRELASER
  JE FireLaser
  CMP BX, TOKEN_OFFLASER
  JE OffLaser

FireLaser:
  MOV AX, 1
  JMP SetLaserStatus

OffLaser:
  MOV AX, 0

SetLaserStatus:
  Call SetLaser

EndChangeLaser:
  MOV AX, 0 ; to make sure a non zero value isn't recorded in AX to 
            ; make it seem like there was an overflow error
  RET

ChangeLaser	ENDP



CODE    ENDS


END
