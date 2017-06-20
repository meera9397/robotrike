NAME    MOTORS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   Motors                                   ;
;                                  EE/CS 51                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Overall Description: These are the functions to control the DC motor and laser
; for the Robotrike.

; Table of Contents
; 1)  InitMotors(): initializes variables needed to determine if motors are on
;       or off and run all other functions
; 2)	SetMotorSpeed(speed, angle): sets the speed of the RoboTrike to speed,
;       with 65534 being the full speed; and set the direction of movement to
;       angle in degrees, with 0 degrees being straight ahead.
; 3)	GetMotorSpeed(): gets the current speed setting for the RoboTrike
; 4)	GetMotorDirection(): gets the current direction of movement setting for
;       the RoboTrike
; 5)	SetLaser(onoff): turn on (onoff is true) or turn off (onoff is false) the
;       laser. Onoff is a Boolean variable
; 6)	GetLaser(): get the current laser status (on or off)
; 7)  PWM_Function(): determines whether motor is on/ off based on location
;       in duty cycle

; All shared variables: driveSpeed, driveAngle, laserStatus, pulseWidths[],
; pulseWidthCounter, overallSignal
;
; Revision History:
;    11/11/16  Meera Krishnamoorthy   wrote code


CGROUP  GROUP   CODE
DGROUP  GROUP   DATA


CODE	SEGMENT PUBLIC 'CODE'


ASSUME  CS:CGROUP, DS: DGROUP
EXTRN   Sin_Table:WORD       ; include Trig_Table
EXTRN   Cos_Table:WORD       ; include Trig_Table

EXTRN   MotorForcesX_Table:WORD      ; include Force Table
EXTRN   MotorForcesY_Table:WORD      ; include Force Table

EXTRN   MotorOnOff:BYTE          ; include Force Table
EXTRN   MotorDirection:BYTE      ; include Force Table
$include(motors.inc)

; InitMotor
;
; Description: This function initializes all the shared variables used in
;              functions concerning the motors.
;
; Operation: This is done by setting all shared variables created in the
; data segment to zero.
;
; Arguments: None.
; Return Value: None.
;
; Local Variables: None.
; Shared Variables: 1) driveSpeed, W, type = word, length = 16 bits; driveSpeed
;                    is the variable describing the speed of the RoboTrike,
;                    modified to fit the bounds (0, 65535)
;                   2) driveAngle, W, type = words, length = 16 bits; driveAngle
;                    is the variable describing the direction of the RoboTrike,
;                    modified to fit the bounds (0, 359)
;                   3) laserStatus, W, type = bytes, length = 8 bits; laserStatus
;                    is the variable describing whether the laser is on or
;                    off
;                   4) pulseWidths[], W, type = word, length = (16 bits) *
;                     NUM_MOTORS; pulseWidths is an array holding the power
;                     delivered to each motor. The index of pulseWidth
;                     corresponds to the motor, and pulseWidth[index]
;                     corresponds to the power delivered to that motor.
;                   5) pulseWidthCounter, W, type = word, length = 16 bits;
;                     pulseWidthCounter is a counter called each time
;                     the PWM Event Handler Function is called --  it keeps
;                     track of how many motor time interrupts have occurred
;                     since the start of the duty cycle, and thus which motors
;                     to turn on and off.
;                   6) overallSignal, W, type = byte, length = 8 bits;
;                     this variable describes the signal sent to the motors
;                     telling them if they are on/ off and the direction
;                     that they are travelling in.
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
; Last Modified: 11/11/16
;

InitMotor      PROC        NEAR
        PUBLIC      InitMotor

InitMostVars:
  MOV driveSpeed, 0 ; initialize driveSpeed variable
  MOV driveAngle, 0 ; initialize driveAngle variable
  MOV laserStatus, 0 ; initialize laserStatus variable
  MOV pulseWidthCounter, 0 ; initialize pulseWidthCounter variable
  MOV overallSignal, 0 ; initialize overallSignal variable
  MOV index, 0;

  MOV DX, INIT_MOTOR_ADDRESS
  MOV AX, INIT_SIGNAL

  OUT DX, AX ; write initial signal to INIT_MOTOR_ADDRESS indicating
             ; that port B is an output port

  MOV BX, 0  ; clear out BX (will be used as an index to access elements
             ; in pulseWidths array, starting from the 0th element, so
             ; it is necessary for BX to start as 0)

  MOV AX, NUM_MOTORS
  
InitPulseWidthArray:
  MOV pulseWidths[BX], 0
  INC BX; increment index (to increment BX+1-th element in pulseWidths
         ; array in next loop)
  CMP BX, AX ; check if have reached end of array (are NUM_MOTORS
                         ; elements in the pulseWidths array)
  JLE InitPulseWidthArray ; if have not reached end, loop back to continue
                         ; initializing elements in pulseWidths array
  ; JGE EndInitMotor ; if have reached end, can return

EndInitMotor:
  RET

InitMotor	ENDP



; SetMotorSpeed
;
; Description: The function is passed 2 arguments. The first argument (speed)
; is passed in AX and is the absolute speed at which the Robotrike is to run.
; A speed of 65535 indicates the current speed should not be changed,
; ignoring the speed argument. The second argument (angle) is passed in BX and
; is the signed angle at which the Robotrike is to move in degrees, with 0
; degrees being straight ahead relative to the RoboTrike orientation.
; An angle of -32768 indicates the current direction of travel should not be
; changed, effectively ignoring the angle argument. After it takes in the
; speed and angle, this function calculates the total horizontal and vertical
; power given to each motor based on the fact that the first motor is only
; powered in the horizontal direction, the second and third motors are
; powered in both the horizontal and vertical directions, but more in
; the vertical direction. The force components are in a table.
;
; Operation: This is done by only storing the speed
; arguments if AX != 65535 and only storing the angle argument if BX !=
; -32768. The driveSpeed variable should be between 0 and 65535. The MOD
; of AX is taken with 65535 in order to ensure that these bounds
; are kept, and the value is negated if AX < 0.
; This function has to change the passed in angle before storing it in driveAngle.
; The passed in angle (in BX) will be between -32767 and 32767, and driveAngle
; must be between 0 and 359; thus, the angle needs to be normalized. An angle
; of -32768 indicates the current direction of travel should not be changed.
; Thus, that corresponds to an angle of 0. Thus, -32767 corresponds to
; an angle of 1. To change the bounds from (-32767, 32767) to (0, 359), we
; take the MOD of BX with 32767 and negate the value if it is less
; than 0.
; Then, the function stores AX and the normalized BX in shared variables
; (speed and angle). After it does this, the function
; calculates the power delivered to each motor. The power is delivered
; to each motor thusly: F1 = [1, 0], F2 = [(-1/2), -sqrt(3)/2],
; F3 = [(1/2), -sqrt(3)/2]. The negative signs indicate the angle of the
; motor from the horizontal/ vertical axis. The general formula
; for calculating the power delivered to each motor is
; s_n = F_n_x *  v * cos(theta) + F_n_y * v * sin(theta). We
; get the sin(theta) and cos(theta) values from a lookup table. These powers
; are then stored in a shared array (pulseWidths[]), and are used
; in the PWM event handler to determine which motors to turn on and off.

; Arguments: AX (speed), the speed of the RoboTrike before modified to
;             fit bounds (0, 65535)
;            BX (angle), the direction the RoboTrike moves in before modified
;             to fit bounds (0, 359);
;
; Return Value: None.
;
; Local Variables: None.
; Shared Variables: 1) driveSpeed, W, type = word, length = 16 bits; driveSpeed
;                    is the variable describing the speed of the RoboTrike,
;                    modified to fit the bounds (0, 65535). Possible values
;                    of driveSpeed are 0 - 65535.
;                   2) driveAngle, W, type = words, length = 16 bits; driveAngle
;                    is the variable describing the direction of the RoboTrike,
;                    modified to fit the bounds (0, 359). Possible values
;                    of driveAngle are 0 - 359.
;                   3)  pulseWidths[], W, type = byte, length = (8 bits) *
;                     NUM_MOTORS; pulseWidths is an array holding the power
;                     delivered to each motor. The index of pulseWidth
;                     corresponds to the motor, and pulseWidth[index]
;                     corresponds to the power delivered to that motor.
;
; Global Variables: None.
;
; Input: None.
; Output: None.
;
; Error Handling: None.
;
; Algorithms: PWM calculation
; Data Structures: Trig_Table, MotorForces_Table
;
;
; Registers Changed: None.
; Stack Depth: [exits???]
;
; Limitations: None.
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/11/16
;

SetMotorSpeed      PROC        NEAR
        PUBLIC      SetMotorSpeed

SetSpeed: ; speed is stored in AX
          ; checks if speed = MAX_SPEED
          ; if not, change driveSpeed to AX
          ; negate it if its negative to make sure it's
          ; within lower bounds
  CMP AX, MAX_SPEED ; if speed is equal to MAX_SPEED, should not change
                    ; speed, can jump to check if angle should be set.
  JE SetAngle
  ;CMP AX, 0 ; check that speed (in AX) is positive
  ;JGE ContSetSpeed
  ;NEG AX ; if not, shift value to only look at signed number, not signed bit

;ContSetSpeed: ; now, mod speed with MAX_SPEED to make sure it's less than
              ; upper bound
  ;MOV DX, 0         ; initialize DX for division
  ;CWD
  ;MOV CX, MAX_SPEED
  ;DIV CX     ; take the MOD of speed and MAX_SPEED to ensure that
                    ; speed is within appropriate bounds // remainder of 
                    ; speed / MAX_SPEED is stored in DX
  MOV driveSpeed, AX


SetAngle: ; angle is stored in BX
          ; checks if angle = MAX_ANGLE
          ; if not, change driveAngle to BX
          ; negate it if its negative to make sure it's
          ; within lower bounds

  CMP BX, MIN_ANGLE ; if angle is equal to MAX_ANGLE, should not change
                    ; speed, can jump to computing pulse widths
  JE InitComputePulseWidths

  ;CMP BX, 0 ; check that driveAngle is positive
  ;JGE ContSetAngle
  ;SHR BX, 1 ; if not, only read signed portion of value (not signed bit)

;ContSetAngle:  ; now, mod angle with MAX_ANGLE to make sure it's less than
              ; upper bound
  MOV AX, BX     ; move angle into AX to prepare to make sure it's in bounds
  MOV DX, 0      ; initialize DX for division
  CWD
  MOV CX, MAX_ANGLE
  DIV CX ; take the MOD of angle and MAX_ANGLE to ensure that
                ; speed is within appropriate bounds// remainder of angle /
                ; MAX_ANGLE is stored in DX
  MOV AX, DX
  ADD AX, 360
  MOV DX, 0      ; initialize DX for division
  CWD
  MOV CX, MAX_ANGLE
  DIV CX ; take the MOD of angle and MAX_ANGLE to ensure that
                ; speed is within appropriate bounds// remainder of angle /
                ; MAX_ANGLE is stored in DX
  MOV driveAngle, DX


InitComputePulseWidths:
   MOV BX, 0 ; initializing index used to access elements of pulseWidths array
            ; indicates the motor that we are looking at

HorizontalForce:
  PUSH BX ; save index
  SHL BX, 1 ; multiplies index by 2
    
  MOV CX, CS:MotorForcesX_Table[BX] ; get horizontal force component of motor

  MOV BX, driveAngle ; set driveAngle as index in table to look at
  SHL BX, 1 ; multiplies driveAngle by 2
  
  MOV AX, CS:Cos_Table[BX] ; get cos(driveAngle)
  
  IMUL CX ; multiply AX (cos(driveAngle)) by BX (force value) to get 
          ; horizontal force
		  
  MOV AX, DX
  
VerticalForce:
  POP BX ; get saved pulseWidth array index (current motor we are looking at)
  PUSH AX ; store value of horizontal force
  PUSH BX ; save it before change BX
  SHL BX, 1 ; multiplies index by 2  

  MOV CX, CS:MotorForcesY_Table[BX] ; get vertical force component of motor

  MOV BX, driveAngle ; set driveAngle as index in table to look at
  SHL BX, 1 ; multiplies driveAngle by 2
 
  MOV AX, CS:Sin_Table[BX] ; get sin(driveAngle)
    
  IMUL CX ; multiply AX (sin(driveAngle)) by BX (force value) to get 
          ; horizontal force
  MOV CX, DX; move force * sin(driveAngle), which is in DX, into BX
  
ForceToPulseWidth:
  POP BX ; get original index
  POP AX ; get horizontal power component
  ADD CX, AX ; add horizontal and vertical force components
  
  MOV AX, driveSpeed
  SHR AX, 1 ; scale speed down so would be positive
  
  MOV DX, 0
  IMUL CX ; multiply total force by speed to get pulseWidth
                          ; (power associated with motor)

  ;POP BX ; get index of pulseWidth (current motor whose pulse width is being
         ; computed)

  SAL DX, 2
  
  MOV pulseWidths[BX], DH ; set pulseWidth of motor BX

ContinueComputation:
  MOV AX, NUM_MOTORS
  INC BX; increment index (to increment BX+1-th element in pulseWidths
         ; array in next loop)
  CMP BX, AX ; check if have reached end of array (are NUM_MOTORS
                         ; elements in the pulseWidths array)
  JL HorizontalForce ; if have not reached end, loop back to continue
                         ; initializing elements in pulseWidths array
  ; JGE EndSetMotorSpeed ; if have reached end, can return

EndSetMotorSpeed:
  RET

SetMotorSpeed	ENDP

; GetMotorSpeed
;
; Description: The function is called with no arguments and returns the current
; speed setting for the RoboTrike in AX. A speed of 65534 indicates
; maximum speed and a value of 0 indicates the RoboTrike is stopped.
; The value returned will always be between 0 and 65534.
;
; Operation: This is done by storing the shared variable driveSpeed in AX.
;
;
; Arguments: None.
; Return Value: AX, register that holds the speed of the RoboTrike
;
; Local Variables: None.
; Shared Variables: 1) driveSpeed, W, type = word, length = 16 bits; driveSpeed
;                    is the variable describing the speed of the RoboTrike,
;                    modified to fit the bounds (0, 65535)
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
; Last Modified: 11/11/16
;

GetMotorSpeed      PROC        NEAR
        PUBLIC      GetMotorSpeed

MOV AX, driveSpeed
RET

GetMotorSpeed	ENDP


; GetMotorDirection
;
; Description: The function is called with no arguments and returns the current
; direction of movement setting for the RoboTrike as an angle in degrees in
; AX. An angle of 0 indicates straight ahead relative to the RoboTrike
; orientation. Angles are measured clockwise. The value returned will always
; be 0 and 359 inclusively.
;
; Operation: This is done by storing the shared variable
; driveAngle in AX.
;
; Arguments: None.
; Return Value: AX, register that holds the angle of the RoboTrike
;
; Local Variables: None.
; Shared Variables: 1) driveAngle, W, type = words, length = 16 bits; driveAngle
;                    is the variable describing the direction of the RoboTrike,
;                    modified to fit the bounds (0, 359)
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
; Last Modified: 11/11/16
;

GetMotorDirection      PROC        NEAR
        PUBLIC      GetMotorDirection

MOV AX, driveAngle
RET

GetMotorDirection	ENDP


; SetLaser
;
; Description: This function is passed a single argument (onoff) in AX that
; indicates whether to turn the RoboTrike laser on or off. A zero value turns
; the laser off and a non zero value turns it on.
;
; Operation: This is done by storing the value of AX in a shared variable
; laserStatus. This variable is 0 if the laser should be off and 1 if the
; laser should be on.
;
; Arguments: AX, register that holds the onoff argument to determine whether
;            the laser should be on or off.
; Return Value: None.
;
; Local Variables: None.
; Shared Variables: 1) laserStatus, W, type = bytes, length = 8 bits; laserStatus
;                    is the variable describing whether the laser is on or
;                    off
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
;set
; Registers Changed: None.
; Stack Depth: None.
;
; Limitations: None.
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/11/16
;

SetLaser      PROC        NEAR
        PUBLIC      SetLaser

CMP AL, 0
JNE LaserOn
MOV laserStatus, 0
JMP EndSetLaserStatus

LaserOn:
	MOV laserStatus, 1

EndSetLaserStatus:
	RET

SetLaser	ENDP


; GetLaser
;
; Description: The function is called with no arguments and returns the status
; of the RoboTrike laser in AX. A value of 0 indicates the laser is off and a
; non zero value indicates that the laser is on.
;
; Operation: This is done by setting AX equal to the value of the shared
; variable laserStatus (which is 0 if the laser is off and 1 if the laser is
; on).
;
; Arguments: None.
; Return Value: AX, register that holds the value to determine whether
;            the laser should be on or off.
;
; Local Variables: None.
; Shared Variables: 1) laserStatus, W, type = bytes, length = 8 bits; laserStatus
;                    is the variable describing whether the laser is on or
;                    off
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
; Last Modified: 11/11/16
;

GetLaser      PROC        NEAR
        PUBLIC      GetLaser
        
MOV AX, 0
MOV AL, laserStatus
RET

GetLaser	ENDP


; PWM_Function
;
; Description: This function determines which motors should be turned on
; and off based on the value of the pulseWidthCounter, which starts at 0
; and is incremented each time this function is called -- this determines
; where in the duty cycle we are. If the motor's pulse width (which
; is calculated in setMotorSpeed) is greater than the pulseWidthCounter,
; that motor is on. If it's not, that motor is off. A signal is sent
; to the ports at the end of the function to tell the motors if they
; on or off.
;
; Operation: This is done by comparing the pulse widths
; of each motor (stored in the shared variable pulseWidths) to the
; pulseWidthCounter. If the pulse width of a motor is greater than
; pulseWidthCounter, that motor is on. [NEED TO KNOW HOW TO DEAL WITH
; DIRECTION!?!?! ]
;
; Arguments: None.
; Return Value: None.
;
; Local Variables: OverallSignal
; Shared Variables: 1) pulseWidths[], W, type = word, length = (16 bits) *
;                     NUM_MOTORS; pulseWidths is an array holding the power
;                     delivered to each motor. The index of pulseWidth
;                     corresponds to the motor, and pulseWidth[index]
;                     corresponds to the power delivered to that motor.
;                   2) pulseWidthCounter, W, type = word, length = 16 bits;
;                     pulseWidthCounter is a counter called each time
;                     the PWM Event Handler Function is called --  it keeps
;                     track of how many motor time interrupts have occurred
;                     since the start of the duty cycle, and thus which motors
;                     to turn on and off.
;                   3) overallSignal, W, type = byte, length = 8 bits;
;                     this variable describes the signal sent to the motors
;                     telling them if they are on/ off and the direction
;                     that they are travelling in.
; Global Variables: None.
;
; Input: None.
; Output: Parallel ports (To motors/ lasers)
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
; Last Modified: 11/11/16
;

PWM_Function      PROC        NEAR
        PUBLIC      PWM_Function
; Overall signal has 7 bits
;

InitPWM:
   MOV BX, 0
   MOV overallSignal, 0
  ;MOV AX, 0 ; initializing index used to access elements of pulseWidths array
            ; indicates the motor that we are looking at
    
Direction:
  MOV DX, 0
  PUSH BX
  MOV AX, 0
  MOV AL, pulseWidths[BX]
  CMP AX, 0
  JGE CheckMotorStatus
  MOV DX, 1

CheckMotorStatus: 
  MOV AL, pulseWidths[BX]
  CMP AX, pulseWidthCounter
  JLE WrapPulseWidthCounter
  
  POP BX
  MOV AX, BX ; put index into AX
  
  PUSH BX
  
  MOV BX, offset(MotorOnOff)
  SHR AX, 1 ; divde AX by 2 before continuing
  XLAT CS:MotorOnOff
  OR overallSignal, AL  ; write 1 to port indicating motor is on
  
  POP BX
  MOV AX, BX ; put index back into AX
  PUSH BX

DirectionCheck:  
  CMP DX, 1
  JNE WrapPulseWidthCounter
  SHR AX, 1 ; divde AX by 2 before continuing
  MOV BX, offset(MotorDirection)
  XLAT CS:MotorDirection
  OR overallSignal, AL  ; write 1 to port indicating motor is on
  
WrapPulseWidthCounter:
  INC pulseWidthCounter
  CMP pulseWidthCounter, MAX_PULSE_WIDTH  ; check if have reached end of duty
                                          ; cycle (where pulseWidthCounter =
                                          ; MAX_PULSE_WIDTH)
  JL ContinuePWM ; if we have not, continue
  MOV pulseWidthCounter, 0 ; if we have, set pulseWidthCounter to 0 again
                           ; (for wrapping purposes)

ContinuePWM:
  MOV AX, NUM_MOTORS
  INC BX; increment index (to increment BX+1-th element in pulseWidths
         ; array in next loop)
  CMP BX, AX ; check if have reached end of array (are NUM_MOTORS
                         ; elements in the pulseWidths array)
  JL Direction ; if have not reached end,
  ; JGE EndPWM ; if have reached end, can return

AddLaser:
  CMP laserStatus, 0
  JE NoLaser
  OR overallSignal, INIT_SIGNAL
  JMP SendSignal

NoLaser:
  AND overallSignal, 01111111B
	
SendSignal:
  MOV AL, overallSignal
  MOV DX, PORT_ADDRESS
  OUT DX, AL

EndPWM: 
  RET

PWM_Function	ENDP


CODE    ENDS


;the data segment

DATA    SEGMENT PUBLIC  'DATA'

driveSpeed          DW      ?   ; overall speed of RoboTrike
driveAngle          DW      ?   ; overall direction RoboTrike is travelling in

laserStatus         DB      ?   ; whether laser is on or off

; an array that contains the power delivered to each motor
pulseWidths         DB  (NUM_MOTORS + 1)  DUP (?)

pulseWidthCounter   DW      ?   ; counter that  keeps track of how many motor
                                ; time interrupts have occurred
                                ; since the start of the duty cycle
overallSignal       DB    ?     ; signal telling the motors if they are
                                ; on and off, and if they are, what
                                ; direction they're travelling in

index			    DW 		?								
DATA    ENDS


END
