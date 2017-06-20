NAME    MOTORS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   Motors                                   ;
;                                  EE/CS 51                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; WHAT I CHANGED on 12/9
; 1) in InitMotor: when writing to the control port, use OUT DX, AL instead of
; OUT DX, AX
; 2) clarified in my GetLaser function that we only needed to check lower byte
; of the laser status to determine if the laser should be on
; 
; Overall Description: These are the functions to control the DC motor and laser
; for the Robotrike.

; Table of Contents
; 1)    InitMotors(): initializes variables needed to determine if motors are on
;       or off and run all other functions
; 2)	SetMotorSpeed(speed, angle): sets the speed of the RoboTrike to speed,
;       with MAX_SPEED - 1 being the full speed; and set the direction of movement to
;       angle in degrees, with 0 degrees being straight ahead.
; 3)	GetMotorSpeed(): gets the current speed setting for the RoboTrike
; 4)	GetMotorDirection(): gets the current direction of movement setting for
;       the RoboTrike
; 5)	SetLaser(onoff): turn on (onoff is non zero) or turn off (onoff is 0) the
;       laser. Onoff is a Boolean variable
; 6)	GetLaser(): get the current laser status (on or off)
; 7)    PWM_Function(): determines whether motor is on/ off based on location
;       in duty cycle

; All shared variables: driveSpeed, driveAngle, laserStatus, pulseWidths[],
; pulseWidthCounter, overallSignal
;
; Revision History:
;    11/11/16  Meera Krishnamoorthy   wrote code
;    12/9/16   Meera Krishnamoorthy   edited code


CGROUP  GROUP   CODE
DGROUP  GROUP   DATA


CODE	SEGMENT PUBLIC 'CODE'


ASSUME  CS:CGROUP, DS: DGROUP


EXTRN   Sin_Table:WORD       ; table describing sine values
							 ; for all angles from 0 to 356 degrees
							 
EXTRN   Cos_Table:WORD       ; table describing cosine values
							 ; for all angles from 0 to 356 degrees

EXTRN   MotorForcesX_Table:WORD      ; table showing horizontal force
                                     ; associated with each motor
                                     
EXTRN   MotorForcesY_Table:WORD       ; table showing vertical force
                                     ; associated with each motor

EXTRN   MotorOnOff:BYTE          ; table writing to appropriate bit
								 ; if motor corresponding to table index
								 ; is on
								 
EXTRN   MotorDirection:BYTE      ; table writing to appropriate bit
								 ; if motor corresponding to table index
								 ; is moving backwards
$include(motors.inc)

; InitMotor
;
; Description: This function initializes all the shared variables used in
;              functions concerning the motors, and also initializes
;              the parallel port. 
;
; Operation: This is done by setting all shared variables created in the
; data segment to zero, including all used elements in the pulseWidths array.
; It also outputs to the control port a value that makes sure that port B
; is an output port so it can be outputted to. 
;
; Arguments: None.
; Return Value: None.
;
; Local Variables: None.
; Shared Variables: 1) driveSpeed, W, type = word, length = 16 bits; driveSpeed
;                    is the variable describing the speed of the RoboTrike
;                   2) driveAngle, W, type = words, length = 16 bits; driveAngle
;                    is the variable describing the direction of the RoboTrike
;                   3) laserStatus, W, type = bytes, length = 8 bits; laserStatus
;                    is the variable describing whether the laser is on or
;                    off
;                   4) pulseWidths[], W, type = byte, length = (8 bits) *
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
; Data Structures: array (pulseWidths[])
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

  MOV DX, CONTROL_PORT ; initial port determining use of ports A, B, C
  MOV AL, INIT_SIGNAL ; initial signal indicating port B should be for
					  ; output

  OUT DX, AL ; write initial signal to CONTROL_PORT indicating
             ; that port B is an output port

  MOV BX, 0  ; clear out BX (will be used as an index to access elements
             ; in pulseWidths array, starting from the 0th element, so
             ; it is necessary for BX to start as 0)

  MOV AX, NUM_MOTORS 

InitPulseWidthArray:
  MOV pulseWidths[BX], 0 ; initialize BX-th element in pulseWidths array
  INC BX
  CMP BX, AX
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
; A speed of MAX_SPEED indicates the current speed should not be changed,
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
; -32768. The driveSpeed variable should be between 0 and 65535. 
; This function has to change the passed in angle before storing it in driveAngle.
; The passed in angle (in BX) will be between -32767 and 32767, and driveAngle
; must be between 0 and 359; thus, the angle needs to be normalized. An angle
; of -32768 indicates the current direction of travel should not be changed.
; Thus, -32767 corresponds to
; an angle of 1. To change the bounds from (-32767, 32767) to (0, 359), we
; take the MOD of BX with 360, add 360 to that value (in case it's negative),
; and take the MOD of that value again. 
; Then, the function stores AX and the normalized BX in shared variables
; (driveSpeed and driveAngle). After it does this, the function
; calculates the power delivered to each motor. The power is delivered
; to each motor thusly: F1 = [1, 0], F2 = [(-1/2), -sqrt(3)/2],
; F3 = [(1/2), -sqrt(3)/2]. The negative signs indicate the angle of the
; motor from the horizontal/ vertical axis. These values exist
; in a lookup table. 
; The general formula for calculating the power delivered to each motor is
; s_n = F_n_x *  v * cos(theta) + F_n_y * v * sin(theta), where v is 
; speed and theta is the direction of the RoboTrike. We
; get the sin(theta) and cos(theta) values from a lookup table as well. These powers
; are then stored in a shared array (pulseWidths[]), and are used
; in the PWM event handler to determine which motors to turn on and off.

; Arguments: AX (speed), the speed of the RoboTrike before modified to
;             fit bounds (0, 65534)
;            BX (angle), the direction the RoboTrike moves in before modified
;             to fit bounds (0, 359);
;
; Return Value: None.
;
; Local Variables: None.
; Shared Variables: 1) driveSpeed, R, type = word, length = 16 bits; driveSpeed
;                    is the variable describing the speed of the RoboTrike,
;                    modified to fit the bounds (0, 65535). Possible values
;                    of driveSpeed are 0 - 65535.
;                   2) driveAngle, R, type = words, length = 16 bits; driveAngle
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
; Data Structures: Trig_Table, MotorForces_Table, array (pulseWidths[])
;
;
; Registers Changed: None.
; Stack Depth: 2 words
;
; Limitations: None.
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/11/16
;

SetMotorSpeed      PROC        NEAR
        PUBLIC      SetMotorSpeed

SetSpeed: ; speed is stored in AX
  CMP AX, MAX_SPEED ; if speed is equal to MAX_SPEED, should not change
                    ; driveSpeed, can jump to check if angle should be set.
  JE SetAngle
  MOV driveSpeed, AX ; if speed != MAX_SPEED, can change value of
					 ; shared variable driveSpeed


SetAngle: ; angle is stored in BX
  CMP BX, MIN_ANGLE ; if angle is equal to MAX_ANGLE, should not change
                    ; speed, can jump to computing pulse widths
  JE InitComputePulseWidths

  MOV AX, BX     ; move angle into AX to prepare to make sure it's in bounds
				 ; (will need to divide to take MOD and AX is always involved
				 ; in divisions)
  MOV DX, 0      ; initialize DX for division
  CWD			 ; further initialize DX for signed division
  MOV CX, MAX_ANGLE 
  IDIV CX ; take the MOD of angle and MAX_ANGLE to ensure that
                ; speed is within appropriate bounds// remainder of angle /
                ; MAX_ANGLE is stored in DX
  MOV AX, DX    
  ADD AX, MAX_ANGLE ; (add the maximum value that AX could possibly be
					; so that we are absolutely sure AX is not < 0
  MOV DX, 0    	; initialize DX for division
  CWD			; further initialize DX for signed division
  MOV CX, MAX_ANGLE
  IDIV CX ; take the MOD of angle and MAX_ANGLE to ensure that
                ; speed is within appropriate bounds// remainder of angle /
                ; MAX_ANGLE is stored in DX
  MOV driveAngle, DX ; store this normalized angle in the shared variable
					 ; driveAngle


InitComputePulseWidths:
   MOV BX, 0 ; initializing index used to access elements of pulseWidths array
             ; indicates the motor that we are looking at

HorizontalForce:
  PUSH BX ; save index used to access elements of pulseWidths array
  
  SHL BX, BYTE_TO_WIDTH_CONV ; multiplies BX by 2*BYTE_TO_WIDTH_CONV
							 ; because need to access value in 
							 ; table below that corresponds to current
							 ; motor we are looking at, which is the index
							 ; of the pulseWidths array. however, this array
							 ; is in bytes and the table is words. to convert
							 ; the index to access the word table, we
							 ; shift the index register right, which
							 ; multiplies the index by 2*BYTE_TO_WIDTH_CONV
    
  MOV CX, CS:MotorForcesX_Table[BX] ; get horizontal force component of motor

  MOV BX, driveAngle ; set driveAngle as key to look up in next table
  SHL BX, BYTE_TO_WIDTH_CONV ; multiplies driveAngle by 2*BYTE_TO_WIDTH_CONV
							 ; for same reason as above (want to search
							 ; in word table). 
  
  MOV AX, CS:Cos_Table[BX] ; get cos(driveAngle)
  
  IMUL CX ; multiply AX (cos(driveAngle)) by CX (force value) to get 
          ; horizontal force
		  
  MOV AX, DX ; the most significant bits of this multiplication is stored in
			 ; in DX, so take DX and store it in AX (where we will be storing
			 ; the horizontal force value)
  
VerticalForce:
  POP BX ; get saved pulseWidth array index (current motor we are looking at)
  PUSH AX ; store value of horizontal force
  PUSH BX ; save index value before shifting it
  SHL BX, BYTE_TO_WIDTH_CONV ; multiplies BX by 2*BYTE_TO_WIDTH_CONV
							 ; for same reason as above (want to search
							 ; in word table). 

  MOV CX, CS:MotorForcesY_Table[BX] ; get vertical force component of motor

  MOV BX, driveAngle ; set driveAngle as key to look up in next table
  SHL BX, BYTE_TO_WIDTH_CONV ; multiplies driveAngle by 2*BYTE_TO_WIDTH_CONV
							 ; for same reason as above (want to search
							 ; in word table). 
 
  MOV AX, CS:Sin_Table[BX] ; get sin(driveAngle)
    
  IMUL CX ; multiply AX (sin(driveAngle)) by CX (force value) to get 
          ; vertical force
  MOV CX, DX ; the most significant bits of this multiplication is stored in
			 ; in DX, so take DX and store it in CX (where we will be storing
			 ; the vertical force value)
  
ForceToPulseWidth:
  POP BX ; get saved pulseWidth array index (current motor we are looking at)
  POP AX ; get horizontal power component
  ADD CX, AX ; add horizontal and vertical force components
  
  MOV AX, driveSpeed ; move driveSpeed into register to prepare to 
					 ; multiply it to force values
  SHR AX, 1 				 ; scale speed down so will be positive
							 ; current speed bounds = (0, 65535). by shifting
							 ; this value by 1, we are dividing this value by
							 ; 2, to change the bounds to (0, 32767)
  
  IMUL CX ; multiply total force by speed to get pulseWidth(power associated 
		  ; with motor)
  SHL DX, NUM_SIGNED_BITS ; the most significant bits of this multiplication 
						  ; is stored in DX. DX is the result of three
						  ; multiplied values, so it has three signed bits.
						  ; We want to get rid of NUM_SIGNED_BITS of these
						  ; signed bits, and leave one remaining, so we
						  ; shift DX by NUM_SIGNED_BITS so that the value
						  ; only has 1 signed bit. This remaining signed bit
						  ; will be used to determine if the motor is moving
						  ; forwards or backwards
						  
  MOV pulseWidths[BX], DH ; set pulseWidth of motor BX to DH (most 
						  ; significant part of DH)

ContinueComputation:
  MOV AX, NUM_MOTORS ; set AX to maximum number of values that should be
					 ; in pulseWidths array
  INC BX; increment index (to increment BX+1-th element in pulseWidths
         ; array in next loop)
  CMP BX, AX ; check if have reached end of array (are NUM_MOTORS
                         ; elements in the pulseWidths array)
  JL HorizontalForce ; if have not reached end, loop back to continue
                         ; calculating pulse widths of motors
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

MOV AX, driveSpeed ; get stored speed of motor and store in AX
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

MOV AX, driveAngle ; get stored angle of motors' travel and store in AX
RET

GetMotorDirection	ENDP


; SetLaser
;
; Description: This function is passed a single argument (onoff) in AX that
; indicates whether to turn the RoboTrike laser on or off. A zero value turns
; the laser off and a non zero value turns it on.
;
; Operation: This is done by storing the value of AX in a shared variable
; laserStatus. This variable is 0 if the laser should be off and nonzero if the
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

CMP AX, 0 ; check if laser status is 0 (if it is, laser should be off)
JE OffLaser ; turn laser off
MOV laserStatus, 1 ; move nonzero value into laserStatus if laser should be on
JMP EndSetLaserStatus

OffLaser:
    MOV laserStatus, AL ; set value of laserStatus (which determines whether
                        ; the laser should be on or off) to whatever is in
                        ; AL. 

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
; Shared Variables: 1) laserStatus, R, type = bytes, length = 8 bits; laserStatus
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
        
MOV AX, 0 ; laserStatus is a byte, so we only move laserStatus into AL.
		  ; however, we don't want AH to be something random, so we
		  ; initialize all of AX to 0. 
MOV AL, laserStatus ; move value of laserStatus into AL (laserStatus
					; describes if the laser is on or off)
                    ; we know this works because we put a byte value into laser status
                    ; when the status is on, not a word, so the important
                    ; part of laserStatus is a byte
RET

GetLaser	ENDP


; PWM_Function
;
; Description: This function determines which motors should be turned on
; and off based on the value of the pulseWidthCounter, which starts at 0
; and is incremented each time this function is called. PulseWidthCounter determines
; where in the duty cycle we are. If the motor's pulse width (which
; is calculated in setMotorSpeed) is greater than the pulseWidthCounter,
; that motor is on. If it's not, that motor is off. This function also deter-
; mines the direction in which a motor should travel if it's on -- the
; direction the motor travels is determined by the value of its pulse width.
; If the pulse width is negative, the motor is travelling backwards, and if
; it's positive, the motor is moving forward. An overall signal (in
; the shared variable overallSignal) is sent
; to the motors at the end of the function telling each motor if it's on
; or off, and if it's on or off, what direction it is travelling in. This signal
; also contains information related to the laser, specifically, a bit that
; is set if the laser is on or off. 

; Operation: This is done by comparing the pulse widths
; of each motor (stored in the shared variable pulseWidths) 0. If the 
; value is less than 0, then that motor should be moving backwards 
; if it is on. To determine if its on, we negate the value of the pulse
; width if it is negative, and compare the pulse widths to the pulse width
; counter shared variable. If the pulse width of the motor is greater
; than the pulseWidthCounter, that motor is on. We can then send a signal
; to that motor (determined by bit patterns in look up tables MotorOnOff
; and MotorDirection) telling it if it is on, and if it is on, what 
; direction it is travelling in. Each motor corresponds to two bits; one 
; bit determines whether it is on or off, and the other determines the direction.
; The direction bit should only be set if the motor on bit is set. 
; After the bit pattern have been determined for all three motors, 
; a bit pattern is added to the overall bit pattern to indicate whether 
; the laser should be on or off. 
;
; Arguments: None.
; Return Value: None.
;
; Local Variables: OverallSignal
; Shared Variables: 1) pulseWidths[], W, type = byte, length = (8 bits) *
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
; Output: Parallel ports (to motors/ lasers)
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

PWM_Function      PROC        NEAR
        PUBLIC      PWM_Function

InitPWM:
   MOV BX, 0 ; initializing index used to access elements of pulseWidths array
             ; indicates the motor that we are looking at
   MOV overallSignal, 0 ; initialize variable that will hold signal to send to
						 ; parallel ports at end of function

Direction:
  MOV DX, 0 ; clear DX (saving values to register later)
  PUSH BX ; save value of index while performing all these functions
  MOV AX, 0 ; initialize variable used to store pulsewidth (since pulsewidth
			; is a byte array)
  MOV AL, pulseWidths[BX] ; move pulsewidth array value to AL
						  ; so that if it needs to be negated
						  ; before its comparison with pulseWidthCounter
						  ; (if it's negative), it can be 
						  ; without changing the original pulseWidths value
  CMP AL, 0; compare pulsewidth to 0
  JGE CheckMotorStatus ; determine if pulsewidth value is negative, and therefore,
			; if motor associated with pulsewidth is moving backwards
			; if not negative, can move on
  MOV DX, MOVE_BACKWARDS ; if is negative, set DX to MOVE_BACKWARDS value
						 ; this DX register will continue to store 
						 ; this value which indicates the motor should
						 ; move backwards until the signal indicating
						 ; direction of motor is outputted
  NEG AL ; negate value of pulseWidth so it can be compared to the 
		 ; pulseWidthCounter

CheckMotorStatus: 
  CMP AX, pulseWidthCounter ; compares pulseWidth with pulseWidthCounter
  JLE WrapPulseWidthCounter ; if absolute value of pulseWidth is less than
							; or equal to pulseWidthCounter, motor is off
							; else, motor is ON
   
  POP BX ; get pulseWidth array index (indicates which motor looking at)
  MOV AX, BX ; put index into AX to prepare for table look up in XLAT 
			 ; (which uses AL and BX)
  
  PUSH BX ; save pulseWidth index because BX will be used to hold table
		  ; address
  
  MOV BX, offset(MotorOnOff) ; store the table holding bit patterns
							 ; that indicate what bit should be 
							 ; written to if the motor is on
							 ; in BX 
  XLAT CS:MotorOnOff ; looks up value to write to overall signal
					 ; that indicates that current motor is on
  OR overallSignal, AL  ; adds this value to overall signal
						; use OR because only want to add bit, not 
						; subtract anything from signal
  
  POP BX ; get pulseWidth array index (indicates which motor looking at)
  MOV AX, BX ; put index into AX to prepare for table look up in XLAT 
			 ; (which uses AL and BX)
  PUSH BX ; save pulseWidth index because BX will be used to hold table
		  ; address
  
  CMP DX, MOVE_BACKWARDS ; compare DX register with value indicating
						 ; motor is moving backwards (which it would be
						 ; set to if pulseWidth was negative)
  JNE WrapPulseWidthCounter ; if not negative, move on
  
  ; if negative, add bit pattern to overall signal to indicate
  MOV BX, offset(MotorDirection) ; store the table holding bit patterns
							 ; that indicate what bit should be 
							 ; written to if the motor is moving backwards
							 ; in BX 
  XLAT CS:MotorDirection ; looks up value to write to overall signal
					 ; that indicates that current motor is moving backwards
  OR overallSignal, AL  ; adds this value to overall signal
						; use OR because only want to add bit, not 
						; subtract anything from signal
  
WrapPulseWidthCounter:
  INC pulseWidthCounter ; pulseWidthCounter is incremented each iteration
						; of function (keeps track of location in duty
						; cycle)
  CMP pulseWidthCounter, MAX_PULSE_WIDTH  ; check if have reached end of duty
                                          ; cycle (where pulseWidthCounter =
                                          ; MAX_PULSE_WIDTH)
  JL ContinuePWM ; if we have not, continue
  MOV pulseWidthCounter, 0 ; if we have, set pulseWidthCounter to 0 again
                           ; (for wrapping purposes)

ContinuePWM:
  MOV AX, NUM_MOTORS ; set AX to maximum number of values that should be
					 ; in pulseWidths array
  POP BX
  
  ADD BX, 1 ; increment index (to increment BX+1-th element in pulseWidths
         ; array in next loop)
  CMP BX, AX ; check if have reached end of array (are NUM_MOTORS
                         ; elements in the pulseWidths array)
  JL Direction ; if have not reached end, continue looping to determine
			   ; what to add to overallSignal to represent each motor.
  ; JGE EndPWM ; if have reached end, can return

AddLaser: ; add bit to describe laser's status to overallSignal
  CMP laserStatus, 0 ; if laserStatus is zeroed, laser is off, move on
  JE NoLaser ; check to see if laser is working
  OR overallSignal, INIT_SIGNAL ; if laser is on, add INIT_SIGNAL to
								; overall signal to indicate this	
  JMP SendSignal
  
NoLaser:
  AND overallSignal, 01111111B ; (make sure that top bit is 0 --> that
							   ; represents the laser's status)
	
SendSignal:
  MOV AL, overallSignal
  MOV DX, MOTOR_PORT
  OUT DX, AL ; out uses DX and AL. DX holds address to write to (MOTOR_PORT),
			 ; and AL holds value of signal to write to motor port

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

DATA    ENDS


END
