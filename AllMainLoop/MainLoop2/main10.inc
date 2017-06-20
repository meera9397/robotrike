; MAIN10.INC
; This contains all of the constants needed to run the motor main loop.

; constants to create update string 
SPEED_SIZE EQU 4    ; the maximum number of hexadecimal bytes needed
                    ; to display the speed of the robotrike
                    
ANGLE_SIZE EQU 3    ; the maximum number of hexadecimal bytes needed
                    ; to display the direction (angle) of the robotrike
                    
ASCII_SPACE EQU 32  ; ascii value for space (used in update string
                    ; to separate the speed from the angle)
