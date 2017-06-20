        NAME    MAIN10

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                MOTOR MAIN LOOP                             ;
;                                  EE/CS  51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (more details in functional specification)
; Description: 		This program is the main loop for the motor unit. It
; 					handles the serial interface and motors. 
; 					All errors are reported to the user via
; 					the serial port (they will be displayed by the remote
; 					unit. In this main loop, the stack and data segment are set 
; 					up, and so are the addresses/ values related to the parser, 
; 					motors, and serial port so that they can be used. 
; 					Furthermore, interrupts are enabled (the timer interrupts 
; 					to be used by the motor and INT2 interrupts 
; 					used by the serial port). Finally, this function
; 					initializes an EventQueue, which is used to keep track of 
;					serial events that are occurring. Then, the main
; 					loop continues to dequeue from the event queue if
; 					event exist to dequeue. The dequeueing function
; 					includes calling a call table to call functions
; 					to deal with events that are dequeued. 
; 
; Input:            The input of this program is from the serial port
;                   via the remote unit (keypad presses). 
;
; Output:           This program outputs to to the serial port based
; 					after changing motor speed/direction/laser status
;					or after encountering a serial/parsing error. This
; 					output describes either the changed RoboTrike status
; 					or the error that occurred. This program also outputs
; 					to the RoboTrike itself, changing its speed, direction, 
; 					and laser status. 
;
; User Interface:   None. 
; Error Handling:   Send strings when motor 
;
; Algorithms:       PWM motion for motors
; Data Structures:  Event Queue
;
; Known Bugs:       None.
; Limitations:      No feedback.
;
; Revision History: Meera Krishnamoorthy 12/8/16    wrote code, debugged

;

;definitions

CGROUP  GROUP   CODE
DGROUP  GROUP   DATA, STACK
     
CODE    SEGMENT PUBLIC 'CODE'
		ASSUME  CS:CGROUP, DS:DGROUP


$INCLUDE(MAIN9.inc)
$INCLUDE(queues.inc)
 
;external function declarations

        EXTRN   InitCS:NEAR ; initializes chip select
        EXTRN   ClrIRQVectors:NEAR ; initializes illegal event handler
        EXTRN   InitTimer:NEAR ; initializes timer used to time
                               ; PWM for motors
                               
        ; event/ event queue related initializations
        EXTRN   InitEventQueue:NEAR
        EXTRN   DequeueEventMotor:NEAR
        EXTRN   GetError:NEAR
        EXTRN   GetReceivedData:NEAR
        EXTRN   CheckCriticalFlag:NEAR
		
        
        ; serial related initializations
        EXTRN   InitSerial:NEAR
        EXTRN   InitINT2:NEAR
        EXTRN   Install_Serial_Handler:NEAR
		
       
        ;motor related initializations
		EXTRN	InitMotor:NEAR
		EXTRN	Install_PWM_Handler:NEAR
        EXTRN   InitMotorFunct:NEAR
        
        ; parser related functions
        EXTRN   InitParser:NEAR
		
        START:

        MAIN:
                CLI
                MOV     AX, STACK               ;initialize the stack pointer
                MOV     SS, AX
                MOV     SP, OFFSET(TopOfStack)

                MOV     AX, DGROUP                ;initialize the data segment
                MOV     DS, AX
        StartMainLoop:
                ; create function to initialize buffer
                CALL InitEventQueue ; creates event queue
                CALL InitCS ; initializes chip selects
                CALL ClrIRQVectors ; sets up illegal event handler
                CALL InitTimer ; initializes timer that controls
                               ; motor and display interrupts
                CALL InitINT2  ;initialize interrupts via int2
                
                CALL InitSerial ; initializes serial port for use

				CALL InitMotor ; initializes variables used to run motor
                               ; functions
                
                CALL InitMotorFunct ; sets up shared variables used in
                                     ; functions made for the motor unit
                                     ; main loop
                
                                                 
                CALL Install_Serial_Handler ; installs the serial event handler
				
                CALL InitParser ; initializes parser
				CALL Install_PWM_Handler ; initializes event handler for motor
				
                STI ; turns on interrupts so the event handlers can run
                
                CheckRestart:
                    CALL CheckCriticalFlag ; the critical flag is set if the
                                           ; event queue is full
                                           ; this function gets the value
                                           ; of the critical flag and stores it
                                           ; in AX
                    CMP AX, CRITICAL_FLAG ; if AX is the value the critical
                                          ; flag is set to when 
                    JNE GetNewElement ; if the critical flag is not set,
                                      ; then we can dequeue an element
                                      ; of the event queue, which
                                      ; will tell us what type of event
                                      ; (with what value to process)
                Restart:
                    MOV AX, 0
                    JMP StartMainLoop ; reinitializes everything   
                GetNewElement:
                    CALL DequeueEventMotor ; dequeues element of event queue
                                      ; if the queue is not empty,
                                      ; and uses a call table to proceed
                                      ; based on event
                EndMain:
                    JMP CheckRestart  ; main loop continues
                                      ; loop, checking if the event queue
                                      ; is full, and if not, dequeues
                                      ; events of the queue
                    

CODE ENDS

; initialized because used later on
DATA    SEGMENT PUBLIC  'DATA'
   

DATA    ENDS


;the stack

STACK   SEGMENT STACK  'STACK'

                DB      80 DUP ('Stack ')       ;240 words

TopOfStack      LABEL   WORD

STACK   ENDS




END     START
