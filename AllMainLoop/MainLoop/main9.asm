        NAME    MAIN9

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                               REMOTE MAIN LOOP                             ;
;                                  EE/CS  51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description: (more details in functional specification) This is the
; main loop for the remote (keypad/ display) unit, the keypad user interface, the
; serial interface, and any error handling that needs to be done in the main loop
; (serial port errors, key entry errors, etc.) All errors are reported
; to the user. In this main loop, the stack and data segment are set up, 
; and so are the addresses/ values related to the keypad, display, and serial
; port so that they can be used. Furthermore, interrupts are enabled
; (both timer interrupts to be used by the keypad and display, and 
; INT2 interrupts used by the serial port). Finally, this function
; initializes an EventQueue, which is used to keep track of keypad
; and serial events that are occurring. Then, the main
; loop continues to dequeue from the event queue if event exist to dequeue. 
; The dequeueing function includes calling a call table to call functions
; to deal with events that are dequeued. 
; 
; Input:            The input of this program is from the serial port
;                   via the motors and from the keypad.
; Output:           This program outputs to the display and to the serial
;                   port. 
;
; User Interface:   The user interface of this program is the keypad/ display.
;                   Users can see what commands they send to the motor
;                   when they press the keys over the display (they
;                   send commands to the motor via the key pad)
; Error Handling:   Display error messages for serial/parsing errors. 
;
; Algorithms:       Muxing in display. 
; Data Structures:  EventQueue
;
; Known Bugs:       None.
; Limitations:      Displaying update string right after displaying string related to
;                   key press. 
;
; Revision History: Meera Krishnamoorthy 12/3/16    wrote code
;                   Meera krishnamoorthy 12/4/15    debugged
;                   Meera krishnamoorthy 12/5/15    debugged/commented

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
        EXTRN   ClrIRQVectors:NEAR
        EXTRN   InitTimer:NEAR
        EXTRN   InitEventQueue:NEAR
        EXTRN   InitKeypad:NEAR
        EXTRN   InitSerial:NEAR
        EXTRN   InitINT2:NEAR
        EXTRN   InstallKeypadDisplayHandler:NEAR
        EXTRN   Install_Serial_Handler:NEAR
        EXTRN   DequeueEvent:NEAR
        EXTRN   InitDisplay:NEAR
        EXTRN   GetKeyPress:NEAR
        EXTRN   GetError:NEAR
        EXTRN   GetReceivedData:NEAR
        EXTRN   CheckCriticalFlag:NEAR
        
        EXTRN   InitRemoteFunct:NEAR
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
                
                CALL InitKeypad ; initializes keypad for use
                CALL InitSerial ; initializes serial port for use
                CALL InitDisplay ; initializes display for use
                
                CALL InitRemoteFunct ; sets up shared variables used in
                                     ; functions made for the remote unit
                                     ; main loop
                
                CALL InstallKeypadDisplayHandler ; installs keypad/ display
                                                 ; event handler that
                                                 ; calls functions to check
                                                 ; the keypad/ display
                                                 ; for input/ output
                                                 ; when the timer tells it to
                                                 
                CALL Install_Serial_Handler ; installs the serial event handler
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
                    CALL DequeueEvent ; dequeues element of event queue
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
