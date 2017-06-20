NAME    MFUNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                      FUNCTIONS USED IN MOTOR MAIN LOOP                     ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description: 
; 
; Table of Contents
; 1) InitEventQueue: initializes event queue
; 2) DequeueEvent: dequeues event from event queue
; 3) EnqueueEvent: enqueues event to event queue 
; 4) InitRemoteFunct: initializes shared variables used in the following 
;                     functions


; 6) GetError: displays serial errors that occur/ prevents any more
;              data from being sent serially
; 7) GetReceivedData: displays data received serially from motors
; 8) GetMotorError: displays that parsing error on motor side occurred
; 9) SendSerialPutStringRemote: sends string from GetKeyPress
;                               over serial to motor side
; 10) CheckCriticalFlag: called by the main loop to check if the event queue
;                        is full
; 
; 
; Revision History: Meera Krishnamoorthy 12/3/16    wrote code
;                   Meera krishnamoorthy 12/4/15    debugged
;                   Meera krishnamoorthy 12/5/15    debugged/commented
;
;

CGROUP  GROUP   CODE
DGROUP  GROUP   DATA


CODE	SEGMENT PUBLIC 'CODE'

; used to create/ dequeue/ enqueue from event queue
EXTRN   QueueInit:NEAR
EXTRN   Enqueue:NEAR
EXTRN   Dequeue:NEAR
EXTRN   QueueFull:NEAR
EXTRN   QueueEmpty:NEAR


; used to send values over serial channel
EXTRN   SerialPutChar:NEAR


EXTRN ParseSerialChar:NEAR
EXTRN GetMotorSpeed:NEAR
EXTRN GetMotorDirection:NEAR
EXTRN GetLaser:NEAR


EXTRN Dec2String:NEAR

EXTRN doNOP:NEAR

$INCLUDE(MAIN9.inc)
$INCLUDE(queues.inc)
$INCLUDE(simpmac.inc)
$INCLUDE(display.inc)
$INCLUDE(main10.inc)

ASSUME  CS:CGROUP, DS:DGROUP

; Function Name: InitEventQueue
; Description: This function initializes the event queue using a pre-existing
; function QueueInit. The reason that this function exists is to specify
; the address of the event queue, so that remote functions can access
; the event queue without the event queue having to become a
; global structure.
;
; Operational Description: This function does this by setting up the registers
; appropriately (with the address of the queue in SI, the length of the
; queue in AX, and the element size in BX), and then calling the
; already existing function QueueInit to create a queue structure.
;
; Arguments: AX: length of queue
;            BX: element size
;            SI: address of queue
; Return Values: None.
; Global Variables: None.
; Shared Variables: None.
; Local Variables: None.
;
; Inputs: None.
; Outputs: None.
; User Interface: None.
; Error Handling: None.
; Algorithms: None.
;
; Data Structures: None.
; Limitations: None.
; Known Bugs: None.
; Registers used:
;
; Author: Meera Krishnamoorthy
; Last Modified: 12/5/16
;

InitEventQueue      PROC        NEAR
        PUBLIC      InitEventQueue
        
LEA SI, EventQueue ; sets address of queue to be event queue
MOV AX, EVENTQUEUE_SIZE ; sets queue size
MOV BX, word_len ; sets element size of queue to be words
CALL QueueInit ; calls function to initialize event queue

RET

InitEventQueue	ENDP



; EnqueueEvent
; Description: This function enqueues an event to the event queue, assuming
; that the value to be enqueued is in AX. If the queue is full, it
; sets the critical flag (which is used in the main loop
; to determine whether to reset all values (in the case that the event
; queue is full). 
; The reason that this function exists is to specify
; the address of the event queue, so that remote functions can access
; the event queue without the event queue having to become a global structure.
;
; Operational Description: This function begins by checking if the queue is 
; full. If the queue is full, instead of trying to enqueue to
; it, this function sets the critical flag variable to a special constant
; that will be used in the main loop to determine whether to reset
; the main loop. Then, it puts the address of the event queue in SI and
; the value to be enqueued in AX, and calls the already written function 
; Enqueue to enqueue AX to the event queue. 
;
; Arguments: AX: value to enqueue to the event queue
; Return Values: None.
; Global Variables: None.
; Shared Variables: criticalflag: set if the event queue is full (used to 
;                             check if the main loop has to restart. size: word,
;                             type: W
; Local Variables: None.
;
; Inputs: None.
; Outputs: None.
; User Interface: None.
; Error Handling: None.
; Algorithms: None.
;
; Data Structures: EventQueue: type: words, size: EVENTQUEUE_SIZE, 
;                       holds all key press/ serial events
; Limitations: None.
; Known Bugs: None.
; Registers used: CX, AX, SI
;
; Author: Meera Krishnamoorthy
; Last Modified: 12/5/16
;

EnqueueEvent      PROC        NEAR
        PUBLIC      EnqueueEvent

InitEnqueueEvent:
    MOV CX, AX   ; saves value of AX (value to enqueue to event)
                 ; because queue full changes values of AX
    LEA SI, EventQueue ; sets SI to address of event queue (because it
                       ; is an argument of QueueFull)
    CALL QueueFull ; checks if the queue is full. sets the zero
                   ; flag if the queue is full, and resets it if
                   ; queue is not full
    JNZ CanEnqueue  ; if queue is not full (zero flag is reset), can enqueue
                   ; to the queue
    MOV criticalflag, CRITICAL_FLAG ; if queue is full, should set critical
                                    ; flag (which will be used in main loop
                                    ; to check if we need to restart
                                    ; our system)
    JMP EndEnqueueEvent  ; after setting critical flag, jump to end
    
CanEnqueue:
    MOV AX, CX ; get back value to enqueue to event queue (was saved in
               ; CX at the beginning of this function)
    LEA SI, EventQueue ; sets SI to address of event queue (because
                       ; that is how queue full is called)
    CALL Enqueue ; enqueues to the event queue
    
EndEnqueueEvent:
    RET

EnqueueEvent	ENDP


; Function Name: DequeueEventMotor
; Description: This function dequeues an event from the event queue.
; The reason that this function exists is to specify
; the address of the event queue, so that remote functions can access
; the event queue without the event queue having to become a global structure.
;
; Operational Description: This function does this by first checking if the
; queue is empty. If it is, we do not dequeue, and instead call a function
; that simply returns (to delay return back to
; main loop). If we can dequeue, we dequeue from the event queue,
; and based on the type of event dequeued (which is stored in AH -- the
; event queue is a word queue, which stores event types in AH and event
; values in AL), it calls an appropriate function to deal with that
; error. This is done using the call table MotorTable which is indexed
; by event types. 
;
; Arguments: None.
; Return Values: None.
; Global Variables: None.
; Shared Variables: None.
; Local Variables: None.
;
; Inputs: None.
; Outputs: None.
; User Interface: None.
; Error Handling: None.
; Algorithms: None.
;
; Data Structures:  EventQueue: type: words, size: EVENTQUEUE_SIZE, 
;                       holds all key press/ serial events
;                   MotorTable, type: word, length: 4 elements: is a call
;                       table that is indexed by event type -- calls
;                       the appropriate function to deal with each event
; Limitations: None.
; Known Bugs: None.
; Registers used: SI, flags
;
; Author: Meera Krishnamoorthy
; Last Modified: 12/5/16
;

DequeueEventMotor      PROC        NEAR
        PUBLIC      DequeueEventMotor

InitDequeueEvent:
    LEA SI, EventQueue ; sets SI to address of event queue (it is an   
                       ; argument of QueueEmpty)
    CALL QueueEmpty ; check if the queue is empty
    JNZ CanDequeue ; if queue is not empty, dequeue
                   ; we do this because if the queue is empty and we try
                   ; to dequeue, our code will enter a blocking function
                   ; that it will not be able to get out of.
                   ; thus we must check if the queue is empty before we
                   ; actually dequeue. 
ResetAX: ; if queue is empty, we call a special function
         ; to delay us from re-entering our main loop (which would prevent
         ; faster enqueues). this function is indexed in the remote table
         ; (which calls functions based on the event constant of 
         ; queues) with QUEUE_EMPTY_CONSTANT.
    MOV BX, QUEUE_EMPTY_CONST ; set BX to be QUEUE_EMPTY_CONSTANT
    JMP CallDequeueFunc ; call special function to delay from 
                        ; re-entering main loop
CanDequeue: 
    LEA SI, EventQueue ; sets SI to address of event queue (it is an argument
                       ; of Dequeue)
    CALL Dequeue ; dequeues a value of the event queue
    MOV BX, 0 ; clear upper byte of BX (used to index remote table)
    XCHG BL, AH ; moves the event type into BL so that it can index table
                ; also now high bit of AH is cleared so AL (event value)
                ; which is only necessary information is the only thing
                ; remaining
                
                ; all functions in the Remote Table are called with AL as
                ; an argument, and are indexed by BX

    SHL BL, 1   ; the remote table is a word table, so need to shift
                ; index before calling table
    
CallDequeueFunc:
    CALL CS:MotorTable[BX] ; calls function associated with
                            ; each event type to deal with those events

EndDequeueEventMotor:
    RET

DequeueEventMotor	ENDP




; Function Name: InitRemoteFunct
; Description: This function initializes all shared variables used in the
; functions to handle keypress events and serial events.
;
; Operational Description: This is done by zeroing the values of all the
; shared variables. 
;
; Arguments: None. 
; Return Values: None.
; Global Variables: None.
; Shared Variables: rdindex: indexes the string buffer used to store
;                            received data so we know how many strings
;                            we've received (so we can compare that to the
;                            display length to properly display)
;                            size: word, type: W
;                   rdstringbuffer: holds string sent in from the
;                            serial port (is added to until a carriage
;                            return, then is null terminated and displayed).
;                            size: DISPLAY_LEN, type: B
;                   remoteindex: used to index the string created
;                            in serialputchar because these strings
;                            are all fixed length, and the function
;                            returns once a fixed number of characters
;                            has been sent to the serial port. size: word,
;                             type: W
;                   criticalflag: set if the event queue is full (used to 
;                             check if the main loop has to restart. size: word,
;                             type: W
;                   errorflag: set if a serial error occurs (so no data
;                             can be received from the serial. size: word,
;                             type: B
;                   
; Local Variables: None.
;
; Inputs: None.
; Outputs: None.
; User Interface: None.
; Error Handling: None.
; Algorithms: None.
;
; Data Structures: None.
; Limitations: None.
; Known Bugs: None.
; Registers used:
;
; Author: Meera Krishnamoorthy
; Last Modified: 12/5/16
;

InitRemoteFunct      PROC        NEAR
        PUBLIC      InitRemoteFunct
        
MOV rdindex, 0 ; sets received data buffer index to 0
MOV remoteindex, 0 ; sets index for sending serial strings to 0
MOV criticalflag, 0 ; sets critical flag to 0 
MOV rdstringbuffer, 0 ; sets received data buffer to 0
MOV dispmotorbuffer, 0 ; sets sending motor buffer to 0
MOV errorflag, 0 ; sets the error flag to 0

RET

InitRemoteFunct	ENDP

; GetError
; Description: This function is called whenever a Line Status Interrupt
; occurs (they occur when a serial error happens). Once a 
; Line Status Interrupt occurs, a previously written function
; enqueues an event to the event queue signifying that
; this happened. When that event is dequeued, this
; function is called. This function displays the type of error that 
; occurred using a table that is indexed based on the value of the error
; event (which is the value of the Line Status Register). 
;
; Operational Description: This is done by first setting the shared
; variable errorflag to ERROR_FLAG_VAL so nothing can be sent from the
; serial channel after this error happens. Then, it checks if the value
; of the event (sent in AL) corresponds to an index in the error table. If it 
; does, then we get the string corresponding to that index of the table
; and call Display to display that string (a function in display.asm).
; If it is not, we display that a generic error has occurred, because
; it is not one that is in our table. 
;
; Arguments: AL: value of line status register (tells you what
;            error has occurred)
; Return Values: None.
; Global Variables: None.
; Shared Variables: errorflag: set if a serial error occurs (so no data
;                              can be received from the serial after
;                              error occurs). size: word, type: R/W
; Local Variables: None.
;
; Inputs: None.
; Outputs: None. 
; User Interface: Displays error message on display
; Error Handling: None.
; Algorithms: None.
;
; Data Structures: ErrorTable: type: byte, has strings to display
;                      on the display for each error
; Limitations: The error table does not account for every possible
;              error that could occur, thus we can sometimes display
;              that a generic error occurred.
; Known Bugs: None.
; Registers used: None. 
;
; Author: Meera Krishnamoorthy
; Last Modified: 12/5/16
;

GetError      PROC        NEAR
        PUBLIC      GetError
CheckValidResult: ; check if lsr value is in error table
    MOV errorflag, ERROR_FLAG_VAL ; does this to make sure no received data
                                  ; is displayed
    MOV CL, AL ; move lsr value to CL (because will be modified)
    CMP CL, 0 ; there is no table value corresponding to lsr = "0" so display
              ; that unidentified error occurred
    JE DisplayUnIdentifiedError
    
    CMP CL, MAX_ERROR_TABLE_VAL ; check if lsr value is greater than the number
                                ; of elements in the error table
    JGE DisplayUnIdentifiedError ; there is no table value corresponding to 
                                 ; these values of lsr so display
                                 ; that unidentified error occurred
    ; this checks if the lsr is negative because the lsr should be 
    ; a multiple of 2
    AND CL, 1 ; gets rid of all bytes except the last byte (which would
              ; only be set if a number is odd
    CMP CL, 1 ; checks if the last bit is set 
    JE DisplayUnIdentifiedError ; if last bit is set, then not even, 
                                ; not a valid result
    
FindErrorString:
    SHR AL, 1 ; error table is indexed by bytes (that are a factor of 2 
              ; lower than the lsr value), thus need to divide by 2
              ; to index it
    LEA SI, ErrorTable
    ; AL contains the error 
    MOV AH, 0 ; AL: only important information
              ; make sure AH is 0 before multiplying by DISPLAY_LEN
    MOV BX, DISPLAY_LEN ; moves into BX the length of the display (which
                        ; is the length of all 
    MUL BX
    ADD SI, AX ; now SI contains address of table offset with correct thing
               ; to display
    
    Call SerialPutStringRemote
    
    JMP EndGetError
    
DisplayUnIdentifiedError:
    LEA SI, ErrorTable ; if error is unidentified, display 
                       ; that generic error has occurred (we have set
                       ; the generic error to be the first index
                       ; of the table, so the table address points
                       ; to this string
    
    Call SerialPutStringRemote
    
EndGetError: 
    RET
 
RET

GetError	ENDP


; GetReceivedData
; Description: 
;
; Operational Description: 
;
; Arguments: AL: received character that will be added to the string
;            buffer and displayed once a carriage return is
;            hit
; Return Values: None.
; Global Variables: None.
; Shared Variables: 
; Local Variables: None.
;
; Inputs: None.
; Outputs: None.
; User Interface: None.
; Error Handling: If the error flag is set (a serial error occurred), 
;                 no received data can be displayed. The system
;                 must be reloaded for serial data to be sent to display. 
; Algorithms: None.
;
; Data Structures: None.
; Limitations: 
; Known Bugs: None.
; Registers used:
;
; Author: Meera Krishnamoorthy
; Last Modified: 12/5/16
;

GetReceivedData      PROC        NEAR
        PUBLIC      GetReceivedData
 
IntGetReceivedData: 
    Call ParseSerialChar
    CMP AX, 0
    JE EndGetReceivedData
SendParseError:
    LEA SI, MotorErrors
    Call SerialPutStringRemote
EndGetReceivedData: 
    RET

GetReceivedData	ENDP


; SerialPutStringRemote
; Description: This function sends a series of characters over the serial
; channel from the remote unit to the motor unit. It expects CS:SI to be
; the location of the string to send over serial, and loops through
; SI until it reaches the end of the string (all strings are a fixed length,
; which is how we know we have reached the end of the string). 
;
; Operational Description: This is done using the shared variable remote
; index to keep track of how much of the string has been sent to
; the serial port. The string is stored in CS:SI, and we send characters
; in the string over serial using the already written function
; SerialPutChar. This function continues
; to call SerialPutChar until all characters in the string have been sent
; over (the remote index is set to be the length of a string). 
; Then it adds a carriage return to the end of the string (because
; that's how the parser knows the command is done) and calls SerialPutChar.
;
; Arguments: CS:[SI] - address of string to send serially
; Return Values: None.
; Global Variables: None.
; Shared Variables:  remoteindex: used to index the string created
;                            in serialputchar because these strings
;                            are all fixed length, and the function
;                            returns once a fixed number of characters
;                            has been sent to the serial port. size: word,
;                             type: R/W
; Local Variables: None.
;
; Inputs: None.
; Outputs: None.
; User Interface: None.
; Error Handling: None.
; Algorithms: None.
;
; Data Structures: None.
; Limitations: None.
; Known Bugs: None.
; Registers used: AX, SI
;
; Author: Meera Krishnamoorthy
; Last Modified: 10/17/16
;

SerialPutStringRemote      PROC        NEAR
        PUBLIC      SerialPutStringRemote
ContSerialPutStringRemote: 
    MOV AL, CS:[SI]  ; gets current character to send to the serial port
                     ; (all the characters are sent to SI before this
                     ; function is called)
    INC SI   ; SI is incremented (to move on to next character)
    INC remoteindex  ; remote index is incremented (keeps track of 
                     ; how many characters have been sent to serial
                     ; because all the strings are the same length,
                     ; and this allows us to keep track of when an entire
                     ; string has been sent
    
    CMP remoteindex, DISPLAY_LEN - 1 ; all strings are of DISPLAY_LEN - 1
                                     ; length
    JG CheckSerialPutStringRemote ; if the remote index is DISPLAY_LEN - 1,
                                  ; means entire string has been sent to the
                                  ; serial, so can return
    PUSH SI ; if entire string has not been sent, need to send this new
            ; character to the serial port
            ; save value of SI because this contains address of string,
            ; and is changed in serialputchar
    Call SerialPutChar ; calls function to put characters over string
    POP SI ; gets back address of string to send to serial
    JMP ContSerialPutStringRemote ; continue looping to get more characters
                                  ; to send to serial because have not sent
                                  ; entire string yet
    
CheckSerialPutStringRemote:  
    MOV AL, CARRIAGE_RETURN ; add carriage return to end of string at end
                            ; because this is how the motor reads strings
                            ; in (knows they are done)
    Call SerialPutChar ; send carriage return to serial to join rest of string
    MOV remoteindex, 0 ; reset motor index to index new string to send to serial
    
EndSerialPutStringRemote:
    RET


SerialPutStringRemote	ENDP

; DisplayMotorInfo
; Description: 
;
; Operational Description: 
;
; Arguments: 
; Return Values: None.
; Global Variables: None.
; Shared Variables:  
; Local Variables: None.
;
; Inputs: None.
; Outputs: None.
; User Interface: None.
; Error Handling: None.
; Algorithms: None.
;
; Data Structures: None.
; Limitations: None.
; Known Bugs: None.
; Registers used: AX, SI
;
; Author: Meera Krishnamoorthy
; Last Modified: 10/17/16
;

DisplayMotorInfo      PROC        NEAR
        PUBLIC      DisplayMotorInfo
MOV BX, 0
StoreCurrSpeed:
    MOV dispmotorbuffer[BX], 'S'
    INC BX
    Call GetMotorSpeed ; stores current speed in AX
    MOV BX, 65534
    MOV DX, 0
    DIV BX ; now AX has percent of speed as compared to maximum speed
    LEA SI, dispmotorbuffer
    ADD SI, BX
    Call Dec2String
  
    ADD BX, TEMP_SIZE
 
StoreCurrDir:   
    MOV dispmotorbuffer[BX], 'A'
    INC BX
    Call GetMotorDirection
    LEA SI, dispmotorbuffer
    ADD SI, BX
    Call Dec2String
    
    ADD BX, TEMP_SIZE
    
StoreCurrLaser:      
    MOV dispmotorbuffer[BX], 'L'
    INC BX
    Call GetLaser
    LEA SI, dispmotorbuffer
    ADD SI, BX
    Call Dec2String
    
    ADD BX, TEMP_SIZE
    
    MOV dispmotorbuffer[BX], ASCII_NULL
    
EndDispMotorInfo:
    RET


DisplayMotorInfo	ENDP

; CheckCriticalFlag
; Description: This function checks the critical flag (which is set if
; the Event Queue is full. It moves the value of the critical flag
; to a register so the main loop can find out if the queue is full,
;
; Operational Description: This is done by moving the value of the critical
; flag into AX. The main loop will then check the value of AX, and if 
; it is set to be a special value (CRITICAL_FLAG), then the queue
; is empty.
;
; Arguments: None.
; Return Values: None.
; Global Variables: None.
; Shared Variables: criticalflag: set if the event queue is full (used to 
;                             check if the main loop has to restart. size: word,
;                             type: R/W
; Local Variables: None.
;
; Inputs: None.
; Outputs: None.
; User Interface: None.
; Error Handling: None.
; Algorithms: None.
;
; Data Structures: None.
; Limitations: None.
; Known Bugs: None.
; Registers used: AX
;
; Author: Meera Krishnamoorthy
; Last Modified: 12/5/16
;

CheckCriticalFlag      PROC        NEAR
        PUBLIC      CheckCriticalFlag
        
ConfirmQueueStatus:
    MOV AX, criticalflag ; the main loop checks AX for the value
                         ; of AX. AX is set to CRITICAL_VALUE 
EndCriticalFlag: 
    RET

CheckCriticalFlag	ENDP


; MotorTable
;
; Description:     This table indexes functions to call to handle various
;                  events based on the constants related to those
;                  events (stored in the high bit of AH when enqueued
;                  to the event queue). A constant of 0 would indicate
;                  a key event, for example, which would lead to calling
;                  get key press, and so on. 
;
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/12/16



;the data segment. initialized because used later on. 
MotorTable       LABEL   WORD
        PUBLIC  MotorTable

DW          offset(doNOP) ; corresponds to key press event
DW          offset(GetError) ; corresponds to serial error event
DW          offset(GetReceivedData) ; corresponds to received data event
DW          offset(doNOP) ; corresponds to queue being empty


; MotorErrors
;
; Description:      This table is called when there are motor errors and
;                   will display those strings. This table only has one
;                   entry for now, but could have more if we wanted
;                   to specify type of parsing error. 
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/12/16

MotorErrors       LABEL   BYTE
        PUBLIC  MotorErrors        
DB 'mpArSEr',ASCII_NULL		 ; is displayed when there is a parsing error
                             ; on the motor side

; ErrorTable
;
; Description:      This table has a list of errors corresponding to the
;                   value of the line status register
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    12/5/16

ErrorTable       LABEL   BYTE
        PUBLIC  ErrorTable 
DB 'Error  ',ASCII_NULL    ; lsr is other value 
DB 'OvErrun',ASCII_NULL	   ; lsr: 2// second bit of lsr is set   
DB 'PArity ',ASCII_NULL	   ; lrs: 4// third bit of lsr is set  	     
DB 'o-P    ',ASCII_NULL	   ; lsr: 6// 2/3 bit of lsr is set    
DB 'FrAning',ASCII_NULL	   ; lsr: 8// fourth bit of lsr  is set 	   
DB 'F-o    ',ASCII_NULL    ; lsr: 10// 4/2 bit of lsr is set 
DB 'F-P    ',ASCII_NULL    ; lsr: 12// 4/3 bit of lsr is set 
DB 'F-P-o  ',ASCII_NULL    ; lsr: 14// 4/3/2 bit of lsr is set 
DB 'break  ',ASCII_NULL    ; lsr: 16// fifth bit of lsr is set  
DB 'b-o    ',ASCII_NULL    ; lsr: 18// 5/2 bit of lsr is set 
DB 'b-P    ',ASCII_NULL    ; lsr: 20// 5/3 bit of lsr is set 
DB 'b-f    ',ASCII_NULL    ; lsr: 22// 5/3/2 bit of lsr is set 
DB 'b-f-o  ',ASCII_NULL    ; lsr: 24// 5/4 bit of lsr is set 
   
CODE    ENDS

;the data segment

DATA    SEGMENT PUBLIC  'DATA'

rdstringbuffer    DB DISPLAY_LEN DUP (?)
dispmotorbuffer   DB DISPLAY_LEN DUP (?)
rdindex         DW      ?
remoteindex     DW      ?
EventQueue QueueStruct <>  ; creates an instance of the queue structure   
                           ; to be used to hold events that occur
                                                        
criticalflag    DW      ?
errorflag       DB      ?

DATA    ENDS


END
