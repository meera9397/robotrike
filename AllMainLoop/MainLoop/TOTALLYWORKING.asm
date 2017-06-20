NAME    MFUNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                                                            ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description: This file has several functions that are needed to 
; run the remote main loop. These functions are mostly user interface
; focused (they focus on displaying values to the display when 
; the motor sends over its status/ keys are pressed to change various
; aspects of the motor speed and direction). Other functions
; included in this file are functions related to the Event Queue, 
; a queue used to store keypad and serial events that occur, and functions
; to communicate serial/parsing errors on the motor side to the user.
; 
; Table of Contents
; 1) InitEventQueue: initializes event queue
; 2) DequeueEvent: dequeues event from event queue
; 3) EnqueueEvent: enqueues event to event queue 
; 4) InitRemoteFunct: initializes shared variables used in the following 
;                     functions
; 5) GetKeyPress: sends motor command corresponding to key press to
;                 motor side via serial (using SendSerialPutStringRemote)
;                 and displays what was sent on display
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

; used to display
EXTRN   Display:NEAR

; used to send values over serial channel
EXTRN   SerialPutChar:NEAR

$INCLUDE(MAIN9.inc)
$INCLUDE(queues.inc)
$INCLUDE(simpmac.inc)
$INCLUDE(display.inc)

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


; Function Name: DequeueEvent
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
; error. This is done using the call table RemoteTable which is indexed
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
;                   Remote Table, type: word, length: 4 elements: is a call
;                       table that is indexed by event type -- calls
;                       the appropriate function to deal with each event
; Limitations: None.
; Known Bugs: None.
; Registers used: SI, flags
;
; Author: Meera Krishnamoorthy
; Last Modified: 12/5/16
;

DequeueEvent      PROC        NEAR
        PUBLIC      DequeueEvent

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
    CALL CS:RemoteTable[BX] ; calls function associated with
                            ; each event type to deal with those events

EndDequeueEvent:
    RET

DequeueEvent	ENDP




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
MOV errorflag, 0 ; sets the error flag to 0

RET

InitRemoteFunct	ENDP

; GetKeyPress
; Description: This function is called whenever the event queue dequeues
; a key press event. This function converts a key press event into a command
; that the motor will be able to parse. This is done using various tables
; After the command is found, SerialPutString is called to put the
; command into the serial channel (send the command to the motor side).
;
; Operational Description: This function works by first comparing the 
; key press to a table of valid key presses. If the key press exists
; on that table, it will display a string corresponding to that key press
; on the display, and send a command to the motor unit to execute using
; SerialPutString. If the key press does not exist, a "bad key press"
; error message is sent. 
;
; Arguments: AL: key press to decode
; Return Values: None
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
; Data Structures: AllKeyPresses table: type: byte, has all valid key presses
;                  SerialKeyPressTable: type: byte, has strings that
;                           need to be sent serially to the remote side
;                  KeyDisplayTable: type: byte, has strings that need
;                           to be sent to the display
; Limitations: None.
; Known Bugs: None.
; Registers used:
;
; Author: Meera Krishnamoorthy
; Last Modified: 10/17/16
;

GetKeyPress      PROC        NEAR
        PUBLIC      GetKeyPress
MOV errorflag, 0 ; sets the error flag to 0
MOV BX, 0 ; BX will index through our AllKeyPresses table, 
          ; a table that checks if the value in AL is a valid key press
CheckKeyPress:
    MOV CL, CS:AllKeyPresses[BX] ; move a value from AllKeyPresses table
                                 ; into CL
    CMP AL, CL ; compare the key pressed to the value in CL
    JE GetKeyString ; if they are equal, we can get the string associated
                    ; with that key press to send over serial/ display
                    ; on display
    CMP BX, MAX_KEY_PRESS ; we check if BX is the index of the last
                          ; table element (have checked all the valid
                          ; key presses)
    JGE NoKeyPress ; if we are at the last index, jump to label to
                   ; display appropriate string to display fact
                   ; that key press is not a valid key
    INC BX ; increment index of table to check next index
    JMP CheckKeyPress ; if we have not hit the last value of the AllKeyPresses
                      ; table, we keep looping until we find a valid key press 
    
GetKeyString:
    LEA SI, SerialKeyPressTable ; move address of SerialKeyPressTable
                                ; (table that has strings that need
                                ; to be sent serially to the remote side)
                                ; into SI
    
    ; the AllKeyPresses table and the SerialKeyPressTable are indexed
    ; to correspond (the key press in AllKeyPresses is at the
    ; same index as the string corresponding to it in
    ; SerialKeyPressTable). The only difference is that
    ; the SerialKeyPress table has strings in it that are
    ; DISPLAY_LEN long. Thus, to get the index corresponding to the
    ; string related to the key press in SerialKeyPressTable, we need
    ; to multiply the index of the pressed key in AllKeyPresses
    ; by DISPLAY_LEN (the indexes go by bytes, and each
    ; "index" in a table actually corresponds to DISPLAY_LEN
    ; indexes). 
                
    MOV AX, BX  ; we move the index of the key press from AllKeyPresses
                ; into AX
    MOV BX, DISPLAY_LEN ; move the length of each string in the SerialKeyPress
                        ; table into BX 
    MUL BX ; multiply index from AllKeyPresses table by DISPLAY_LEN
           ; to get appropriate index of the SerialKeyPressTable
    ADD SI, AX ; add index to address of table (address of string to send 
               ; serially related to key press) 
    
    PUSH AX ; save value of AX (KeyDisplayTable, which holds strings
            ; to display on the display is indexed the same way
            ; that SerialKeyPressTable is indexed, so we just need
            ; to add AX to the address of the KeyDisplayTable in order
            ; to get string to display associatd with key press)
    
    Call SerialPutStringRemote ; calls function to send characters over 
                               ; serial 
    POP AX ; get back value of AX to index KeyDisplayTable

    LEA SI, KeyDisplayTable   ; move address of KeyDisplayTable
                              ; (table that has strings that need
                              ; to be sent to the display)
                              ; into SI
    ADD SI, AX ; add index to address of table (address of string to send 
               ; to the display related to key press)
    JMP AllDisplay ; now we can display the result of the key press
                   ; on the display
    
NoKeyPress:
    MOV SI, offset(KeyDisplayTable)
    ADD SI, MAX_KEY_PRESS + 1 ; our KeyDisplayTable has the same number
                              ; of elements as our SerialKeyPressTable + 1 for
                              ; bad key presses. The element corresponding
                              ; to bad key presses is at the end of the
                              ; table, hence the + 1. 
    
AllDisplay:
    MOV AX, CS ; copy code segment into ES because display reads strings
               ; from ES
    MOV ES, AX
    
    Call Display ; display string corresponding to key press
    
EndGetKeyPress: 
    RET

    
        
GetKeyPress	ENDP


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
    
    MOV AX, CS ; changes the segment from CS to ES because Display works
               ; in the segment ES
    MOV ES, AX
    
    Call Display  ; calls function to display string corresponding to error
                  ; on display
    JMP EndGetError
    
DisplayUnIdentifiedError:
    LEA SI, ErrorTable ; if error is unidentified, display 
                       ; that generic error has occurred (we have set
                       ; the generic error to be the first index
                       ; of the table, so the table address points
                       ; to this string
    MOV AX, CS ; changes the segment from CS to ES because Display works
               ; in the segment ES
    MOV ES, AX ; calls function to display string corresponding to error
                  ; on display
    
    Call Display 
	
	MOV errorflag, 0 ; does this to make sure no received data
                                  ; is displayed
    
EndGetError: 
    RET
 
RET

GetError	ENDP



; doNOP
; Description: This function is called when the queue is empty and no 
; action can be taken in the function DequeueEvent. It does nothing.
;
; Operational Description: This function simply returns. 
;
; Arguments: AL: values to display
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

doNOP      PROC        NEAR
        PUBLIC      doNOP

RET

doNOP	ENDP

; GetReceivedData
; Description: This function displays the received data on the display.
; The received data is in the form of a command (from the motor side), which
; is in a string, so the entire string is read in, and then is displayed.
; We assume the characters of the string are sent in order, so we put
; them in the string buffer in order, and then display once a carriage
; return is sent. This function is bypassed if the error flag is set
; (meaning a serial error occurred).
;
; Operational Description: The string is read in and stored in the string
; buffer by storing characters when this function is called until
; a character equals a carriage return. Then, a null character is added to the
; end of the string buffer and it is displayed. This function is bypassed
; if the error flag is set (jumps to the end) because the error flag
; is checked at the beginning of the function. 
;
; Arguments: AL: received character that will be added to the string
;            buffer and displayed once a carriage return is
;            hit
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
;                            size: DISPLAY_LEN, type: W
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
; Limitations: Scrolling has not been implemented so if a string greater
;              than DISPLAY_LEN characters is sent to the function, the function
;              only displays the latter characters (the characters that are
;              indexed (total length of string MOD DISPLAY_LEN)
; Known Bugs: None.
; Registers used:
;
; Author: Meera Krishnamoorthy
; Last Modified: 12/5/16
;

GetReceivedData      PROC        NEAR
        PUBLIC      GetReceivedData
 
 
IntGetReceivedData: 
    CMP errorflag, ERROR_FLAG_VAL ; if the error flag is set (meaning
                                  ; a serial error occurred), this 
                                  ; ensures that the string that
                                  ; follows is not displayed
    JE EndGetReceivedData ; jump to end (never display received data
                          ; when error occurs) 
    MOV BX, rdindex ; get index of received data string buffer (how many
                    ; characters in string that we have received)
    MOV rdstringbuffer[BX], AL ; move received character into buffer
                               ; holding all received characters
    
    CMP AL, CARRIAGE_RETURN ; check if carriage return (signals end of command)
    JNE ContAddString ; if no carriage return, need to keep
                      ; adding to current string buffer, increment index

DisplayReceivedData: ; if this overflows, it only outputs the first DISPLAY_LEN
                     ; characters before returning
    CMP rdindex, 0 ; if got a carriage return on first key, error so jump
                   ; to end
    JE EndRDDisplay
    
    INC BX ; otherwise, add null character to end of string buffer
           ; (get end of string buffer by incrementing BX, which stores
           ; the length of the string buffer)
        
    MOV rdstringbuffer[BX], ASCII_NULL ; add ASCII_NULL to end of buffer 
                                       ; because we are calling display
                                       ; which only displays characters
                                       ; that are null terminated
    
    LEA SI, rdstringbuffer ; put address of string buffer in SI, because
                           ; that's where Display reads strings from
    
    PUSH DS ; copies DS (where Display reads strings
            ; from)into to ES   from DS (where string buffer is stored)
    POP ES
    Call Display ; will display stringbuffer (starting at SI)


EndRDDisplay:      ; this reinitializes shared variables that are used in this
              ; function to be ready to receive the next string
    MOV rdindex, 0
    MOV rdstringbuffer, 0
    JMP EndGetReceivedData

ContAddString:
    INC rdindex ; increment index of SI
    
EndGetReceivedData: 
    RET

GetReceivedData	ENDP

; GetMotorError
; Description: This function is called when an event is enqueued to 
; the event queue indicating that a motor error has occurred. On the motor
; side, if the motor gets a parsing error, it will
; enqueue an event to the event queue. This will be read in
; by this function and display that such an error has occurred. 
;
; Operational Description: This is done by displaying a message
;
; Arguments: None.
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
; Registers used: AX
;
; Author: Meera Krishnamoorthy
; Last Modified: 12/5/16
;

GetMotorError      PROC        NEAR
        PUBLIC      GetMotorError

ParserMotorError:
    LEA SI, MotorErrors ; get string corresponding to parser error
    MOV AX, CS ; changes the segment from CS to ES because Display works
               ; in the segment ES
    MOV ES, AX ; calls function to display string corresponding to error
                  ; on display
    
    Call Display  ; display string in MotorErrors table
    
    RET
    
GetMotorError	ENDP

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


; RemoteTable
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
RemoteTable       LABEL   WORD
        PUBLIC  RemoteTable

DW          offset(GetKeyPress) ; corresponds to key press event
DW          offset(GetError) ; corresponds to serial error event
DW          offset(GetReceivedData) ; corresponds to received data event
DW          offset(doNOP) ; corresponds to queue being empty
DW          offset(GetMotorError)

; AllKeyPresses
;
; Description:      This table has a list of all possible key presses.
;                   It is used to translate key presses to an index that
;                   can be used to index the SerialKeyPressTable (which
;                   tells you the string to output to the serial channel
;                   based on key press)) and the KeyDisplayTable (which
;                   tells you the string to output ot the display based
;                   on the key press)
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    12/5/16

AllKeyPresses       LABEL   BYTE
        PUBLIC  AllKeyPresses

DB          KEY_PRESS_1       ; key in first row, first column of keyboard
DB          KEY_PRESS_2       ; key in first row, second column of keyboard
DB          KEY_PRESS_3       ; key in first row, third column of keyboard
DB          KEY_PRESS_4       ; key in first row, fourth column of keyboard
       
DB          KEY_PRESS_5       ; key in second row, first column of keyboard
DB          KEY_PRESS_6       ; key in second row, second column of keyboard 
DB          KEY_PRESS_7       ; key in second row, third column of keyboard
DB          KEY_PRESS_8       ; key in second row, fourth column of keyboard
      
DB          KEY_PRESS_9       ; key in third row, first column of keyboard
DB          KEY_PRESS_10      ; key in third row, second column of keyboard
DB          KEY_PRESS_11      ; key in third row, third column of keyboard
DB          KEY_PRESS_12      ; key in third row, fourth column of keyboard
       
DB          KEY_PRESS_13      ; key in fourth row, first column of keyboard
DB          KEY_PRESS_14      ; key in fourth row, second column of keyboard 
DB          KEY_PRESS_15      ; key in fourth row, third column of keyboard
DB          KEY_PRESS_16      ; key in fourth row, fourth column of keyboard


; SerialKeyPressTable
;
; Description:      This table has a list of strings to output to the
;                   serial channel. It is indexed based on key press.
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    12/5/16

SerialKeyPressTable       LABEL   BYTE
        PUBLIC  SerialKeyPressTable        
DB 'S0     ',CARRIAGE_RETURN	     ; responds to key press 1     
DB 'S32678 ',CARRIAGE_RETURN		 ; responds to key press 2     
DB 'S10000 ',CARRIAGE_RETURN	     ; responds to key press 3     
DB 'S65534 ',CARRIAGE_RETURN	     ; responds to key press 4

DB 'V1000  ',CARRIAGE_RETURN		 ; responds to key press 5      
DB 'V10000 ',CARRIAGE_RETURN	     ; responds to key press 6 
DB 'V100000',CARRIAGE_RETURN	     ; responds to key press 7
DB 'V200000',CARRIAGE_RETURN	     ; responds to key press 8

DB 'D90    ',CARRIAGE_RETURN		 ; responds to key press 9     
DB 'D-90   ',CARRIAGE_RETURN	     ; responds to key press 10 
DB 'D0     ',CARRIAGE_RETURN		 ; responds to key press 11
DB 'D180   ',CARRIAGE_RETURN	     ; responds to key press 12 

DB 'F      ',CARRIAGE_RETURN	     ; responds to key press 13 
DB 'O      ',CARRIAGE_RETURN	     ; responds to key press 14 
DB 'F      ',CARRIAGE_RETURN	     ; responds to key press 15 
DB 'O      ',CARRIAGE_RETURN		 ; responds to key press 16 
    
	
; KeyDisplayTable
;
; Description:      This table has a list of strings to output to the
;                   display. It is indexed based on key press.
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    12/5/16

KeyDisplayTable       LABEL   BYTE
        PUBLIC  KeyDisplayTable
        
DB 'StoP   ',ASCII_NULL	    ; corresponds to key press in first row, 
                            ; first column of keyboard      
DB 'hAlF SP',ASCII_NULL		; corresponds to key press in first row, 
                            ; second column of keyboard       
DB 'rAnd SP',ASCII_NULL	    ; corresponds to key press in first row, 
                            ; third column of keyboard       
DB 'FULL SP',ASCII_NULL	    ; corresponds to key press in first row, 
                            ; fourth column of keyboard  

DB 'ACC 1  ',ASCII_NULL		; corresponds to key press in second row, 
                            ; first column of keyboard         
DB 'ACC 10 ',ASCII_NULL	    ; corresponds to key press in second row, 
                            ; second column of keyboard  
DB 'ACC 100',ASCII_NULL	    ; corresponds to key press in second row, 
                            ; third column of keyboard  
DB 'ACC 200',ASCII_NULL	    ; corresponds to key press in second row, 
                            ; fourth column of keyboard  

DB 'dir R  ',ASCII_NULL		; corresponds to key press in third row, 
                            ; first column of keyboard       
DB 'dir L  ',ASCII_NULL	    ; corresponds to key press in third row, 
                            ; second column of keyboard  
DB 'dir F  ',ASCII_NULL		; corresponds to key press in third row, 
                            ; third column of keyboard        
DB 'dir B  ',ASCII_NULL	    ; corresponds to key press in third row, 
                            ; fourth column of keyboard  

DB 'FLASEr ',ASCII_NULL	    ; corresponds to key press in fourth row, 
                            ; first column of keyboard  
DB 'OLASEr ',ASCII_NULL	    ; corresponds to key press in fourth row, 
                            ; second column of keyboard    
DB 'FLASEr ',ASCII_NULL	    ; corresponds to key press in fourth row, 
                            ; third column of keyboard   
DB 'OLASEr ',ASCII_NULL		; corresponds to key press in fourth row, 
                            ; fourth column of keyboard     
        
DB 'badkey ',ASCII_NULL		; displayed when key not corresponding
                            ; to value in key values table is displayed
        
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


   
CODE    ENDS

;the data segment

DATA    SEGMENT PUBLIC  'DATA'

rdstringbuffer    DB DISPLAY_LEN DUP (?)  
rdindex         DW      ?
remoteindex     DW      ?
EventQueue QueueStruct <>  ; creates an instance of the queue structure   
                           ; to be used to hold events that occur
                                                        
criticalflag    DW      ?
errorflag       DB      ?

DATA    ENDS


END
