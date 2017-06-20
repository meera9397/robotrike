NAME    MFUNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                                                            ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description: This file has several functions that are needed to 
; run the remote main loop, including functions to initialize, enqueue,
; and dequeue the Event Queue, which is used to store all key press and
; serial events
; 
; Table of Contents
; 1) InitEventQueue
; 
; Revision History:
;
;

CGROUP  GROUP   CODE
DGROUP  GROUP   DATA


CODE	SEGMENT PUBLIC 'CODE'

EXTRN   QueueInit:NEAR
EXTRN   Enqueue:NEAR
EXTRN   Dequeue:NEAR
EXTRN   QueueFull:NEAR
EXTRN   QueueEmpty:NEAR
EXTRN   Display:NEAR
EXTRN   Hex2String:NEAR
EXTRN   GetMotorSpeed:NEAR
EXTRN   GetMotorDirection:NEAR
EXTRN   GetLaser:NEAR

EXTRN   SerialPutChar:NEAR

$INCLUDE(MAIN9.inc)
$INCLUDE(queues.inc)
$INCLUDE(simpmac.inc)

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
CALL QueueInit ; calls function to initialize t

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
; Operational Description: This function does this by putting the address
; of the event queue in SI, and calling the already written function
; Enqueue that enqueues an event to the event queue if the queue is not
; full. Furthermore, if the queue is full, instead of trying to enqueue to
; it, this function sets the critical flag variable to a special constant
; that will be used in the main loop to determine whether to reset
; the main loop. 
;
; Arguments: AX: value to enqueue to the event queue
; Return Values: None.
; Global Variables: None.
; Shared Variables: critical flag: 
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
; Registers used: SI, flags
;
; Author: Meera Krishnamoorthy
; Last Modified: 12/5/16
;

EnqueueEvent      PROC        NEAR
        PUBLIC      EnqueueEvent

InitEnqueueEvent:
    MOV CX, AX   ; saves value of AX (value to enqueue to event)
                 ; because queue full changes values of AX
    LEA SI, EventQueue ; sets SI to address of event queue (because
                       ; that is how queue full is called)
    CALL QueueFull ; checks if the queue is full. sets the zero
                   ; flag if the queue is full, and resets it if
                   ; queue is not full
    JNZ CanEnqueue
    MOV criticalflag, CRITICAL_FLAG
    JMP EndEnqueueEvent
    
CanEnqueue:
    MOV AX, CX
    LEA SI, EventQueue
    CALL Enqueue
    
EndEnqueueEvent:
    RET

EnqueueEvent	ENDP


; Function Name: DequeueEvent
; Description: This function dequeues an event from the event queue.
; The reason that this function exists is to specify
; the address of the event queue, so that remote functions can access
; the event queue without the event queue having to become a global structure.
;
; Operational Description:  This function does this by putting the address
; of the event queue in SI, and calling the already written function
; Dequeue that dequeues an event to the event queue if the queue is not
; empty.
;
; Arguments: None.
; Return Values: None.
; Global Variables: None.
; Shared Variables: None.
; Local Variables: None.l hw9
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
; Registers used: SI, flags
;
; Author: Meera Krishnamoorthy
; Last Modified: 12/5/16
;

DequeueEvent      PROC        NEAR
        PUBLIC      DequeueEvent

InitDequeueEvent:
    LEA SI, EventQueue
    CALL QueueEmpty
    JNZ CanDequeue
ResetAX:
    MOV BX, QUEUE_EMPTY_CONST
    JMP CallDequeueFunc
CanDequeue:
    LEA SI, EventQueue
    CALL Dequeue
    MOV BX, 0 ; clear upper byte of BX (used to index remote table)
    XCHG BL, AH ; moves the event type into BL so that it can index table
                ; also now high bit of AH is cleared so AL (event value)
                ; which is only necessary information is the only thing
                ; remaining

    SHL BL, 1
    
CallDequeueFunc:
    CALL CS:RemoteTable[BX]

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
; command into the serial channel.
;
; Operational Description: This function works by first comparing the 
; key press to a table of valid key presses 
; 
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
; Data Structures: None.
; Limitations: None.
; Known Bugs: None.
; Registers used:
;
; Author: Meera Krishnamoorthy
; Last Modified: 10/17/16
;

GetKeyPress      PROC        NEAR
        PUBLIC      GetKeyPress
MOV BX, 0
CheckKeyPress:
    MOV CL, CS:AllKeyPresses[BX]
    CMP AL, CL
    JE GetKeyString
    CMP BX, MAX_KEY_PRESS
    JGE NoKeyPress
    INC BX
    JMP CheckKeyPress
    
GetKeyString:
    LEA SI, KeyPressTable
    MOV AX, BX
    MOV BX, LEN_DISPLAY
    MUL BX
    ADD SI, AX
    
    PUSH AX
    Call SerialPutStringRemote
    POP AX

    LEA SI, KeyDisplayTable
    ADD SI, AX
    JMP AllDisplay
    
NoKeyPress:
    MOV SI, offset(KeyDisplayTable)
    ADD SI, 16
    
AllDisplay:
    MOV AX, CS
    MOV ES, AX
    
    Call Display 
    
EndGetKeyPress: 
    RET

    
        
GetKeyPress	ENDP


; GetError
; Description: This function displays the type of error that occurred
; using a table that is indexed based on the value of the error
; event (the Line Status Register). 
;
; Operational Description: This is done by first setting the shared
; variable error flag to ERROR_FLAG_VAL so nothing can be sent from the
; serial channel (due to this error). Then, it checks if the value
; is a value that corresponds to an index in the error table. If it is,
; then we get the string corresponding to that index of the table
; and call Display to display that string (a function in display.asm).
; If it is not, we display that a generic error has occurred, because
; it is not one that is in our table). 
;
; Arguments: AL: values to display
; Return Values: None.
; Global Variables: None.
; Shared Variables: errorflag: set if a serial error occurs (so no data
;                             can be received from the serial. size: word,
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
; Registers used:
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
              ; make sure AH is 0 before multiplying by LEN_DISPLAY
    MOV BX, LEN_DISPLAY ; moves into BX the length of the display (which
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
; Arguments: AL: characters to display
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
; Error Handling: None.pcde
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

GetReceivedData      PROC        NEAR
        PUBLIC      GetReceivedData
 
 
IntGetReceivedData: 
    CMP errorflag, ERROR_FLAG_VAL ; if the error flag is set (meaning
                                  ; a serial error occurred, do not
                                  ; display the string that follows)
    JE EndGetReceivedData
    MOV BX, rdindex ; get index of received data string buffer (how many
                    ; characters in string that we have received)
    MOV rdstringbuffer[BX], AL ; move received character into buffer
                               ; holding all received characters
    
    CMP AL, CARRIAGE_RETURN ; check if carriage return (signals end of command)
    JNE ContAddString ; if no carriage return, need to keep
                      ; adding to current string buffer, increment index

DisplayReceivedData: ; if this overflows, it only outputs the latter half
                     ; of the string
    CMP rdindex, 0 ; if got a carriage return on first key, error so jump
                   ; to end
    JE EndRDDisplay
    
    INC BX ; otherwise, add null character to end of string buffer
        
    MOV rdstringbuffer[BX], ASCII_NULL
    
    LEA SI, rdstringbuffer
    
    PUSH DS
    POP ES
    Call Display ; will display stringbuffer (starting at SI)


EndRDDisplay:     
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
; Operational Description: This is done by disp;laying a message
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
    
    Call Display  ; display string
    
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
; Arguments: None.
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
    
    CMP remoteindex, LEN_DISPLAY - 1 ; all strings are of LEN_DISPLAY - 1
                                     ; length
    JG CheckSerialPutStringRemote ; if the remote index is LEN_DISPLAY - 1,
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
    MOV AX, criticalflag
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
;                   can be used to index the KeyPressTable (which
;                   tells you the string to output to the serial channel
;                   based on key press) and the KeyDisplayTable (which
;                   tells you teh string to output ot the display based
;                   on the key press)
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/12/16

AllKeyPresses       LABEL   BYTE
        PUBLIC  AllKeyPresses

DB          KEY_PRESS_1       
DB          KEY_PRESS_2      
DB          KEY_PRESS_3
DB          KEY_PRESS_4
       
DB          KEY_PRESS_5       
DB          KEY_PRESS_6        
DB          KEY_PRESS_7
DB          KEY_PRESS_8
      
DB          KEY_PRESS_9       
DB          KEY_PRESS_10       
DB          KEY_PRESS_11
DB          KEY_PRESS_12
      
DB          KEY_PRESS_13       
DB          KEY_PRESS_14       
DB          KEY_PRESS_15
DB          KEY_PRESS_16


; KeyPressTable
;
; Description:      This table has a list of strings to output to the
;                   serial channel. It is indexed based on key press.
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/12/16

KeyPressTable       LABEL   BYTE
        PUBLIC  KeyPressTable        
DB 'S1000  ',CARRIAGE_RETURN	     ; responds to key press of e0     
DB 'S-1000 ',CARRIAGE_RETURN		     
DB 'S5000  ',CARRIAGE_RETURN	         
DB 'S10000 ',CARRIAGE_RETURN	

DB 'V1000  ',CARRIAGE_RETURN		       
DB 'V-1000 ',CARRIAGE_RETURN	
DB 'V5000  ',CARRIAGE_RETURN	
DB 'V10000 ',CARRIAGE_RETURN	

DB 'D1000  ',CARRIAGE_RETURN		     
DB 'D-1000 ',CARRIAGE_RETURN	
DB 'D5000  ',CARRIAGE_RETURN		      
DB 'D10000 ',CARRIAGE_RETURN	

DB 'F      ',CARRIAGE_RETURN	
DB 'O      ',CARRIAGE_RETURN	      
DB 'F      ',CARRIAGE_RETURN	     
DB 'O      ',CARRIAGE_RETURN		   
    
	
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
; Last Modified:    11/12/16

KeyDisplayTable       LABEL   BYTE
        PUBLIC  KeyDisplayTable
        
DB 'Frd1   ',ASCII_NULL	          
DB 'Frd-1  ',ASCII_NULL		     
DB 'Frd5   ',ASCII_NULL	         
DB 'Frd10  ',ASCII_NULL	

DB 'rFrd1  ',ASCII_NULL		       
DB 'rFrd-1 ',ASCII_NULL	
DB 'rFrd5  ',ASCII_NULL	
DB 'rFRr10 ',ASCII_NULL	

DB 'dir1   ',ASCII_NULL		     
DB 'dir-1  ',ASCII_NULL	
DB 'dir5   ',ASCII_NULL		      
DB 'dir10  ',ASCII_NULL	

DB 'FLASEr ',ASCII_NULL	
DB 'OLASEr ',ASCII_NULL	      
DB 'FLASEr ',ASCII_NULL	     
DB 'OLASEr ',ASCII_NULL		   
    
DB 'BADKEY ',ASCII_NULL   
        
        
        
; ErrorTable
;
; Description:      This table has a list of errors corresponding to the
;                   value of the line status register
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/12/16

ErrorTable       LABEL   BYTE
        PUBLIC  ErrorTable 
DB 'Error  ',ASCII_NULL    ; lsr is other value 
DB 'OvErrun',ASCII_NULL	   ; lsr: 2// second bit of lsr is set   
DB 'PArity ',ASCII_NULL	   ; lrs: 4// third bit of lsr is set  	     
DB 'o-P    ',ASCII_NULL	   ; lsr: 6// 2/3 bit of lsr    
DB 'FrAning',ASCII_NULL	   ; lsr: 8// fourth bit of lsr  	   
DB 'F-o    ',ASCII_NULL    ; lsr: 10// 4/2 bit of lsr
DB 'F-P    ',ASCII_NULL    ; lsr: 12// 4/3 bit of lsr
DB 'F-P-o  ',ASCII_NULL    ; lsr: 14// 4/3/2 bit of lsr
DB 'break  ',ASCII_NULL    ; lsr: 16// 5/2 bit of lsr   
DB 'b-o    ',ASCII_NULL    ; lsr: 18// 5/3 bit of lsr   
DB 'b-P    ',ASCII_NULL    ; lsr: 20// 5/4 bit of lsr
DB 'b-f    ',ASCII_NULL    ; lsr: 22// 5/4 bit of lsr
DB 'b-f-o  ',ASCII_NULL    ; lsr: 24// 5/4 bit of lsr

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
DB 'mpArSEr',ASCII_NULL		 ; responds to motor parsing error

   
CODE    ENDS

;the data segment

DATA    SEGMENT PUBLIC  'DATA'

rdstringbuffer    DB LEN_DISPLAY DUP (?)  
rdindex         DW      ?
remoteindex     DW      ?
EventQueue QueueStruct <>  ; creates an instance of the queue structure   
                           ; to be used to hold events that occur
                                                        
criticalflag    DW      ?
errorflag       DB      ?

DATA    ENDS


END
