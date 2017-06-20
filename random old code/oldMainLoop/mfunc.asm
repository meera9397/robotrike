NAME    MFUNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                                                            ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; file description including table of contents
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

EXTRN   GetMotorSpeed:NEAR
EXTRN   GetMotorDirection:NEAR
EXTRN   GetLaser:NEAR
EXTRN   Dec2String:NEAR
EXTRN   SerialPutChar:NEAR

$INCLUDE(MAIN9.inc)
$INCLUDE(queues.inc)

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
; Last Modified: 10/17/16
;

InitEventQueue      PROC        NEAR
        PUBLIC      InitEventQueue

LEA SI, EventQueue
MOV AX, queue_size
MOV BX, word_len
CALL QueueInit

RET

InitEventQueue	ENDP


; EventQueueEmpty
; Description: This function checks if the event queue is empty using a pre
; existing function QueueEmpty. The reason that this function exists is to specify
; the address of the event queue, so that remote functions can access
; the event queue without the event queue having to become a
; global structure.
;
; Operational Description: This function does this by putting the address
; of the event queue in SI, and calling the already written function
; QueueEmpty to check if the event queue is empty. The zero flag is set if
; the queue is empty, and reset if the queue is not empty.
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
; Data Structures: None.
; Limitations: None.
; Known Bugs: None.
; Registers used: SI, flags
;
; Author: Meera Krishnamoorthy
; Last Modified: 10/17/16
;

EventQueueEmpty      PROC        NEAR
        PUBLIC      EventQueueEmpty

LEA SI, EventQueue
CALL QueueEmpty

RET

EventQueueEmpty	ENDP

; EventQueueFull
; Description: This function checks if the event queue is full.
; The reason that this function exists is to specify
; the address of the event queue, so that remote functions can access
; the event queue without the event queue having to become a global structure.
;
; Operational Description: This function does this by using the preexisting
; function QueueFull, putting the address of the Event Queue in SI because
; this is how QueueFull is called.
;
; Arguments: SI: address of EventQueue
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
; Last Modified: 10/17/16
;

EventQueueFull      PROC        NEAR
        PUBLIC      EventQueueFull

LEA SI, EventQueue
CALL QueueFull

RET

EventQueueFull	ENDP


; EnqueueEvent
; Description: This function enqueues an event to the event queue, assuming
; that the value to be enqueued is in AX.
; The reason that this function exists is to specify
; the address of the event queue, so that remote functions can access
; the event queue without the event queue having to become a global structure.
;
; Operational Description: This function does this by putting the address
; of the event queue in SI, and calling the already written function
; Enqueue that enqueues an event to the event queue if the queue is not
; full.
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
; Data Structures: None.
; Limitations: None.
; Known Bugs: None.
; Registers used: SI, flags
;
; Author: Meera Krishnamoorthy
; Last Modified: 10/17/16
;

EnqueueEvent      PROC        NEAR
        PUBLIC      EnqueueEvent

InitEnqueueEvent:
    CALL EventQueueFull
    JE EndEnqueueEvent
CanEnqueue:
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
; Last Modified: 10/17/16
;

DequeueEvent      PROC        NEAR
        PUBLIC      DequeueEvent

InitDequeueEvent:
    CALL EventQueueEmpty
    JE ResetAX
CanDequeue:
    LEA SI, EventQueue
    CALL Dequeue
    JMP EndDequeueEvent
ResetAX:
    MOV AX, 0
EndDequeueEvent
    RET

DequeueEvent	ENDP


; GetKeyPress
; Description: This function is called whenever the event queue dequeues
; a key press event. This function converts a key press event into a command
; that the motor will be able to parse. This is done using various tables
; After the command is found, SerialPutString is called to put the
; command into the serial channel.
;
; Operational Description:
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
    JE NoKeyPress
    INC BX
    JMP CheckKeyPress
GetKeyString:
    CMP AL, 0D3H
    JE DisplaySpeed
    CMP AL, 0B3H
    JE DisplayDirection
    CMP AL, 073H
    JE DisplayLaser
    SHL BX, 2 ; get word index for table
    MOV SI, offset(KeyPressTable)
    ADD SI, BX
    Call SerialPutStringRemote
    JMP EndGetKeyPress

DisplaySpeed:
    PUSH SI
    Call GetMotorSpeed
    MOV byte ptr [SI], 'S'
    JMP GetReadyDisplay

DisplayDirection:
    PUSH SI
    Call GetMotorDirection
    MOV byte ptr [SI], 'D'
    JMP GetReadyDisplay
    
DisplayLaser:
    PUSH SI
    Call GetLaser
    MOV byte ptr [SI], 'L'
     
GetReadyDisplay:
    INC SI
    Call Dec2String
    JMP AllDisplay

NoKeyPress:
    MOV SI, 'X'
    
AllDisplay:
    POP SI
    PUSH DS
    POP ES
    Call Display

EndGetKeyPress:  
    RET

    
        
GetKeyPress	ENDP


; GetError
; Description: This function displays the value of the event (which is
; usually the line status register) on the display, preceded by an "E"
; to indicate error.
;
; Operational Description: This is done by moving "E" and the value of the
; error event (in AL previously), into the stringbuffer, and then
; calling Display to display the string. The value in AL has to be converted
; to a string using "HexToString"
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
; Last Modified: 10/17/16
;

GetError      PROC        NEAR
        PUBLIC      GetError

MOV SI, ASCII_E
MOV CX, SI
INC CX
MOV AH, 0 ; clear high byte of AH because all we need is AH
MOV CX, AX
PUSH DS
POP ES
Call Display ; will display stringbuffer

RET

GetError	ENDP


; GetReceivedData
; Description: This function displays the received data on the display.
; The received data is in the form of a command (from the motor side), which
; is in a string, so the entire string is read in, and then is displayed.
; We assume the characters of the string are sent in order, so we put
; them in the string buffer in order, and then display once a carriage
; return is sent.
;
; Operational Description: THe string is read in and stored in the string
; buffer by storing characters when this function is called until
; a character equals a carriage return. Then, a null character is added to the
; end of the string buffer and it is displayed.
;
; Arguments: AL: characters to display
; Return Values: None.
; Global Variables: None.
; Shared Variables: rdindex: saves our index in the string buffer (so we know
;                   how much of a string we've stored)
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

GetReceivedData      PROC        NEAR
        PUBLIC      GetReceivedData

IntGetReceivedData: 
    MOV CX, SI
    ADD CX, rdindex   ; increment SI
    MOV CL, AL
    CMP AL, ASCII_NULL ;(carriage return)
    JNE ContAddString
    MOV CX, ASCII_NULL ;(to indicate end of the string)
    
    PUSH DS
    POP ES
    
    Call Display ; will display stringbuffer (starting at SI)
    
    JMP EndGetReceivedData

ContAddString:
    INC rdindex ; increment index of SI
    
EndGetReceivedData: 
    RET

GetReceivedData	ENDP


; SerialPutStringRemote
; Description: This function sends a series of characters over the serial
; channel from the remote unit to the motor unit.
;
; Operational Description: This is done using the already written function
; SerialPutChar, which puts characters into the serial channel. It continues
; to call SerialPutChar until all characters in the string have been sent
; over. Then it adds a carriage return to the end of the string (because
; that's how the parser knows the command is done) and calls SerialPutChar.
;
; Arguments: None.
; Return Values: None.
; Global Variables: None.
; Shared Variables: remoteindex: saves our index in the string buffer (so we know
;                   how much of a string we've stored)
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
; Registers used: SI
;
; Author: Meera Krishnamoorthy
; Last Modified: 10/17/16
;

SerialPutStringRemote      PROC        NEAR
        PUBLIC      SerialPutStringRemote
        
ContSerialPutStringRemote:
    CMP byte ptr [SI], ASCII_NULL
    JE AddCR
    MOV AL, byte ptr [SI]
    INC SI
    Call SerialPutChar
    JMP ContSerialPutStringRemote
    
AddCR:
    MOV SI, CARRIAGE_RETURN
    Call SerialPutChar
  
EndSerialPutStringRemote:   
    RET

SerialPutStringRemote	ENDP

; CheckCriticalFlag
; Description: 
;
; Operational Description: 
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
; Registers used: SI
;
; Author: Meera Krishnamoorthy
; Last Modified: 10/17/16
;

CheckCriticalFlag      PROC        NEAR
        PUBLIC      CheckCriticalFlag
        
ConfirmQueueStatus:
    Call EventQueueFull
    JNZ EndCriticalFlag 
    MOV AX, CRITICAL_FLAG
EndCriticalFlag: 
    RET

CheckCriticalFlag	ENDP

; AllKeyPresses
;
; Description:      This table has a list of all key presess
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/12/16

AllKeyPresses       LABEL   BYTE
        PUBLIC  AllKeyPresses

DB          0E0H       
DB          0D0H      
DB          0B0H
DB          070H
       
DB          0E1H       
DB          0D1H        
DB          0B1H
DB          071H
      
DB          0E2H       
DB          0D2H       
DB          0B2H
DB          072H
      
DB          0E3H       
DB          0D3H       
DB          0B3H
DB          073H


; KeyPressTable
;
; Description:      This table has a list of all key presess
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/12/16

KeyPressTable       LABEL   BYTE
        PUBLIC  KeyPressTable
        
DB 'S10'	          
DB 'S-100'		     
DB 'S1000'	         
DB 'S65534'

DB 'V10'	       
DB 'V-100'
DB 'V1000'      
DB 'V33000'	        

DB 'D10'	     
DB 'D-100'		      
DB 'D33000'	      
DB 'F'

DB 'O'
DB 'DISP_S'      
DB 'DISP_D'     
DB 'DISP_L'	   
    


    
        

CODE    ENDS

;the data segment

DATA    SEGMENT PUBLIC  'DATA'

stringbuffer2    DB      ?  
rdindex          DW      ?
EventQueue QueueStruct <>  ; creates an instance of the queue structure   
                           ; to be used to hold events that occur
                             

DATA    ENDS


END
