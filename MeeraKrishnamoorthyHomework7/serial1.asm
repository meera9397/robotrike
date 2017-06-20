NAME    SERIAL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   SERIAL                                   ;
;     This file contains all functions necessary to transmit values to       ;
;                           other devices via serial port                    ;
;                                  EE/CS 51                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Overall Description: This function contains all the necessary functions
; to transmit values to other devices via serial port.
;
; Table of Contents:
; 1) InitSerial: initializes all the registers and values needed to transmit
; and receive data
; 2) SetSerialBaudRate: changes baud rate if bit in LCR is set to allow
; the baud rate to be changed
; 3) SetSerialValue: ors LCR to whatever is stored in AL
; 4) SerialEH: directs event handler to appropriate function to deal
; with current interrupt.
; 5) LSI_Function: function to deal with line status interrupts
; 6) RDI_Function: function to deal with received data interrupts
; 7) TEI_Function: function to deal with transmitter empty interrupts
; 8) MSI_Function: function to deal with modem status interrupts
;
; Revision History:
;    11/18/16  Meera Krishnamoorthy   wrote code


CGROUP  GROUP   CODE
DGROUP  GROUP   DATA

CODE	SEGMENT PUBLIC 'CODE'

ASSUME  CS:CGROUP, DS: DGROUP

$INCLUDE(SERIAL.INC)
$INCLUDE(QUEUES.INC)
$INCLUDE(SIMPMAC.INC)

EXTRN   QueueInit:NEAR
EXTRN   QueueEmpty:NEAR
EXTRN   QueueFull:NEAR
EXTRN   Dequeue:NEAR
EXTRN   Enqueue:NEAR
EXTRN   EnqueueEvent:NEAR


; InitSerial
;
; Description: This function initializes all the registers and values
; needed to transmit and receive data. This involves initializing the
; TransmitterQueue, the BaudRate variable (which initializes the baud rate),
; the line control register and the interrupt enable register.
;
; Operation: This sets the baud rate to a value with the divisor 2, the
; line control register with values that set the number of transmitted
; characters to 8, enables parity, and enable the divisor latch. It
; also sets the interrupt enable register to a value that enables
; modem status, receiver line status, THRE, and received data available
; interrupts.
;
; Arguments: None.
; Return Value: None.
;
; Local Variables: None.
; Shared Variables:
; Global Variables: None.
;
; Input: None.
; Output: None.
;
; Error Handling: None.
;
; Algorithms: None.
; Data Structures: TransmitterQueue: a queue that holds characters to be
;                       sent to the data register in the 16C450, which performs
;                       serial to parallel conversion on data received
;                       from this queue.
;                    This structure defines a queue and all of its attributes:
;                       1) length: the total length that the queue can be
;                       2) element_size: if this variable is 1, the element
;                          size is word (2 bytes). if this variable is 2,
;                          the element size is one byte.
;                       3) front: describes the element of the array that is
;                          the front of the queue
;                       4) rear: describes the element of the array that is
;                          the back of the queue
;                       5) elements: an array with all the elements of the
;                          queue stored in it
;
;
; Registers Changed: AX, BX, DX
; Stack Depth: None.
;
; Limitations:
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/18/16
;

InitSerial      PROC        NEAR
        PUBLIC      InitSerial

; initialize TransmitterQueue
LEA SI, TransmitterQueue ; address of queue to be used is stored in SI, and
                         ; will be used in QueueInit, etc. to access
                         ; the queue
MOV AX, QUEUE_SIZE ; move the total size of the queue in AX, which will
                   ; be used in QueueInit to create the queue
MOV BL, 0 ; if BL is 0, then the elements stored in this queue will be bytes
Call QueueInit ; initializes queue with a size of QUEUE_SIZE, to be filled
               ; with bytes, and to be stored at the address of TransmitterQueue


; set the LINE CONTROL REGISTER (LCR), which contains values to set the
; parity and the baud rate

; Clears out the current value of the LCR Register
MOV AX, 0
MOV DX, SERIAL_LCR  ; set the address of DX to the LCR
OUT DX, AL

; set Baud Rate
%CRITICAL_START
OR AL, DIVISOR_LATCH ; set AL to be equal to value to enable Baud
                      ; rate to be changed
OUT DX, AL ; enable baud rate to be changed

SetBaudRate: 
	PUSH AX
	Call SetSerialBaudRate ; set baud rate based on BAUD_DIVISOR_INDEX
	POP AX

; reset DLAB
AND AL, NOT(DIVISOR_LATCH) ; make DLAB 0 so baud rate cannot be changed
OUT DX, AL ; enable baud rate to be changed
%CRITICAL_END
	
SetParity: 
	; do nothing so parity is not set
	
	
SetOtherThings: 
	OR AL, TRANSMIT8CHAR ; set LCR so that outputs 8 bits at a time
	OR AL, MORE_1_STOP_BIT ; set LCR to have more than 1 stop bit
	
	MOV DX, SERIAL_LCR  ; set the address of DX to the LCR
	OUT DX, AL; stores this new value in the line control register

; set the INTERRUPT ENABLE REGISTER (IER)

SetIER: 
	MOV AL, MODEM_STATUS
	OR AL, RECEIVER_LINE_STATUS
	OR AL, THRE
	OR AL, RECEIVED_DATA_AVAILABLE

	MOV DX, SERIAL_IER

	OUT DX, AL

MOV KickStartFlag, 1 ; sets kick start flag to 1 because the TransmitterQueue
                     ; has no elements in it, so nothing should be
                     ; transmitted

RET
InitSerial	ENDP


; Function Name: SetSerialBaudRate
; Description: This checks that the bit in the line control register that allows
; the baud rate to be changed is set. This allows the DLL register to be changed
; -- this register holds the value of the baud divisor (which is multiplied
; by 16 and then the clock rate is divided by it to get the baud rate).
; The baud divisors are in a table, and the index of that table
; used to get the baud divisor to be chosen is set in the variable
; BAUD_DIVISOR_INDEX. This table is accessed, and the value in that
; table associated with BAUD_DIVISOR_INDEX is stored in DLL.
;
; Operational Description:
; Arguments: None.
; Return Values: None.
; Global Variables: None.
; Shared Variables: baudRate, W, byte, 8 bits: it stores the
;                   baud rate, which is how fast the data flows.
; Local Variables: None.
;
; Inputs: None.
; Outputs: None.
; User Interface: None.
; Error Handling: None.
; Algorithms: None.
;
; Data Structures: None.
; Known Bugs: None.
;
; Registers Changed: DX, AX, BX
; Stack Depth: None.
; Limitations: Divisor must be a value that exists on the table.
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/18/16
;

SetSerialBaudRate      PROC        NEAR
        PUBLIC      SetSerialBaudRate

MOV DX, SERIAL_LCR  ; set the address of DX to the LCR
IN AL, DX
AND AL, DIVISOR_LATCH ; MASKS all bits except the first one of the LCR,
                       ; which states if the baud rate can be changed
CMP AL, DIVISOR_LATCH
JNE EndSetSerialBaudRate

ChangeBaudDivisor:
  MOV BX, BAUD_DIVISOR_INDEX
  MOV AX, CS:BaudRateTable[BX]

  MOV DX, DLL
  OUT DX, AL ; output low byte to DLL
  
  MOV DX, DLM
  XCHG AH, AL
  OUT DX, AL
EndSetSerialBaudRate:
  RET
SetSerialBaudRate	ENDP


; Function Name: SerialPutChar
; Description: This function outputs the passed character (c) to
; the serial channel. It returns with the carry flag reset if the character
; has been "output", (put in the channel's queue, not necessarily sent over
; the serial channel) and set otherwise (transmit queue is full).
; The character c is passed in value in AL.
;
; Operational Description: This is done using the written functions
; QueueFull and Enqueue. If the TransmitterQueue is Full (if the zero flag is set
; by the QueueFull function), nothing will be enqueued and the carry
; flag will be set.  Else, "c" will be enqueued and the carry flag is reset.
;
; Arguments: AL, register that holds the character (c) to be sent to the
; serial channel
;
; Return Values: None.
;
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
; Data Structures: TransmitterQueue: a queue that holds characters to be
;                       sent to the data register in the 16C450, which performs
;                       serial to parallel conversion on data received
;                       from this queue.
;                    This structure defines a queue and all of its attributes:
;                       1) length: the total length that the queue can be
;                       2) element_size: if this variable is 1, the element
;                          size is word (2 bytes). if this variable is 2,
;                          the element size is one byte.
;                       3) front: describes the element of the array that is
;                          the front of the queue
;                       4) rear: describes the element of the array that is
;                          the back of the queue
;                       5) elements: an array with all the elements of the
;                          queue stored in it
; Limitations: None.
; Known Bugs: None.

;
; Registers Changed: SI, carry flag, AX
; Stack Depth: None.
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/18/16
;

SerialPutChar      PROC        NEAR
        PUBLIC      SerialPutChar
LEA SI, TransmitterQueue
PUSH AX
Call QueueFull
POP AX
JNZ CanEnqueue

CannotEnqueue:
  STC ; sets carry flag
  JMP EndSerialPutChar

CanEnqueue:
  LEA SI, TransmitterQueue
  %CRITICAL_START
  Call Enqueue
  %CRITICAL_END
  CMP KickStartFlag, 1
  JNE EnqueueSetFlag
  
KickStart:
  ; now that we're kickstarting, don't need to kickstart anymore
  MOV KickStartFlag, 0

  ; disable interrupts
  MOV DX, SERIAL_IER
  MOV AL, 0
  OUT DX, AL

  ; re-enable interrupts
  MOV AL, MODEM_STATUS
  OR AL, RECEIVER_LINE_STATUS
  OR AL, THRE
  OR AL, RECEIVED_DATA_AVAILABLE
  OUT DX, AL
  
EnqueueSetFlag:
  CLC ; clear carry flag
  
EndSerialPutChar:
  RET

SerialPutChar	ENDP


; Function Name: SerialEH
; Description: This function is called when any interrupt occurs. It
; finds out what interrupt has occurred, and then determines how to proceed
; by using a table with functions written to deal with each interrupt
; that could occur. The IIR has four potential values: 0, 2, 4, and 5.
; Those values are indexes in the tableSerialIRQTable -- this table has addresses
; of the functions that deal with these interrupts at indexes that match
; those interrupts' IIR value.
;
; Operational Description: This function is called when an interrupt occurs,
; and checks the interrupt identifying register (IIR) to determine what kind
; of interrupt occurred. Once the function has identified the type of
; interrupt that occurred, it looks up the associated register in
; SerialIRQTable, a jump table that associates IIR values with addresses of
; functions written to deal with the interrupts.
;
; Arguments: None.
; Return Values: None.
;
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
; Data Structures: SerialIRQTable: a jump table that associates IIR values
;                  with addresses of functions written to deal with
;                  interrupts.
;
; Limitations: The IIR needs to be valid.
; Known Bugs: None.
;
; Registers: AX, BX, DX
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/18/16
;

SerialEH      PROC        NEAR
        PUBLIC      SerialEH

MOV AX, 0 ; clear AX
MOV BX, 0 ; clear BX

MOV DX, SERIAL_IIR ; address of interrupt identifying register
IN AL, DX ; read from interrupt identifying register to identify
          ; the interrupt that occurred
MOV BL, AL ; move value read into BX so that it can be used to
           ; index the SerialIRQTable
Call CS:SerialIRQTable[BX] ; the SerialIRQTable is indexed based on the
                           ; interrupt identifying register values.
                           ; each potential interrupt identifying
                           ; register value corresponds to an index
                           ; in this table that points to a function
                           ; that deals with the interrupt.

RET

SerialEH	ENDP


; LSI_Function
;
; Description: This function is called when an a line status interrupt
; occurs. This interrupt occurs when an error occurs. It updates the
; shared variable errorStatus and enqueues to the EventQueue AX, where
; AH is a constant representing that a Line Status Interrupt occurred,
; and AL is the value of the Line Status Register.
;
; Operational Description: This is done by resetting the errorStatus
; variable (because an error has just occurred),
; and setting the Register AL to the value of the Line Status
; Register, and AH to a constant indicating that a line status interrupt
; occurred. Finally, this function enqueues the register AX to the event queue.
;
; Arguments: None.
;
; Return Values: None.
;
; Global Variables: None.
; Shared Variables: None.
; Local Variables: None.
;
; Inputs: Serial
; Outputs: None.
; User Interface: None.
; Error Handling: None.
; Algorithms: None.
;
; Data Structures: None.
;
; Limitations: None.
; Known Bugs: None.
;
; Registers Changed: AX, DX
; Stack Depth: None.
;
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/18/16
;

LSI_Function      PROC        NEAR
        PUBLIC      LSI_Function

MOV AX, 0 ; clears AX register because will be reading in value from
          ; register into AL
MOV DX, SERIAL_LSR ; get location of line status register which tells
                   ; us if an error occurs.
IN AL, DX ; reads in value from line status register, stores in AL
AND AL, 00001110B ; mask non-error bits of line status register

MOV AH, LSI_CONST ; moves constant representing a line status interrupt
                ; occured

Call EnqueueEvent ; enqueues this event to show that this interrupt with
                  ; a value of AL in the LSR has occurred

RET
LSI_Function	ENDP


; Function Name: RDI_Function
; Description: This function is called when an a received data interrupt
; occurs. This interrupt occurs when the serial port received some
; information from another board or serial chip. It enqueues to the EventQueue
; AX, where AH is a constant representing that a Received Status Interrupt occurred,
; and AL is the value of the Receiver Buffer Register.
;
; Operational Description: This is done by calling SetSerial to
; set the error status variable, and then setting the Register AL to the
; value of the Receiver Buffer Register, and AH to a constant indicating that
; a received data interrupt occurred.
; Finally, this function enqueues the register AX to the event queue.
; This function also enqueues the information from the other board
; to the receiver queue.
;
; Arguments: None.
; Return Values: None.
;
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
; Data Structures: None
; Limitations: None.
; Known Bugs: None.
;
; Registers Changed: AX, DX
; Stack Depth: None.
;
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/18/16
;

RDI_Function      PROC        NEAR
        PUBLIC      RDI_Function

MOV AX, 0 ; clears AX register because will be reading in value from
          ; register into AL
MOV DX, SERIAL_RX_REG ; get location of received buffer register, which holds
                      ; data received from other devices

IN AL, DX ; reads in value in received buffer register (information from
          ; other devices)
MOV AH, RDI_CONST ; constant stating we have received data from another
                  ; device (received data interrupt)

Call EnqueueEvent ; enqueues this event to show that this interrupt with
                  ; a value of AL in the received buffer register
                  ; has occurred.

RET
RDI_Function	ENDP


; Function Name: TEI_Function
; Description: This function is called when an a transmitter empty interrupt
; occurs. This interrupt occurs when the serial port is ready to accept more
; data. If the TransmitterQueue is not empty, this queue will dequeue the
; the TransmitterQueue and send the information to the Transmitter
; Register. It also enqueues to the EventQueue AX, where AH is a constant
; representing that a Transmitter Empty Interrupt occurred, and AL
; is the value of the Transmitter Holding Register.
;
; Operational Description: This is called by setting AL to the Transmitter
; Holding Register value and AH to a constant indicating that a transmitter empty
; interrupt occurred. Then it enqueues AX to the EventQueue. It
; also dequeues the TransmitterQueue if the transmitting queue is not empty,
; and sends that information to Transmitter Holding Register
;
; Arguments: None.
;
; Return Values: None.
;
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
; Data Structures:  TransmitterQueue: a queue that holds characters to be
;                       sent to the data register in the 16C450, which performs
;                       serial to parallel conversion on data received
;                       from this queue.
;                    This structure defines a queue and all of its attributes:
;                       1) length: the total length that the queue can be
;                       2) element_size: if this variable is 1, the element
;                          size is word (2 bytes). if this variable is 2,
;                          the element size is one byte.
;                       3) front: describes the element of the array that is
;                          the front of the queue
;                       4) rear: describes the element of the array that is
;                          the back of the queue
;                       5) elements: an array with all the elements of the
;                          queue stored in it
;
; Limitations: None.
; Known Bugs: None.
;
; Registers Changed: SI, DX, AX
; Stack Depth: None.
;
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/18/16
;

TEI_Function      PROC        NEAR
        PUBLIC      TEI_Function

LEA SI, TransmitterQueue
Call QueueEmpty
JE CannotOutput ; if kickstart flag is set

OutputTxQueue:
  ; MOV KickStartFlag, 0
  LEA SI, TransmitterQueue
  %CRITICAL_START
  Call Dequeue ; dequeue transmitter queue
               ; outputs dequeued value to AH
  %CRITICAL_END
  MOV DX, SERIAL_TX_REG ; address of transmitter register
  ;XCHG AL, AH ; character stored in AH, move to AL
  OUT DX, AL ; moves a character from the transmitter queue to the transmitter
             ; holding register, which will send the character to another
             ; device
  JMP EndTEI_Function

CannotOutput:
  MOV KickStartFlag, 1

EndTEI_Function:
  RET

TEI_Function	ENDP


; Function Name: MSI_Function
; Description: This function deals with modem status interrupts. For now,
; it does nothing but return; we are not dealing with modem status
; interrupts at the moment.
;
; Operational Description: This function simply returns.
; Arguments: None.
;
; Return Values: None.
;
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
;
; Limitations: None.
; Known Bugs: None.
;
; Registers Changed: None.
; Stack Depth: None.
;
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/18/16
;

MSI_Function      PROC        NEAR
        PUBLIC      MSI_Function

RET

MSI_Function	ENDP
; _____________________________________________________________________________
; SerialIRQTable
;
; Description:      This table associates values of the interrupt
;                   identifying register with the address of functions
;                   to deal with registers.
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/12/16
SerialIRQTable       LABEL   WORD
        PUBLIC  SerialIRQTable

DW        offset(MSI_Function)   ; IIR = 0, Modem Status Interrupt. Call
                                 ; function to deal with that

DW        offset(TEI_Function)   ; IIR = 2, Transmitter Holding Register Empty
                                 ; Interrupt. Call function to deal with that

DW        offset(RDI_Function)  ; IIR = 4, Received Data Available interrupt.
                                ; Call function to deal with that

DW        offset(LSI_Function)  ; IIR = 6, Receiver Line Status interrupt.
                                ; Call function to deal with that


; _____________________________________________________________________________
; BaudRateTable
;
; Description:      This table contains the divisors needed to
;                   generate a 16x clock using a 1.8432 MHz crystal
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/12/16
BaudRateTable       LABEL   WORD
        PUBLIC  BaudRateTable

DW        10
DW        15
DW        30
DW        60
DW        80
DW        120
DW        160


CODE    ENDS


;the data segment

DATA    SEGMENT PUBLIC  'DATA'

TransmitterQueue QueueStruct <>  ; creates an instance of the queue structure   
                                 ; to be used to send characters to the
                                 ; transmitter holding receiver

KickStartFlag      DB    ?       ; states if we need to kick start the Interrupt
                                 ; enable register. set to 0 if we do not,
                                 ; and 1 if we do.

DATA    ENDS


END
