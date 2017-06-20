NAME    SERIAL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   SERIAL                                   ;
;     This file contains all functions necessary to transmit values to       ;
;                           other devices via serial port                    ;
;                                  EE/CS 51                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EDIT FROM 12/5/16
; WHAT I HAVE CHANGED:
; In SetSerialBaudRate
; 1) handle critical errors in SetSerialBaudRate by using the
;    macros CRITICAL_START and CRITICAL_END in the function
; 2) doesn't assume LCR value to be 0 at beginning
; 3) ensures AL always has LCR value when necessary
; 4) Ensures the right address is in DX when outputting/ inputting
; In SetSerialParity
; 1) clear/mask parity bits before or-ing
; In SerialPutChar
; 1) don't turn all interrupts on/off anymore (only turn the interrupts
;    that are currently on/off)
; In SerialEH
; 1) mask bits but last bit in IIR
; In LSI_FUNCTION
; 1) checked for errors after masking non-error related bits



; Overall Description: This function contains all the necessary functions
; to transmit values to other devices via serial port.
;
; Table of Contents:
; 1) InitSerial: initializes all the registers and values needed to transmit
; and receive data
; 2) SetSerialBaudRate: changes baud rate( by changing baud divisor)
; if bit in LCR is set to allow the baud rate to be changed. The baud
; rate is the rate at whcih the information is transferred in a communication
; channel. 
; 3) SetSerialParity: sets parity to be on or off depending on 
; input from init function. 
; 4) SerialEH: directs event handler to appropriate function to deal
; with current interrupt.
; 5) LSI_Function: function to deal with line status interrupts
; 6) RDI_Function: function to deal with received data interrupts
; 7) TEI_Function: function to deal with transmitter empty interrupts
; 8) MSI_Function: function to deal with modem status interrupts
; 9) SerialIRQTable: table called in SerialEH that has addresses to 
; functions to deal with interrupts. the index of this table is based
; on the interrupt identification register's values for each interrupt. 
; 10) BaudDivisorTable: lists several baud divisors corresponding
; to a 9.216 MHz clock input. 
;
; Revision History:
;    11/18/16  Meera Krishnamoorthy   wrote code
;    12/8/16   Meera Krishnamoorthy   documented edited code


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
; TransmitterQueue, the baud rate, parity, the type of output of the serial, 
; and the type of interrupts that are enabled. It also sets the kickstart
; flag appropriately so that kickstarting will occur when something
; is added to the TransmitterQueue (because it is initialized to be empty). 
;
; Operation: The TransmitterQueue is initialized using the QueueInit function
; from the queues.asm file, which initializes all variables associated
; with the queue. Then, it sets the values in the Line Control Register 
; associated with the baud rate and parity (using external functions). It
; also sets the appropriate bits in the interrupt enable register to 
; enable all interrupts. Finally, it sets the kickstart flag to 1 to ensures
; kickstarting occurs when a character is enqueued to the TransmitterQueue. 
;
; Arguments: None.
; Return Value: None.
;
; Local Variables: None.
; Shared Variables:
; Global Variables: None.
;
; Input: None.
; Output: to line status register and interrupt enable register
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
; Registers Changed: AX, BX, DX, SI
; Stack Depth: None.
;
; Limitations: None. 
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

ClearLCR: 
	MOV AX, 0
	MOV DX, SERIAL_LCR  ; set the address of DX to the LCR
	OUT DX, AL ; clears out the current value of the LCR Register

SetBaudRate: 
	MOV BX, BAUD_DIVISOR_INDEX ; this is the index of the table containing
						   ; baud divisors of the baud divisor we want
						   ; to use
	Call SetSerialBaudRate ; call function to set the baud rate (by setting
						   ; the baud divisor) 
    MOV AL, 0
    OUT DX, AL
SetParity: 
	MOV BL, EVEN_PARITY ; BX = value to be or-ed to lcr to indicate parity setting
	Call SetSerialParity ; call function that will set parity if BX = 
						 ; PARITY_ON
	
SetOtherPartsLCR:
    IN AL, DX ; get LCR value
	OR AL, TRANSMIT8CHAR ; set LCR so that outputs 8 bits at a time
	OR AL, MORE_1_STOP_BIT ; set LCR to have more than 1 stop bit
	
	MOV DX, SERIAL_LCR  ; set the address of DX to the LCR (in event
						; it was elsewhere changed) 
	OUT DX, AL; stores this new value in the line control register

SetIER: ; set the INTERRUPT ENABLE REGISTER (IER)
	MOV AL, MODEM_STATUS ; sets bit in IER to generate modem status interrupts 
	OR AL, RECEIVER_LINE_STATUS ; sets in IER bit to generate receiver line 
								; status interrupts
	OR AL, THRE ; sets bit in IER  to generate transmitter holding register
			    ; empty interrupts (ready for data to be sent to transmitter
			    ; holding register) 
	OR AL, RECEIVED_DATA_AVAILABLE ; sets bit in IER to generate received
								   ; data interrupts (data ready to be
								   ; taken in) 

	MOV DX, SERIAL_IER ; set DX to be address of interrupt enable register

	OUT DX, AL ; output value to interrupt enable register to enable all
			  ; interrupts

MOV KickStartFlag, 1 ; used to determine the need for kickstarting.
					 ; kickstarting is needed if the TransmitterQueue is 
					 ; empty and values needed to be added to it in the
					 ; SerialPutChar function. If the flag is 1, then 
					 ; kickstarting needs to happen. If it's 0, kickstarting
					 ; does not need to happen. This
					 ; sets kick start flag to 1 because the TransmitterQueue
                     ; has no elements in it, so the IER needs to be
					 ; kickstarted to register that a character has been
					 ; added to the queue. 

RET
InitSerial	ENDP


; Function Name: SetSerialBaudRate
; Description: This function is called with BX as the index pointing to the baud
; rate divisor to be used in the baud rate table. It sets the value
; in the line control register that allows the baud rate to be changed,
; changes the baud rate, and then clears that value in the line
; control register so the baud rate cannot be changed. 
;
; Operational Description: It does this by or-ing the LCR value (in
; AL) with a constant DIVISOR_LATCH, which sets the appropriate
; bit in the line control register to change the baud rate. Once
; this value is outputted to the line control register, the baud rate
; can be changed. This involves accessing the appropriate value in the
; BaudDivisorTable (an index predetermined by BX). It outputs the lower
; bit of the baud divisor in the DLL register and the higher bit of the
; baud divisor in the DLM register. Then, it masks the appropriate bit 
; of the line control register to prevent the baud rate from being changed. 
; This is done by and-ing the current value of the AL with the inverse
; of the DIVISOR_LATCH. Since DIVISOR_LATCH only has the bit set 
; to set the baud rate, and-ing any value with the inverse of DIVISOR_LATCH 
; would cause 
; 
; Arguments: BX (index pointing to baud rate divisor to be used in baud rate
; 				table): R, 8 bits, byte	
; Return Values: None.
; Global Variables: None.
; Shared Variables: None. 
; Local Variables: None.
;
; Inputs: None.
; Outputs: to line status register
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
SetDLAB: 
    %CRITICAL_START ; this uses a macro to disable interrupts
				; as this part of the code runs so that it doesn't 
				; get interrupted (in the event that this code
				; includes critical code) 
    MOV DX, SERIAL_LCR  ; set the address of DX to the LCR (in event
						; it was elsewhere changed) 
    IN AL, DX ; get LCR value
    OR AL, DIVISOR_LATCH ; sets bit in LCR to change baud rate
    OUT DX, AL ; outputs those bits to the LCR

ChangeBaudDivisor:
    MOV AX, CS:BaudDivisorTable[BX] ; get baud divisor associated with
									; index set in BX

    MOV DX, DLL ; set DLL register address to DX
    OUT DX, AL ; output low byte of baud divisor to DLL
  
    MOV DX, DLM ; set DLM register address to DX
    XCHG AH, AL ; put high byte of AX into low byte (because out only outputs
				; a value in AL)
    OUT DX, AL ; output high byte of baud divisor to DLM 

ResetDLAB:
    MOV DX, SERIAL_LCR  ; set the address of DX to the LCR (in event
						; it was elsewhere changed) 
    IN AL, DX ; get LCR value
    AND AL, NOT(DIVISOR_LATCH) ; masks appropriate bit in LCR
							   ; so that baud rate cannot
							   ; be changed. because the bit is set in 
							   ; DIVISOR_LATCH, the inverse of DIVISOR_LATCH
							   ; has that bit not set, so anding any value with
							   ; NOT(DIVISOR_LATCH) causes the bit associatd
							   ; with setting the baud rate to be masked
    OUT DX, AL ; sets baud rate permanently (makes it so that baud
			   ; rate can no longer be changed
    ; AL now has the current value of baud rate, so it can be or-ed with
	; other bit patterns and outputted to the line control register
EndSetSerialBaudRate:
  %CRITICAL_END ; this turns interrupts back on 
  RET
SetSerialBaudRate	ENDP

; Function Name: SetSerialParity
; Description: This function is called with DX as the address of the 
; line control register and BX as a bit pattern to be or-ed with the LCR to 
; enable a specific type of parity. 
; This value is sent to the line control register to turn a specific type of 
; parity on/
;
; Operational Description: The LCR is read in and the parity
; bits of the LCR (bits that control parity) are masked
; so a new parity setting can be set. Then, the current
; value in the line control register is or-ed with BX to enable a specific
; type of parity. Then that value is sent to the line control register. 
; 
; Arguments: BX: value to or to LCR to get appropriat parity settings
;            DX: LCR register address
; Return Values: None.
; Global Variables: None.
; Shared Variables: None. 
; Local Variables: None.
;
; Inputs: None.
; Outputs: to line status register
; User Interface: None.
; Error Handling: None.
; Algorithms: None.
;
; Data Structures: None.
; Known Bugs: None.
;
; Registers Changed: DX, AX, BX
; Stack Depth: None.
; Limitations: None. 
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/18/16
;

SetSerialParity      PROC        NEAR
        PUBLIC      SetSerialParity
CheckParity: 
  IN AL, DX ; get LCR value

  AND AL, MASK_PARITY     
 
  OR AL, BL ; if it does, AL (current value of LCR) is or-ed with
			 ; sent in bit pattern to enable a specific type of parity
  OUT DX, AL ; output this value to the line control register
  
  RET
SetSerialParity	ENDP


; Function Name: SerialPutChar
; Description: This function outputs the passed character (c) to
; the serial channel. It returns with the carry flag reset if the character
; has been "output", (put in the channel's queue, not necessarily sent over
; the serial channel) and set otherwise (transmit queue is full).
; The character c is passed in value in AL. ; If the kickstart flag is set 
; and enqueueing occurs, kickstarting will ensue.
; Kickstarting occurs because we want the system to acknowledge a value
; has been enqueued to the TransmitterQueue, and it might not be if 
; too many interrupts are happening for the system to process this information,
; and send it to the transmitter holding register. 
;
; Operational Description: This is done using the written functions
; QueueFull and Enqueue. If the TransmitterQueue is Full (if the zero flag is set
; by the QueueFull function), nothing will be enqueued and the carry
; flag will be set.  Else, "c" will be enqueued and the carry flag is reset.
; If the kickstart flag is set and enqueueing occurs, then the kickstart
; flag will be reset and the interrupt enable register will be kickstarted
; (all interrupts will be turned off and then back on). 
;
; Arguments: AL, register that holds the character (c) to be sent to the
; serial channel
;
; Return Values: carry flag: set based on whether the character could be 
;                enqueued to the transmit queue
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
; Registers Changed: SI, carry flag, AX, DX
; Stack Depth: 1 word
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/18/16

SerialPutChar      PROC        NEAR
        PUBLIC      SerialPutChar
        
%CRITICAL_START ; this uses a macro to disable interrupts
				  ; as this part of the code runs so that it doesn't 
				  ; get interrupted (in the event that this code
				  ; includes critical code)
LEA SI, TransmitterQueue ; sets SI to address of queue
PUSH AX  ; saves AX which stores value to be outputted to queue because
		 ; the QueueFull function changes the value of AX
Call QueueFull  ; checks if queue is full
POP AX
%CRITICAL_END  ; turns on interrupts
 
JNZ CanEnqueue ; if queue not full, can enqueue value to it

CannotEnqueue: ; if queue full, cannot enqueue to it
  STC ; sets carry flag
  JMP EndSerialPutChar ; leave function

CanEnqueue:
  LEA SI, TransmitterQueue ; reset SI in case it has been changed since
  %CRITICAL_START ; this uses a macro to disable interrupts
				  ; as this part of the code runs so that it doesn't 
				  ; get interrupted (in the event that this code
				  ; includes critical code) 
  Call Enqueue ; enqueues AL to the TransmitterQueue
  %CRITICAL_END  ; turns on interrupts
  CMP KickStartFlag, 1 ; sees if kickstarting is needed (if kickstart
					   ; flag is 1) 
  JNE EnqueueSetFlag ; if it is not, can move on to clear flag
  
KickStart:
   %CRITICAL_START ; this uses a macro to disable interrupts
				  ; as this part of the code runs so that it doesn't 
				  ; get interrupted (in the event that this code
				  ; includes critical code) 
  MOV KickStartFlag, 0 ; since the queue is no longer empty, do not 
					   ; need to kickstart, so set kickstart to 0

  
  MOV DX, SERIAL_IER  ; set the address to the interrupt enable register
					 ; (will be changing values in this register in order
					 ; to shock system) 
  IN AL, DX ; save current value of interrupt enable register
  MOV CL, AL ; put saved value in CL
  MOV AL, 0  ; disable interrupts (bits set in AL determine interrupts
			 ; that are turned on) 
  OUT DX, AL 

  ; re-enable interrupts
  MOV AL, CL; get back saved value of interrupt enable register
  OUT DX, AL ; sends new value to AL to make sure interrupts are enabled again
  %CRITICAL_END  ; turns on interrupts

EnqueueSetFlag:
  CLC ; clear carry flag (so system knows value has been enqueued) 
  
EndSerialPutChar:
  RET

SerialPutChar	ENDP


; Function Name: SerialEH
; Description: This function is called when any interrupt occurs. It
; finds out what interrupt has occurred, and then determines how to proceed
; by using a table with functions written to deal with each interrupt
; that could occur. The IIR has four potential values: 0, 2, 4, and 6.
; Those values are indexes in the tableSerialIRQTable -- this table has addresses
; of the functions that deal with these interrupts at indexes that match
; those interrupts' IIR value. In each function, I read in the register
; corresponding to each interrupt and enqueue a value to the EventQueue
; declaring that an event occurred. 
;
; Operational Description: This function is called when an interrupt occurs,
; and checks the interrupt identifying register (IIR) to determine what kind
; of interrupt occurred. Once the function has identified the type of
; interrupt that occurred, it looks up the associated register in
; SerialIRQTable, a call table that associates IIR values with addresses of
; functions written to deal with the interrupts.
; If the IIR is 1, then we can exit out of this function because no 
; interrupts are occurring.If it is not, we must keep looping to check for 
; interrupts. 
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

MOV AX, 0 ; clear AX, will hold value from interrupt identifying register
MOV BX, 0 ; clear BX, will hold index of SerialIRQTable to go to, 
		  ; which determines function to go to to deal with interrupt

CheckInterrupts: 
	MOV DX, SERIAL_IIR ; address of interrupt identifying register
	IN AL, DX ; read from interrupt identifying register to identify
			  ; the interrupt that occurred
    MOV CL, AL ; save value of interrupt identifying register
    AND CL, NO_INTERRUPT ; mask all bits but bit 0 before check 
	CMP CL, NO_INTERRUPT ; there is a value of the IIR (NO_INTERRUPT)
						 ; that indicates there are no interrupts, which
						 ; will allow us to exit this function. if the IIR
						 ; is this value, we can exit the function						 
	JE EndSerialEH
	MOV BL, AL ; move value read into BX so that it can be used to
			   ; index the SerialIRQTable.
	Call CS:SerialIRQTable[BX] ; the SerialIRQTable is indexed based on the
							   ; interrupt identifying register values.
							   ; each potential interrupt identifying
                               ; register value corresponds to an index
                               ; in this table that points to a function
                               ; that deals with the interrupts.
	JMP CheckInterrupts	; continue to loop to check if interrupts
						; are there 

EndSerialEH: 
	RET

SerialEH	ENDP


; LSI_Function
;
; Description: This function is called when an a line status interrupt
; occurs. This interrupt occurs when an error occurs. It enqueues AX
; to the event queue, where AH is a constant representing that a Line Status 
; Interrupt occurred, and AL is the value of the Line Status Register.
;
; Operational Description: This is done by setting AL to the value 
; of the Line Status Register, and AH to a constant indicating that a line 
; status interrupt occurred. This function enqueues the register AX to the 
; event queue.
;
; Arguments: None.
;
; Return Values: None.
;
; Global Variables: None.
; Shared Variables: None.
; Local Variables: None.
;
; Inputs: (from line status register) 
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
AND AL, MASK_NON_ERROR ; mask non-error bits of line status register
CMP AL, 0 ; check if there is an error after masking non error bits
JE EndLSI_Function

MOV AH, LSI_CONST ; moves constant representing a line status interrupt
                ; occurred

Call EnqueueEvent ; enqueues this event to show that this interrupt with
                  ; a value of AL in the LSR has occurred

EndLSI_Function: 
    RET
LSI_Function	ENDP


; Function Name: RDI_Function
; Description: This function is called when an a received data interrupt
; occurs. This interrupt occurs when the serial port received some
; information from another board or serial chip. It enqueues to the EventQueue
; AX, where AH is a constant representing that a Received Status Interrupt occurred,
; and AL is the value of the Receiver Buffer Register.
;
; Operational Description: This is done by setting the register AL to the
; value of the Receiver Buffer Register, and AH to a constant indicating that
; a received data interrupt occurred.
; Finally, this function enqueues the register AX to the event queue.
;
; Arguments: None.
; Return Values: None.
;
; Global Variables: None.
; Shared Variables: None.
; Local Variables: None.
;
; Inputs: (from the receiver buffer register) 
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
; data. If the TransmitterQueue is empty, the kickstart flag is set, indicating
; kickstarting will need to happen when a character is enqueued to the 
; transmitter queue. 
; If the TransmitterQueue is not empty, this queue will dequeue the
; the TransmitterQueue and send the information to the Transmitter
; Register. It also enqueues to the EventQueue AX, where AH is a constant
; representing that a Transmitter Empty Interrupt occurred, and AL
; is the value of the Transmitter Holding Register.
;
; Operational Description: This is called by setting AL to the Transmitter
; Holding Register value and AH to a constant indicating that a transmitter empty
; interrupt occurred. Then it enqueues AX to the EventQueue. It
; also dequeues the TransmitterQueue if the transmitting queue is not empty,
; and sends that information to Transmitter Holding Register. Finally, 
; if the TransmitterQueue is empty, the kickstart flag is set to indicate
; that kickstarting will need to occur when a character is enqueued to
; the TransmitterQueue. 
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
; Outputs: (to the transmitter holding register) 
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
; Registers Changed: SI, DX, AX, flags
; Stack Depth: None.
;
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/18/16
;

TEI_Function      PROC        NEAR
        PUBLIC      TEI_Function

LEA SI, TransmitterQueue ; store address of TransmitterQueue in SI so that
						 ; it can be accessed in QueueEmpty function
Call QueueEmpty ; check if TransmitterQueue is empty 
JE CannotOutput ; if kickstart flag is set

OutputTxQueue:
  LEA SI, TransmitterQueue ; ensures that the TransmitterQueue address
						   ; is in SI
  %CRITICAL_START ; this uses a macro to disable interrupts
				; as this part of the code runs so that it doesn't 
				; get interrupted (in the event that this code
				; includes critical code) 
  Call Dequeue ; dequeue transmitter queue
               ; outputs dequeued value to AH
  %CRITICAL_END ; sets interrupts to occur again
  MOV DX, SERIAL_TX_REG ; address of transmitter register
  OUT DX, AL ; moves a character from the transmitter queue to the transmitter
             ; holding register, which will send the character to another
             ; device
  JMP EndTEI_Function ; jump to the end

CannotOutput:
  MOV KickStartFlag, 1 ; if cannot output, set kickstart flag to indicate
					   ; kickstarting must occur 

EndTEI_Function:
  RET

TEI_Function	ENDP


; Function Name: MSI_Function
; Description: This function deals with modem status interrupts. For now,
; it does nothing but read in the modem status register and return --
; we are not dealing with modem status interrupts. 
;
; Operational Description: This function reads the modem status register
; and returns. 
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
; Registers Changed: AX, DX
; Stack Depth: None.
;
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/18/16
;

MSI_Function      PROC        NEAR
        PUBLIC      MSI_Function
MOV DX, SERIAL_MSR ; read in register to clear it so the interrupt
                   ; is not set forever
IN AL, DX
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
; BaudDivisorTable
;
; Description:      This table contains the divisors needed to
;                   generate a 16x clock using a 9.216 MHz clock input
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/12/16
BaudDivisorTable       LABEL   WORD
        PUBLIC  BaudDivisorTable

DW        10
DW        15
DW        30
DW        60
DW        80
DW        120
DW        160
DW        240
DW        290
DW        320
DW        480
DW        960
DW        1920
DW        3840
DW        4285
DW        5235



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
