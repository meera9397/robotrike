       NAME  EH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    EH                                      ;
;                     Event Handler related functions                        ;
;                 used to activate display output and keypad input           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:      This program contains functions to initialize and
;                   deal with the timer event handler and illegal
;                   event handlers
; Table of Contents:
; 1) KeypadDisplayHandler: calls MuxSegPatterns (function that outputs segment
;                       patterns corresponding to a string/number to
;                       the display) and Scan (function that scans keyboard
;                       and reports input)and sends an EOI to the event handler 
;                       when done. 
; 2) InstallKeypadDisplayHandler: installs event handler for timer interrupt
; 3) PWM_EventHandler: calls function to deal with pwm events
; 4) Install_PWM_Handler: installs event handler for timer interrupt
; 5) Serial_EventHandler: calls function to deal with serial interrupts
;						  and then sends an EOI when done. 
; 6) Install_Serial_Handler: installs event handler for INT2 interrupt
; 7) IllegalEventHandler: event handler for illegal interrupts -- does nothing
;                         but send no specific EOI
; 8) InitCS: initializes peripheral chip selects on 80188
; 9) ClrIRQVectors: installs illegal event handler for all interrupts in
;                   interrupt vector table
;
; Input:            None.
; Output:           None.
;
; User Interface:   None. 
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Revision History:
;      10/30/16  Meera Krishnamoorthy   used functions written by Glen George
;                                       to use for event handling/ timers


; local include files
$INCLUDE(EH.INC)
$INCLUDE(TIMER.INC)
$INCLUDE(IRQ.INC)
$INCLUDE(DISPLAY.INC)
$INCLUDE(INT2.INC)


CGROUP  GROUP   CODE

CODE SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP
        EXTRN   MuxSegPatterns:NEAR
        EXTRN   Scan:NEAR
        EXTRN   SerialEH:NEAR

; KeypadDisplayHandler
;
; Description:       This procedure is the event handler for the timer
;                    interrupt.  It first saves the registers, and then
;                    calls "MuxSegmentPatterns", a function defined in
;                    displays.asm that puts the segment patterns of each
;                    character int he string on the display. Finally, it 
;                    sends an EOI to the event handler and restorest eh register. 
;
; Operation:         First, the function puts the registers on the stack. Then,
;                    it calls MuxSegPatterns to display the segments on the 
;                    display. Finally it sends the EOI to the event handler and
;                    restores the registers. 

;
; Arguments:         None.
; Return Value:      None.
;
; Local Variables:   None.
; Shared Variables:  None.
; Global Variables:  None.
;
; Input:             None.
; Output:            None.
;
; Error Handling:    None.
;
; Algorithms:        None.
; Data Structures:   None.
;
; Registers Changed: None
; Stack Depth:       3 words
;
; Author:            Meera Krishnamoorthy
; Last Modified:     12/3/2016

KeypadDisplayHandler       PROC    NEAR

StartEventHandler1:

        PUSH    AX                      ;save the registers
        PUSH    BX                      ;Event Handlers should NEVER change
        PUSH    DX                      ;any register values

Call MuxSegPatterns                     ; calls function to display segment
                                        ; patterns on the display one at a time
                                        ; but very quickly (muxing)
Call Scan                               ; calls function to get inputs from
                                        ;keypad


EndTimerEventHandler1:                   

        MOV     DX, INTCtrlrEOI       ;send the EOI to the interrupt controller
        MOV     AX, TimerEOI
        OUT     DX, AL

        POP     DX                      ;restore the registers
        POP     BX
        POP     AX


        IRET                  ;and return (Event Handlers end with IRET not RET)

KeypadDisplayHandler       ENDP

; InstallKeypadDisplayHandler
;
; Description:       Install the event handler for the timer interrupt.
;
; Operation:         Writes the address of the timer event handler to the
;                    appropriate interrupt vector.
;
; Arguments:         None.
; Return Value:      None.
;
; Local Variables:   None.
; Shared Variables:  None.
; Global Variables:  None.
;
; Input:             None.
; Output:            None.
;
; Error Handling:    None.
;
; Algorithms:        None.
; Data Structures:   None.
;
; Registers Changed: flags, AX, ES
; Stack Depth:       0 words
;
; Author:            Glen George
; Last Modified:     Jan. 28, 2002

InstallKeypadDisplayHandler  PROC    NEAR
PUBLIC      InstallKeypadDisplayHandler


        XOR     AX, AX          ;clear ES (interrupt vectors are in segment 0)
        MOV     ES, AX
                                ;store the vector
        MOV     ES: WORD PTR (4 * Tmr0Vec), OFFSET(KeypadDisplayHandler)
        MOV     ES: WORD PTR (4 * Tmr0Vec + 2), SEG(KeypadDisplayHandler)


        RET                     ;all done, return


InstallKeypadDisplayHandler  ENDP


; Serial_EventHandler
;
; Description: This procedure is the event handler for each INT2 interrupt.
; 			   It calls "SerialEH", a function defined in serial.asm
; 			   that determines what interrupt occurred, and how to 
; 			   proceed based on the interrupt that occurred. Finally, it sends 
; 			   an EOI to the event handler and restores the registers. 
;
; Operation: First, the function puts the registers on the stack. Then, it
; 			 calls SerialEH to determine the interrupts that occurred and
; 			 what to do based on which interrupt occured. Finally, it sends the 
; 		     EOI to the event handler and restores the registers. 
;
; Arguments:         None.
; Return Value:      None.
;
; Local Variables:   None.
; Shared Variables:  None.
; Global Variables:  None.
;
; Input:             None.
; Output:            None.
;
; Error Handling:    None.
;
; Algorithms:        None.
; Data Structures:   None.
;
; Registers Changed: None
; Stack Depth:       4 words
;
; Author:            Meera Krishnamoorthy
; Last modified: 	 11/19/16

Serial_EventHandler       PROC    NEAR

StartEventHandler3:

        PUSH    AX                      ;save the registers
        PUSH    BX                      ;Event Handlers should NEVER change
        PUSH    DX                      ;any register values
		PUSH 	SI
		
Call SerialEH     ; calls function to determine function of each motor               

EndSerial_EventHandler:                   

        MOV     DX, INTCtrlrEOI       ;send the EOI to the interrupt controller
        MOV     AX, Int2EOI
        OUT     DX, AL

	    POP 	SI
        POP     DX                      ;restore the registers
        POP     BX
        POP     AX
		

        IRET                  ;and return (Event Handlers end with IRET not RET)


Serial_EventHandler       ENDP

; Install_Serial_Handler
;
; Description:       Install the serial event handler for the INT2 interrupt.
;
; Operation:         Writes the address of the INT2 event handler to the
;                    appropriate interrupt vector.
;
; Arguments:         None.
; Return Value:      None.
;
; Local Variables:   None.
; Shared Variables:  None.
; Global Variables:  None.
;
; Input:             None.
; Output:            None.
;
; Error Handling:    None.
;
; Algorithms:        None.
; Data Structures:   None.
;
; Registers Changed: flags, AX, ES
; Stack Depth:       0 words
;
; Author:            Meera Krishnamoorthy
; Last Modified:     11/19/2016

Install_Serial_Handler  PROC    NEAR
PUBLIC      Install_Serial_Handler


        XOR     AX, AX          ;clear ES (interrupt vectors are in segment 0)
        MOV     ES, AX
                                ;store the vector
        MOV     ES: WORD PTR (4 * Int2Vec), OFFSET(Serial_EventHandler)
        MOV     ES: WORD PTR (4 * Int2Vec + 2), SEG(Serial_EventHandler)


        RET                     ;all done, return


Install_Serial_Handler  ENDP


; IllegalEventHandler
;
; Description:       This procedure is the event handler for illegal
;                    (uninitialized) interrupts.  It does nothing - it just
;                    returns after sending a non-specific EOI.
;
; Operation:         Send a non-specific EOI and return.
;
; Arguments:         None.
; Return Value:      None.
;
; Local Variables:   None.
; Shared Variables:  None.
; Global Variables:  None.
;
; Input:             None.
; Output:            None.
;
; Error Handling:    None.
;
; Algorithms:        None.
; Data Structures:   None.
;
; Registers Changed: None
; Stack Depth:       2 words
;
; Author:            Glen George
; Last Modified:     Dec. 25, 2000

IllegalEventHandler     PROC    NEAR
PUBLIC      IllegalEventHandler

        NOP                             ;do nothing (can set breakpoint here)

        PUSH    AX                      ;save the registers
        PUSH    DX

        MOV     DX, INTCtrlrEOI         ;send a non-sepecific EOI to the
        MOV     AX, NonSpecEOI          ;   interrupt controller to clear out
        OUT     DX, AL                  ;   the interrupt that got us here

        POP     DX                      ;restore the registers
        POP     AX

        IRET                            ;and return


IllegalEventHandler     ENDP


; ClrIRQVectors
;
; Description:      This functions installs the IllegalEventHandler for all
;                   interrupt vectors in the interrupt vector table.  Note
;                   that all 256 vectors are initialized so the code must be
;                   located above 400H.  The initialization skips  (does not
;                   initialize vectors) from vectors FIRST_RESERVED_VEC to
;                   LAST_RESERVED_VEC.
;
; Arguments:        None.
; Return Value:     None.
;
; Local Variables:  CX    - vector counter.
;                   ES:SI - pointer to vector table.
; Shared Variables: None.
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Used:   flags, AX, CX, SI, ES
; Stack Depth:      1 word
;
; Author:           Glen George
; Last Modified:    Feb. 8, 2002

ClrIRQVectors   PROC    NEAR
PUBLIC      ClrIRQVectors

InitClrVectorLoop:              ;setup to store the same handler 256 times

        XOR     AX, AX          ;clear ES (interrupt vectors are in segment 0)
        MOV     ES, AX
        MOV     SI, 0           ;initialize SI to skip RESERVED_VECS (4 bytes each)

        MOV     CX, 256         ;up to 256 vectors to initialize


ClrVectorLoop:                  ;loop clearing each vector
				;check if should store the vector
	CMP     SI, 4 * FIRST_RESERVED_VEC
	JB	DoStore		;if before start of reserved field - store it
	CMP	SI, 4 * LAST_RESERVED_VEC
	JBE	DoneStore	;if in the reserved vectors - don't store it
	;JA	DoStore		;otherwise past them - so do the store

DoStore:                        ;store the vector
        MOV     ES: WORD PTR [SI], OFFSET(IllegalEventHandler)
        MOV     ES: WORD PTR [SI + 2], SEG(IllegalEventHandler)

DoneStore:			;done storing the vector
        ADD     SI, 4           ;update pointer to next vector

        LOOP    ClrVectorLoop   ;loop until have cleared all vectors
        ;JMP    EndClrIRQVectors;and all done


EndClrIRQVectors:               ;all done, return
        RET


ClrIRQVectors   ENDP


CODE ENDS


END