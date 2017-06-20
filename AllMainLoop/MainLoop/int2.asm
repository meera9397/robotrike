       NAME  INT2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   INT2                                     ;
;       	    	Interrupt Enabler Function to set up event               ;
;                                   handler                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:      This program contains the function to enable the interrupts
; 					to be used to control event handlers. 
; Table of Contents: 
; 1) InitInt2: Sets up INT2 register to enable interrupts
;
; Revision History:
;      11/18/16  Meera Krishnamoorthy   wrote 


; local include files
$INCLUDE(EH.INC)
$INCLUDE(IRQ.INC)
$INCLUDE(INT2.INC )

CGROUP  GROUP   CODE

CODE SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP
        

; InitINT2
;
; Description: Initializes I2CON interrupt control register, which 
; serves as a control register for non-cascadable external interrupt
; pins. This register needs to be set so that interrupts are allowed
; to occur and run the event handler.
; This function sets the I2CON register up with a certain trigger mode,
; interrupt mask, and priority level. It also sends an EOI to 
; clear out the controller. 
;
; Operation: This does this by writing to the address of the ICON2 
; register to initialize interrupts, and to the EOI register to clear
; the in-service bit.          
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
; Registers Changed: AX, DX
; Stack Depth:       0 words
;
; Author:            Meera Krishnamoorthy 
; Last Modified:     11/19/2016

InitINT2       PROC    NEAR
PUBLIC      InitINT2
        MOV     DX, I2CON ;sets up the interrupt control register
        MOV     AL, ICON2Val ; moves appropriate value to it
        OUT     DX, AL ; outputs value 

        MOV     DX, INTCtrlrEOI ; address of EOI register
        MOV     AX, Int2EOI ; value to send to EOI register
						    ; to clear in-service bit
        OUT     DX, AL ;send an Int2 EOI (to clear out controller)

        RET         


InitINT2       ENDP


CODE ENDS


END