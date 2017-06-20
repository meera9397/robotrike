       NAME  TIMER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   TIMER                                    ;
;                     Timer Initialization Function to set up event          ;
;                                   handler                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:      This program contains the function to initialize the timer.
; Table of Contents: 
; 1) InitTimer: initializes 80188 timers. 
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
$INCLUDE(EH_TI.INC)


CGROUP  GROUP   CODE

CODE SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP
        

; InitTimer
;
; Description:       Initialize the 80188 Timers.  The timers are initialized
;                    to generate interrupts every MS_PER_SEG milliseconds.
;                    The interrupt controller is also initialized to allow the
;                    timer interrupts.  Timer #2 is used to prescale the
;                    internal clock from 2.304 MHz to 1 KHz.  Timer #0 then
;                    counts MS_PER_SEG timer #2 intervals to generate the
;                    interrupts.
;
; Operation:         The appropriate values are written to the timer control
;                    registers in the PCB.  Also, the timer count registers
;                    are reset to zero.  Finally, the interrupt controller is
;                    setup to accept timer interrupts and any pending
;                    interrupts are cleared by sending a TimerEOI to the
;                    interrupt controller.
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
; Author:            Glen George
; Last Modified:     Oct. 29, 1997

InitTimer       PROC    NEAR
PUBLIC      InitTimer
                                ;initialize Timer #0 for MS_PER_SEG ms interrupts
        MOV     DX, Tmr0Count   ;initialize the count register to 0
        XOR     AX, AX
        OUT     DX, AL

        MOV     DX, Tmr0MaxCntA ;setup max count for milliseconds per segment
        MOV     AX, MS_PER_SEG  ;   count so can time the segments
        OUT     DX, AL

        MOV     DX, Tmr0Ctrl    ;setup the control register, interrupts on
        MOV     AX, Tmr0CtrlVal
        OUT     DX, AL

                                ;initialize interrupt controller for timers
        MOV     DX, INTCtrlrCtrl;setup the interrupt control register
        MOV     AX, INTCtrlrCVal
        OUT     DX, AL

        MOV     DX, INTCtrlrEOI ;send a timer EOI (to clear out controller)
        MOV     AX, TimerEOI
        OUT     DX, AL


        RET                     ;done so return


InitTimer       ENDP


CODE ENDS


END