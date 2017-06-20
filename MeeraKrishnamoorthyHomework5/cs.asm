       NAME  CS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    CS                                   ;
;                     Event Handler/ Timer related functions                 ;
;                 used to display strings/ numbers on a display              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:      This program contains the event handler for the 
;                   display functions, a function to deal with illegal
;                   events, and several functions to initialize what is
;                   necessary for the event handler to run (the timer, illegal
;                   eventhandler, and the PCS)
; Table of Contents:
; 1) InitCS: initializes peripheral chip selects on 80188
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
$INCLUDE(CS.INC)


CGROUP  GROUP   CODE

CODE SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP

        EXTRN   MuxSegPatterns:NEAR
        EXTRN   Scan:NEAR

; InitCS
;
; Description:       Initialize the Peripheral Chip Selects on the 80188.
;
; Operation:         Write the initial values to the PACS and MPCS registers.
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
InitCS  PROC    NEAR
      PUBLIC      InitCS


        MOV     DX, PACSreg     ;setup to write to PACS register
        MOV     AX, PACSval
        OUT     DX, AL          ;write PACSval to PACS (base at 0, 3 wait states)

        MOV     DX, MPCSreg     ;setup to write to MPCS register
        MOV     AX, MPCSval
        OUT     DX, AL          ;write MPCSval to MPCS (I/O space, 3 wait states)


        RET                     ;done so return


InitCS  ENDP


CODE ENDS


END