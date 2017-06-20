        NAME    MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 MAIN LOOP                                  ;
;                       Used to test Homework 7 (Serial )                    ;
;                                  EE/CS  51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:      This is the main loop used to execute serial code.
; 					It calls the initialization functions for the
;                   serial port. Once the event handler and interrupts are
;                   set up, this function can run infinitely. Serial data
; 					is displayed on a serial capture program
; Input:            None.
; Output:           to serial capture program
;
; Error Handling:   None. 
;
; Algorithms:       None.
; Data Structures:  None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;    11/18/16       Meera Krishnamoorthy    created file based on Homework 2
;                                           test code and modified



;definitions

CGROUP  GROUP   CODE
DGROUP  GROUP   DATA
     
CODE    SEGMENT PUBLIC 'CODE'
		ASSUME  CS:CGROUP, DS:DGROUP
		
		
        EXTRN   InitCS:NEAR
        EXTRN   ClrIRQVectors:NEAR
        EXTRN   Install_Serial_Handler:NEAR
        EXTRN   InitINT2:NEAR
        EXTRN   InitSerial:NEAR
		EXTRN   SerialIOTest:NEAR

        
;external function declarations


START:  

MAIN:
        CLI
        MOV     AX, STACK               ;initialize the stack pointer
        MOV     SS, AX
        MOV     SP, OFFSET(TopOfStack)

        MOV     AX, DGROUP                ;initialize the data segment
        MOV     DS, AX

        CALL    InitCS                  ;initialize the 80188 chip selects
                                        ;assumes LCS and UCS already setup

        CALL    ClrIRQVectors           ;clear (initialize) interrupt vector table

                                        
        Call    InitSerial              ;initialize the variables for the int2 
                                        ;event handler
                                        
        CALL    Install_Serial_Handler  ;install the event handler
                                        ;   ALWAYS install handlers before
                                        ;   allowing the hardware to interrupt.

        CALL    InitINT2                ;initialize interrupts via int2
        STI                             ;and finally allow interrupts.
		Call 	SerialIOTest           ; call function to test whether
									   ; serial code is working 
        Forever: JMP    Forever         ;sit in an infinite loop, nothing to
                                        ;do in the background routine
        HLT                             ;never executed (hopefully)

CODE ENDS
;the data segment. initialized because used later on. 

DATA    SEGMENT PUBLIC  'DATA'


DATA    ENDS


;the stack

STACK   SEGMENT STACK  'STACK'

                DB      80 DUP ('Stack ')       ;240 words

TopOfStack      LABEL   WORD

STACK   ENDS



END     START
