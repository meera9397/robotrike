        NAME    MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 MAIN LOOP                                  ;
;                       Used to test Homework 4 (Display)                    ;
;                                  EE/CS  51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:      This is the main loop used to execute the display code.
;                   It calls initialization functions to get the display ready
;                   for the event handler. Once the event handler and timer
;                   (which drives the event handler) are set up,
;                   DisplayTest, a function written to test this code,
;                   is called. It runs some basic tests on the display
;                   to ensure that it is working properly.
;                   After DisplayTest finishes, one can write to print
;                   on the display by using "regs AX ____" where the blank
;                   can be filled by any decimal/hexadecimal number. 
; Input:            None.
; Output:           None.
;
; User Interface:   No real user interface.
; Error Handling:   None. 
;
; Algorithms:       None.
; Data Structures:  None.
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;    10/22/16       Meera Krishnamoorthy    created file based on Homework 2
;                                           test code and modified



;definitions

CGROUP  GROUP   CODE
DGROUP  GROUP   DATA
     
CODE    SEGMENT PUBLIC 'CODE'
        EXTRN   InitCS:NEAR
        EXTRN   ClrIRQVectors:NEAR
        EXTRN   InstallHandler:NEAR
        EXTRN   InitTimer:Near
        EXTRN   InitVariables:NEAR
        EXTRN   DisplayTest:NEAR
        ASSUME  CS:CGROUP, DS:DGROUP
        
;external function declarations


START:  

MAIN:
        MOV     AX, STACK               ;initialize the stack pointer
        MOV     SS, AX
        MOV     SP, OFFSET(TopOfStack)

        MOV     AX, DATA                ;initialize the data segment
        MOV     DS, AX


        CALL    InitCS                  ;initialize the 80188 chip selects
                                        ;assumes LCS and UCS already setup

        CALL    ClrIRQVectors           ;clear (initialize) interrupt vector table

                                        
        Call    InitVariables           ;initialize the variables for the timer 
                                        ;event handler

        CALL    InstallHandler          ;install the event handler
                                        ;   ALWAYS install handlers before
                                        ;   allowing the hardware to interrupt.

        CALL    InitTimer               ;initialize the internal timer
        STI                             ;and finally allow interrupts.
        CALL    DisplayTest
        Forever: JMP    Forever         ;sit in an infinite loop, nothing to
                                        ;do in the background routine
        HLT                             ;never executed (hopefully)

CODE ENDS

;the data segment

DATA    SEGMENT PUBLIC  'DATA'


DATA    ENDS




;the stack

STACK   SEGMENT STACK  'STACK'

                DB      80 DUP ('Stack ')       ;240 words

TopOfStack      LABEL   WORD

STACK   ENDS



END     START
