        NAME    MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 MAIN LOOP                                  ;
;                       Used to test Homework 5 (Keypad)                    ;
;                                  EE/CS  51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:      This is the main loop used to execute keypad and 
;                   display code. It calls the initialization functions for the
;                   display and keypad. Once the event handler and timer are
;                   set up, this function can run infinitely. Keys are pressed
;                   on the keypad which are stored in the string buffer/ 
;                   and displayed on the display. 
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
        EXTRN   InstallKeypadDisplayHandler:NEAR
        EXTRN   InitTimer:Near
        EXTRN   InitDisplay:NEAR
        EXTRN   InitKeypad:NEAR
        ASSUME  CS:CGROUP, DS:DGROUP
        EXTRN   KeyTest:NEAR
        
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

                                        
        Call    InitDisplay           ;initialize the variables for the timer 
                                        ;event handler
        Call    InitKeypad

        CALL    InstallKeypadDisplayHandler          ;install the event handler
                                        ;   ALWAYS install handlers before
                                        ;   allowing the hardware to interrupt.

        CALL    InitTimer               ;initialize the internal timer
        STI                             ;and finally allow interrupts.
        ;CALL    KeyTest
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
