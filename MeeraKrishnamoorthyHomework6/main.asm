        NAME    MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 MAIN LOOP                                  ;
;                       Used to test Homework 6 (Motors )                    ;
;                                  EE/CS  51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description: This is the loop used to test the motor functions in
; motor.asm. 
;
; Revision History:
;    10/22/16       Meera Krishnamoorthy    created file based on Homework 2
;                                           test code and modified



;definitions

CGROUP  GROUP   CODE
DGROUP  GROUP   DATA
     
CODE    SEGMENT PUBLIC 'CODE'
		ASSUME  CS:CGROUP, DS:DGROUP
		
		
        EXTRN   InitCS:NEAR
        EXTRN   ClrIRQVectors:NEAR
        EXTRN   Install_PWM_Handler:NEAR
        EXTRN   InitTimer:NEAR
        EXTRN   InitMotor:NEAR
		EXTRN   MotorTest:NEAR

        
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

                                        
        Call    InitMotor              ;initialize the variables for the timer 
                                        ;event handler
                                        
        CALL    Install_PWM_Handler          ;install the event handler
                                        ;   ALWAYS install handlers before
                                        ;   allowing the hardware to interrupt.

        CALL    InitTimer               ;initialize the internal timer
        STI                             ;and finally allow interrupts.
		Call 	MotorTest
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
