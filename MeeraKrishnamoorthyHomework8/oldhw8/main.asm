        NAME    MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   HW8TEST                                  ;
;                            Homework #8 Test Code                           ;
;                                  EE/CS  51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:      This program tests the parser functions for Homework
;                   #8.  It calls an external function (ParseTest) to that
;                   tests all functions in parser.asm and phelp.asm to 
;                   ensure that accurate parsing occurs. 

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
DGROUP  GROUP   DATA, STACK

CODE    SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP, DS:DGROUP



;external function declarations

        EXTRN   ParseTest:NEAR         ;in the hw8 object file
        EXTRN   ParseBegin:NEAR        ; sets up parser functions
START:  

; change 
MAIN:
        MOV     AX, DGROUP              ;initialize the stack pointer
        MOV     SS, AX
        MOV     SP, OFFSET(DGROUP:TopOfStack)

        MOV     AX, DGROUP              ;initialize the data segment
        MOV     DS, AX
        TestInit: 
        CALL    ParseBegin            ; calls init
        CALL    ParseTest             ;do the tests on the parser
       

CODE ENDS

;the data segment. initialized because used later on

DATA    SEGMENT PUBLIC  'DATA'


DATA    ENDS




;the stack

STACK   SEGMENT STACK  'STACK'

                DB      80 DUP ('Stack ')       ;240 words

TopOfStack      LABEL   WORD

STACK   ENDS



END     START
