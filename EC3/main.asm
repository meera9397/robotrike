        NAME    MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 MAIN LOOP                                  ;
;                                  EE/CS  51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:      This main loop is used to test my macros. 



;definitions

CGROUP  GROUP   CODE
DGROUP  GROUP   DATA
     
CODE    SEGMENT PUBLIC 'CODE'
		ASSUME  CS:CGROUP, DS:DGROUP
		EXTRN   TestMacros:NEAR 
        
;external function declarations


START:  

MAIN:
        CLI
        MOV     AX, STACK               ;initialize the stack pointer
        MOV     SS, AX
        MOV     SP, OFFSET(TopOfStack)

        MOV     AX, DGROUP                ;initialize the data segment
        MOV     DS, AX
		
		Call TestMacros
		
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
