        NAME    MAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   HW3TEST                                  ;
;                            Homework #3 Test Code                           ;
;                                  EE/CS  51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:      This program tests the functions for Homework
;                   #3.  It calls each function in the queue.asm file, and 
;                   then calls "QueueTest" in the hw3.obj file, which
;                   tests the functions in individual cases. It also sets
;                   the size of the queue, creates an instance of the queue
;                   structure, and puts that queue structure at the address in 
;                   the SI register. 
;
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
; Limitations:      The returned strings must be less than MAX_STRING_SIZE
;                   characters.
;
; Revision History:
;    10/22/16       Meera Krishnamoorthy    created file based on Homework 2
;                                           test code and modified



;definitions

MAX_STRING_SIZE EQU     20              ;maximum string buffer size
ASCII_NULL      EQU     0               ;string termination character (<null>)




CGROUP  GROUP   CODE
DGROUP  GROUP   DATA, STACK


$ INCLUDE(queues.inc)

CODE    SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP, DS:DGROUP



;external function declarations

        EXTRN   QueueTest:NEAR         ;in the hw3 object file
        EXTRN   QueueInit:NEAR
        EXTRN   QueueEmpty:NEAR
        EXTRN   QueueFull:NEAR
        EXTRN   Dequeue:NEAR
        EXTRN   Enqueue:NEAR


START:  

MAIN:
        MOV     AX, DGROUP              ;initialize the stack pointer
        MOV     SS, AX
        MOV     SP, OFFSET(DGROUP:TopOfStack)

        MOV     AX, DGROUP              ;initialize the data segment
        MOV     DS, AX

        MOV     CX, queue_size          ; sets the size of the queue
        LEA     SI, queue               ; moves the queue address into SI
        SUB     CX, 1
        CALL    QueueTest             ;do the tests on the queue
       

CODE ENDS

;the data segment

DATA    SEGMENT PUBLIC  'DATA'

queue QueueStruct <>        ; creates an instance of the queue structure

DATA    ENDS




;the stack

STACK   SEGMENT STACK  'STACK'

                DB      80 DUP ('Stack ')       ;240 words

TopOfStack      LABEL   WORD

STACK   ENDS



END     START
