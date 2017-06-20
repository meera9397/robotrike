NAME    MAIN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                MOTOR MAIN LOOP                             ;
;                                  EE/CS  51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:
;
; Input:            None.
; Output:           None.
;
; User Interface:
;
; Error Handling:
;
; Algorithms:       None.
; Data Structures:  None.
;
; Known Bugs:       None.
; Limitations:
; Revision History:
;



;definitions

CGROUP  GROUP   CODE
DGROUP  GROUP   DATA, STACK



CODE    SEGMENT PUBLIC 'CODE'


ASSUME  CS:CGROUP, DS:DGROUP



;external function declarations

EXTRN   Dec2String:NEAR         ;convert a number to a decimal string
EXTRN   Hex2String:NEAR         ;convert a number to a hex string




START:

MAIN:
    MOV     AX, DGROUP              ;initialize the stack pointer
    MOV     SS, AX
    MOV     SP, OFFSET(DGROUP:TopOfStack)

    MOV     AX, DGROUP              ;initialize the data segment
    MOV     DS, AX

StartMainLoop:
    CALL InitCS
    CALL ClrIRQVectors
    CALL InitTimer
    CALL InitEventQueue ; creates event queue
    CALL InitMotors
    CALL InitParser
    CALL InitSerial
    CALL Install_PWM_Handler
    CALL InstallSerialHandler ; written already, installs serial
                              ; event handler
CheckRestart:
      CALL EventQueueFull
      JNE CheckDequeue
      JMP StartMainLoop ; reinitializes everything
CheckDequeue:
      CALL EventQueueEmpty
      JE EndMain
GetNewElement:
      CALL DequeueEvent
      MOV BX, 0 ; clear upper byte of BX (used to index remote table)
      XCHG BL, AH ; moves the event type into BL so that it can index table
                  ; also now high bit of AH is cleared so AL (event value)
                  ; which is only necessary information is the only thing
                  ; remaining
      LEA SI, stringbuffer
      CALL CS:MotorTable[BX]
EndMain:
      JMP CheckRestart

; RemoteTable
;
; Description:
;
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Meera Krishnamoorthy
; Last Modified:    11/12/16

RemoteTable       LABEL   WORD
        PUBLIC  RemoteTable

DW          offset(doNOP)
DW          offset(SendError) ;???
DW          offset(CheckParsing)


CODE ENDS


;the data segment. initialized because used later on. 

DATA    SEGMENT PUBLIC  'DATA'
   
stringbuffer    DW      ?

DATA    ENDS


;the stack

STACK   SEGMENT STACK  'STACK'

                DB      80 DUP ('Stack ')       ;240 words

TopOfStack      LABEL   WORD

STACK   ENDS


