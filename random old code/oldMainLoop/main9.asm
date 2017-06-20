        NAME    MAIN9

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                               REMOTE MAIN LOOP                             ;
;                                  EE/CS  51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:
;
; Input:            None.
; Output:           None.
;
; User Interface:
; Error Handling:
;
; Algorithms:       None.
; Data Structures:  None.
;
; Known Bugs:       None.
; Limitations:
;
; Revision History:
;

;definitions

CGROUP  GROUP   CODE
DGROUP  GROUP   DATA
     
CODE    SEGMENT PUBLIC 'CODE'
		ASSUME  CS:CGROUP, DS:DGROUP


$INCLUDE(MAIN9.inc)
 
;external function declarations

        EXTRN   InitCS:NEAR
        EXTRN   ClrIRQVectors:NEAR
        EXTRN   InitTimer:NEAR
        EXTRN   InitEventQueue:NEAR
        EXTRN   InitDisplay:NEAR
        EXTRN   InitKeypad:NEAR
        EXTRN   InitSerial:NEAR
        EXTRN   InstallKeypadDisplayHandler:NEAR
        EXTRN   InstallSerialHandler:NEAR
        EXTRN   EventQueueFull:NEAR
        EXTRN   DequeueEvent:NEAR
        
        EXTRN   GetKeyPress:NEAR
        EXTRN   GetError:NEAR
        EXTRN   GetReceivedData:NEAR
        EXTRN   CheckCriticalFlag:NEAR

START:

MAIN:
        CLI
        MOV     AX, STACK               ;initialize the stack pointer
        MOV     SS, AX
        MOV     SP, OFFSET(TopOfStack)

        MOV     AX, DGROUP                ;initialize the data segment
        MOV     DS, AX
StartMainLoop:
        ; create function to initialize buffer
        CALL InitCS
        CALL ClrIRQVectors
        CALL InitTimer
        CALL InitEventQueue ; creates event queue
        CALL InitDisplay
        CALL InitKeypad
        CALL InitSerial
        CALL InstallKeypadDisplayHandler
        CALL InstallSerialHandler ; written already, installs serial
                                  ; event handler
    CheckRestart:
        CALL CheckCriticalFlag
        CMP AX, CRITICAL_FLAG
        JNE GetNewElement
    Restart:
        JMP StartMainLoop ; reinitializes everything
    GetNewElement:
        CALL DequeueEvent
        CMP AX, 0
        JE EndMain
        MOV BX, 0 ; clear upper byte of BX (used to index remote table)
        XCHG BL, AH ; moves the event type into BL so that it can index table
                    ; also now high bit of AH is cleared so AL (event value)
                    ; which is only necessary information is the only thing
                    ; remaining
        LEA SI, stringbuffer
        CALL CS:RemoteTable[BX]
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

DW          offset(GetKeyPress)
DW          offset(GetError)
DW          offset(GetReceivedData)

CODE ENDS


;the data segment. initialized because used later on. 

DATA    SEGMENT PUBLIC  'DATA'
   
stringbuffer    DB ?    

DATA    ENDS


;the stack

STACK   SEGMENT STACK  'STACK'

                DB      80 DUP ('Stack ')       ;240 words

TopOfStack      LABEL   WORD

STACK   ENDS




END     START
