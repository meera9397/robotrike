
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   TIMER.INC                                ;
;                              Event Handler Demo                            ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the definitions for intializing and using the timer for 
; event handlers

; Timer Definitions

; Addresses
Tmr0Ctrl        EQU     0FF56H          ;address of Timer 0 Control Register
Tmr0MaxCntA     EQU     0FF52H          ;address of Timer 0 Max Count A Register
Tmr0Count       EQU     0FF50H          ;address of Timer 0 Count Register

; Control Register Values
Tmr0CtrlVal     EQU     0E001H          ;value to write to Timer 0 Control Register
                                        ;1---------------  enable timer
                                        ;-1--------------  write to control
                                        ;--1-------------  enable interrupts
                                        ;----000000------  reserved
                                        ;---0------0-----  read only
                                        ;-----------0----  TMRIN0 is an enable
                                        ;------------00--  count timer 2 outs
                                        ;--------------0-  single counter mode
                                        ;---------------1  continuous mode
Tmr2CtrlVal     EQU     0C001H          ;value to write to Timer 2 Control Register
                                        ;1---------------  enable timer
                                        ;-1--------------  write to control
                                        ;--0-------------  no interrupts
                                        ;----000000-0000-  reserved
                                        ;---0------0-----  read only
                                        ;---------------1  continuous mode

; Interrupt Vectors
Tmr0Vec         EQU     8               ;interrupt vector for Timer 0

; Timing Definitions

COUNTS_PER_MS   EQU     2304            ;number of timer counts per 1 ms (assumes 18.432 MHz clock)
MS_PER_SEG      EQU     2304             ;number of ms for each segment
