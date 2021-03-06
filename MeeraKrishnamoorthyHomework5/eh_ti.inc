;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  EH_TI.INC                                ;
;                              Event Handler Demo                            ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the definitions for the Event Handler Demonstration
; program (EH_TI.ASM).

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


; Interrupt Controller Definitions

; Addresses
INTCtrlrCtrl    EQU     0FF32H          ;address of interrupt controller for timer
INTCtrlrEOI     EQU     0FF22H          ;address of interrupt controller EOI register

; Register Values
INTCtrlrCVal    EQU     00001H          ;set priority for timers to 1 and enable
                                        ;000000000000----  reserved
                                        ;------------0---  enable timer interrupt
                                        ;-------------001  timer priority
TimerEOI        EQU     00008H          ;Timer EOI command (same for all timers)
NonSpecEOI      EQU     08000H          ;Non-specific EOI command


; Timing Definitions

COUNTS_PER_MS   EQU     2304            ;number of timer counts per 1 ms (assumes 18.432 MHz clock)
MS_PER_SEG      EQU     2304             ;number of ms for each segment



; General Definitions

FIRST_RESERVED_VEC	EQU	1	;reserve vectors 1-3
LAST_RESERVED_VEC	EQU	3
NUM_IRQ_VECTORS         EQU     256     ;number of interrupt vectors

LEDDisplay      EQU     0000H           ;display address

NUM_DIGITS      EQU     8               ;number of digits in the display
