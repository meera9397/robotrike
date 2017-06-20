;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 INTERRUPT.INC                              ;
;                              Event Handler Demo                            ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


Int2EOI         EQU     000EH           ; INT2 EOI command
