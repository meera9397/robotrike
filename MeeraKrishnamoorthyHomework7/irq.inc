;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 INTERRUPT.INC                              ;
;                              Event Handler Demo                            ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Interrupt Controller Definitions

; Addresses
INTCtrlrEOI     EQU     0FF22H          ; address of interrupt controller EOI register

; Register Values
Int2EOI         EQU     000EH           ; INT2 EOI command
NonSpecEOI      EQU     08000H          ; Non-specific EOI command
