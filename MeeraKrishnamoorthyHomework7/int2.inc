
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   INT2.INC                                 ;
;                              Event Handler Demo                            ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the definitions for initializing and using INT2 for 
; event handlers

; INT2 Definitions

; Addresses
I2CON           EQU     0FF3CH       ; address of I2CON register associatd
									 ; with INT2 

; Control Register Values
ICON2Val     EQU     00001H          ;value to write to INT 2 Register
                                        ;-----------0----  edge/level triggering
													       ; set for edge
														   ; triggering
                                        ;------------0---  clear to enable
                                        ;                  interrupts
                                        ;-------------001  define priority level

; Interrupt Vectors
Int2Vec         EQU     14               ;interrupt vector for INT2

