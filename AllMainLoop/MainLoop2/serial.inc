; SERIAL.INC
;
; This file lists all the constants used in serial.asm

; Baud Rate Constants
BAUD_DIVISOR_INDEX    EQU     6   ; index to access Baud Divisor Table
								  ; to determine baud rate to use

; Parity Constants
PARITY_OFF			  EQU	  2   ; value to set to a register to set
								  ; parity off 

; Register I/O Addresses
SERIAL_START    EQU     100H      ; value serial registers start at

DLL			    EQU     SERIAL_START       ;low byte of baud divisor (R/W)
DLM			    EQU     SERIAL_START + 1   ;high byte of baud divisor (R/W)
SERIAL_RX_REG   EQU     SERIAL_START       ;receive data register (R)
SERIAL_TX_REG   EQU     SERIAL_START       ;transmit data register (W)
SERIAL_IER      EQU     SERIAL_START + 1   ;interrupt enable register (R/W)
SERIAL_IIR      EQU     SERIAL_START + 2   ;interrupt ident. register (R/W)
SERIAL_LCR      EQU     SERIAL_START + 3   ;line control register (R/W)
SERIAL_MCR      EQU     SERIAL_START + 4   ;modem control register (R/W)
SERIAL_LSR      EQU     SERIAL_START + 5   ;line status register (R/W)
SERIAL_MSR      EQU     SERIAL_START + 6   ;modem status register (R/W)

; Constants for EnqueueEvent
LSI_CONST       EQU     01 ; constant to send to EnqueueEvent associatd
						   ; with line status interrupt
RDI_CONST       EQU     02 ; constant to send to EnqueueEvent associatd
						   ; with received data interrupt 
						   
; other interrupt constants 						   
NO_INTERRUPT	EQU		1  ; value of IIR that indicates no interrupts are occurring
MASK_NON_ERROR	EQU		00011110B ; bit pattern that masks all non-error
MASK_PARITY     EQU     11000111B  ; bits in the LSI

; LCR Register Values
TRANSMIT8CHAR     EQU    00000011B ; bits to be set in LCR to transmit 8 
								   ; characters
TRANSMIT7CHAR     EQU    00000010B ; bits to be set in LCR to transmit 7 
								   ; characters
TRANSMIT6CHAR     EQU    00000001B ; bits to be set in LCR to transmit 6 
								   ; characters
TRANSMIT5CHAR     EQU    00000000B ; bits to be set in LCR to transmit 5 
								   ; characters
; one stop bit = 00000000B
MORE_1_STOP_BIT        EQU    00000100B ; bits to be set in LCR to have more
										; than 1 stop bit
ENABLE_PARITY          EQU    00001000B ; bits to be set in LCR to enable parity
ODD_PARITY             EQU    00001000B ; bits set for LCR to have odd parity
EVEN_PARITY            EQU    00011000B ; bits to be set in LCR to have even
										; parity 
STICK_PARITY           EQU    00111000B ; bits to be set in LCR to have stick
										; parity

BREAK_CONTROL          EQU    01000000B ; bits to be set in LCR to have break control
DIVISOR_LATCH          EQU    10000000B ; bits to be set in LCR to enable 
										; the baud rate to be set (DLAB)

; IER Register Values
MODEM_STATUS              EQU   00001000B ; bits to be set in IER to enable
										  ; modem status interrupts 
RECEIVER_LINE_STATUS      EQU   00000100B ; bits to be set in IER to enable
										  ; receiver line status interrupts
THRE                      EQU   00000010B ; bits to be set in IER to enable
										  ; transmitter holding register empty
										  ; interrupts
RECEIVED_DATA_AVAILABLE   EQU   00000001B ; bits to be set in IER to enable
										  ; received data available interrupts 
