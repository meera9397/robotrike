;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  CONVERTS.INC                              ;
;                       constants for conversion functions                   ;
;             one of which converts from Dec to ASCII, and the other         ;
;                          converts from Hex to ASCII                        ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains the definitions for the structure version of the switch
; functions for the Microprocessor-Based Clock.
;
; Revision History:
;     2/07/03  Glen George          initial revision (from 12/26/99 version of
;                                   clock.inc)
;    12/24/03  Glen George          added constants for "Display" switch settings
;     7/12/10  Glen George          changed name to CLKSWS.INC, updated comments
;    10/15/16  Meera Krishnamoorthy changed name to Converts.INC, updated constants


; constants needed for Dec2String
INIT_POWER equ 10000 ; (10^5) power to divide all digits by
DIV_POWER equ 10 ; (10^1) number that init_power reduces by each iteration of the function to caputer each digit

; constants needed for Hex2String
INIT_HEX_POWER equ 4096 ; (16^3) power to divide all digits by
DIV_HEX_POWER equ 16 ; (16^1) number that init_power reduces by each iteration of the function to caputer each digit
NUM_TO_DIG equ 10 ; barrier between numbers and digits in hex

;constants needed for both Dec2String and Hex2String
ASCII_ZERO equ 48
ASCII_A_offset equ 55
ASCII_negsign equ 45
ASCII_NULL equ 0

