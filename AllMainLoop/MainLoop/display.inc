; DISPLAY.INC
; This file contains the constants for the functions in "display.asm", three
; of which are used to convert decimal/hexadecimal numbers into segment patterns,
; and two of which are used to display those segment patterns on the board


; Revision History:
;    10/30/16  Meera Krishnamoorthy wrote constants

ASCII_NULL      equ 0           ;used to check if the string was terminated
DISPLAY_LEN     equ 8           ;the number of characters that can be displayed
                                ;on the display
; these constants are the same as the ones above, but exist in case the
; LED Display Address changes                                
LEDDisplay      equ 0000H                       ;display address of initial segment
LEDHighByteAdd  equ LEDDisplay + DISPLAY_LEN    ;display address of offset segments in
                                                ; MuxSegPatterns

BUFFER_LENGTH equ 16 ; length of buffer storing segments