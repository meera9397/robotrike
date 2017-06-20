; KEYPAD.INC
; This file lists the constants used in "keypad.asm", which contains all
; the functions needed to determine if a key press occurred on
; the keypad. 

INIT_KEYPAD_ADDRESS  EQU 080H   ; the address of the first row of the keypad
MAX_ROW              EQU 3      ; The number of rows the keypad has (0 - 3)
MAX_TIME             EQU 100     ; the amount of time a key needs to be pressed
                                ; for it to be considered a valid key press

KEY_EVENT            EQU 2      ; constant describing key event

NO_KEY_PRESS         EQU 0F0H   ; the value passed in from the keypad when
                                ; no key in that row is pressed
                                
INC_MAX_TIME        EQU 1000     ; higher max counter