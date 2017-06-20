; MAIN9.INC
; This contains all of the constants needed to run the remote main loop.

; error flags/ related values
CRITICAL_FLAG       EQU 123 ; this is the value that the critical flag shoul
                            ; be set to when the queue is full
ERROR_FLAG_VAL      EQU 66 ; this is the value our error flag is set to
                           ; when there is an error
QUEUE_EMPTY_CONST   EQU 6  ; this is the constant that corresponds to the
                           ; appropriate function to call in the RemoteTable
                           ; call table when the queue is empty
                           
                           

; ASCII constants
; These are contained in other files, but are here simply for quick
; reference

CARRIAGE_RETURN     EQU 13 ; this is the ASCII value of a carriage return.
                           ; the motor unit recognizes this character
                           ; as the end of a command, so when we send
                           ; strings serially, they need to end with
                           ; a carriage return
                          

; constants related to size
MAX_KEY_PRESS       EQU 15 ; this is the maximum number of keys 
                           ; in our AllKeyPresses table (all the keys we are 
                           ; considering valid key presses)

EVENTQUEUE_SIZE     EQU 1024 ; this is the size of our event queue



MAX_ERROR_TABLE_VAL   EQU 24 ; this is length of the ErrorTable if the
                             ; table is a word table. since the lsr
                             ; is set (error wise) as multiples of 2
                             ; this value is used to determine
                             ; if the lsr value is within the table. 
                             

; the following constants are used in the KeyPress table to check
; if a key pressed has a valid value (we are defining what
; valid key presses are)  
; the values (e, d, b, 7) for each key are actually related
; to value locations read from the address 80H to 83H (the keypad) when the
; keys are pressed. e, d, b, 7 aren't the entire values corresponding
; to the key presses, but they are the uniqe part of each key value
                           
KEY_PRESS_1 EQU          0E0H       ; key press corresponding to a value of 
                                    ; e in row 0
KEY_PRESS_2 EQU          0D0H       ; key press corresponding to a value of 
                                    ; d in row 0
KEY_PRESS_3 EQU          0B0H       ; key press corresponding to a value of 
                                    ; b in row 0
KEY_PRESS_4 EQU          070H       ; key press corresponding to a value of 
                                    ; 7 in row 0
       
KEY_PRESS_5 EQU          0E1H       ; key press corresponding to a value of 
                                    ; e in row 1
KEY_PRESS_6 EQU          0D1H       ; key press corresponding to a value of 
                                    ; d in row 1
KEY_PRESS_7 EQU          0B1H       ; key press corresponding to a value of 
                                    ; b in row 1
KEY_PRESS_8 EQU          071H       ; key press corresponding to a value of 
                                    ; 7 in row 1
      
KEY_PRESS_9 EQU           0E2H      ; key press corresponding to a value of 
                                    ; e in row 2
KEY_PRESS_10 EQU          0D2H      ; key press corresponding to a value of 
                                    ; d in row 2 
KEY_PRESS_11 EQU          0B2H      ; key press corresponding to a value of 
                                    ; b in row 2
KEY_PRESS_12 EQU          072H      ; key press corresponding to a value of 
                                    ; 7 in row 2
      
KEY_PRESS_13 EQU          0E3H      ; key press corresponding to a value of 
                                    ; e in row 3 
KEY_PRESS_14 EQU          0D3H      ; key press corresponding to a value of 
                                    ; d in row 34 
KEY_PRESS_15 EQU          0B3H      ; key press corresponding to a value of 
                                    ; b in row 3
KEY_PRESS_16 EQU          073H      ; key press corresponding to a value of 
                                    ; e in row 3

