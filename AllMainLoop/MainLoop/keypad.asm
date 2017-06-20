NAME    KEYPAD

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   Keypad                                   ;
;                    Functions to determine key presses on board             ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description: This file contains all the functions needed to determine
;                   if a key press has been made on the board and what key
;                   was pressed.
;
; Table of Contents:  
; 1) InitKeypad: initializes variables necessary for keypad functions.
; 2) Scan: scans keypad to see if keys are pressed on each row, and if
;          if they are pressed, if a new key or the same key as before was
;          pressed
; 3) Debounce: called if the same key was pressed in the current iteration
;              of the scan function as the last iteration of the scan function.
;              Keeps track of a counter to see how long the key has been
;              pressed. 
; 
; Revision History:
;     1/26/06  Glen George            initial revision
;    11/5/16  Meera Krishnamoorthy    wrote code and debugged



CGROUP  GROUP   CODE
DGROUP  GROUP   DATA

CODE	SEGMENT PUBLIC 'CODE'
    EXTRN   EnqueueEvent:NEAR
    ASSUME  CS:CGROUP, DS:DGROUP

$include(keypad.inc)

; InitKeypad
;
; Description: This function initializes the shared variables used to scan and
;              debounce the keypad.
;
; Operation: It sets keyValue to NO_KEY_PRESS. This shared
;            variables describes the current key pressed, 
;            Then, this function sets debounceCounter, 
;            the variable that debounces a key press once it is made, to 
;            MAX_TIME, the maximum amount of time needed for a key to be pressed 
;            before it is considered a valid key press. Finally, it nulls the 
;            value of rowOffset, a variable used to keep track of what row in the
;            keypad is being checked on. 
;
; Arguments: None
; Return Value: None
;
; Local Variables: None.
; Shared Variables: keyValue: current value of key, R and W, size = 8 bits, 
;                             type = byte
;                   debounceCounter: debounces a key once it has been
;                               pressed, R and W, size = 16 bits, type = word
;                   rowOffset: keeps track of row, R and W, size = 16 bits, 
;                               type = word
; Global Variables: None.
;
; Input: None.
; Output: None.
;
; Error Handling: None.
;
; Algorithms: None.
; Data Structures: None. 
;
;
; Registers Changed: None.
; Stack Depth: None.
;
; Limitations: None.
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/5/16
;

InitKeypad      PROC        NEAR
        PUBLIC      InitKeypad

MOV keyValue, NO_KEY_PRESS
MOV debounceCounter, MAX_TIME
MOV rowOffset, 0
RET

InitKeypad	ENDP

; Scan
;
; Description: This function is called by the timer event handler. It reads
;              in key presses from every row. Depending on the previous
;              key press, the key press is either debounced, or set to the
;              new key value. 

; Operation: This is done by reading in key presses from each row. If there
;            is no key press on a row, the next row is looked at (the
;            rowOffset variable is incremented, and wraps to continuously
;            search the rows). If a key is pressed, only the last 4 bits
;            are looked at (those bits contain the information). Those 4 bits
;            are compared to keyValue, which contains the previously pressed
;            key. If the current press and keyValue are the same, that
;            key value is debounced by calling the debounce method. If the
;            key values are different, keyValue is set to the new pressed
;            key value, and the debounce counter is re-initialized.

; Arguments: None. 
; Return Value:
;
; Local Variables: None.
; Shared Variables: keyValue: current value of key, R and W, size = 8 bits, 
;                             type = byte
;                   debounceCounter: debounces a key once it has been
;                               pressed, R and W, size = 16 bits, type = word
;                   rowOffset: keeps track of row, R and W, size = 16 bits, 
;                               type = word
; Global Variables: None.
;
; Input: Keypad inputs (read in AL)
; Output: None.
;
; Error Handling: None.
;
; Algorithms: None.
; Data Structures: Queues: used to store keypresses, and are stored in 
;                  a buffer. 
;
;
; Registers Changed: AX, DX
; Stack Depth: None.
;
; Limitations: None. 
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/5/16
;

Scan     PROC        NEAR
        PUBLIC      Scan

; get value of current row in keypad we are looking at

StartScan:
    ; set DX equal to the address of the current row being looked at
    MOV DX, INIT_KEYPAD_ADDRESS
    ADD DX, rowOffset
    IN AL, DX ; get value of key being pressed in that row

CheckAL:
    SHL AL, 4 ; only look at the last 4 bits of AL (hold the important 
              ; information
    CMP AL, NO_KEY_PRESS ; check if a key is being pressed
    JE IncRow ; if no key being pressed, move on to next row
    ;JNE DetermineDebounce

DetermineDebounce:
    CMP AL, keyValue ; check if AL is equal to value of key 
                   ; pressed before this method was called
    JE NeedDebounce ; if value is same, debounce the key
    ;JNE NewKey ; if value is not same, set new value of keyValue (new key
                ; being pressed in that row

NewKey: ;if new key being pressed
    MOV debounceCounter, MAX_TIME ; re initialize variable keeping track of 
                                  ; number of seconds key
                                  ; has been pressed
    MOV keyValue, AL ; set keyValue equal to this new pressed key
    JMP EndScan
    
NeedDebounce:
    Call Debounce ;call function to debounce key being pressed
    JMP EndScan
    
IncRow:
    MOV debounceCounter, MAX_TIME ; re initialize variable keeping track of 
                                  ; number of seconds key
                                  ; has been pressed because of new key press
                                  ; (no key press = new key press)
                             
    ; increment the row counter
    MOV BX, rowOffset
    INC BX
    MOV rowOffset, BX
    CMP rowOffset, MAX_ROW ;account for wrapping with the row counter
    JLE EndScan
    ;JGE WrapRow

WrapRow:
    MOV rowOffset, 0 
    JMP EndScan
                 
EndScan:
    RET

Scan	ENDP

; Debounce
;
; Description: This function keeps track of how long a key press has been made,
;              to determine the validity of that key press. If a key press
;              has been made for MAX_TIME calls of the debounce function,
;              then it is considered a valid key press, and it is stored in AL.
;              along with a constant describing the type of event it is stored 
;              in AH. Then, the timer will be set to a higher value to account
;              for any constant key presses. 
;
; Operation: This is done by making a shared variable counter that decrements
;            each time the function is called. Once this counter is 0, 
;            the key press is ready to be enqueued. The value of the key
;            press, along with the row of the key press, is stored in AL.
;            A constant describing the key press (KEY_EVENT) is stored in AH.
;            Then, the timer will be set to a higher value to account
;            for any constant key presses. 
; 
; 
; Arguments: None
; Return Value: None
;
; Local Variables: None.
; Shared Variables: keyValue: current value of key, R and W, size = 8 bits, 
;                             type = byte
;                   debounceCounter: debounces a key once it has been
;                               pressed, R and W, size = 16 bits, type = word
;                   rowOffset: keeps track of row, R and W, size = 16 bits, 
;                               type = word
; Global Variables: None.
;
; Input: None.
; Output: None.
;
; Error Handling: None.
;
; Algorithms: None.
; Data Structures: Queues.
;
;
; Registers Changed: None.
; Stack Depth: None.
;
; Limitations: None.
;
; Author: Meera Krishnamoorthy
; Last Modified: 11/5/16
;

Debounce     PROC        NEAR
        PUBLIC      Debounce

DecCounter:
    DEC debounceCounter ; debounce by ensuring that the key press stays
                        ; the same each time the method is called
    CMP debounceCounter, 0 ; if the debounceCounter is 0, that means
                           ; the key press has been debounced and can be
                           ; enqueued
    JG EndDebounce  
    
StoreVal:
    MOV BX, rowOffset 
    ADD BL, keyValue  ; add row offset to key value so that we know
                      ; which row the pressed key was on (keys in 
                      ; each column have the same value)

    MOV AL, BL ; set AL = new key press + row that it was on
    MOV AH, KEY_EVENT ; move constant describing type of event into AH
    MOV debounceCounter, INC_MAX_TIME ; autorepeat, if get same key 
                                      ; press again, set timer to 
                                      ; greater value. 
    Call EnqueueEvent ;enqueue the keypress (keep track of it)

EndDebounce:
    RET

Debounce	ENDP


CODE    ENDS


DATA    SEGMENT PUBLIC  'DATA'

keyValue        DB      ?       ;the current value of key press
debounceCounter DW      ?       ;number of times key has been pressed
rowOffset       DW      ?       ;current row looking at      

DATA    ENDS


END