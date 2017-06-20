NAME    DISPLAY

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   Display                                  ;
;  Functions to display numbers (in decimal and hexadecimal), and strings    ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; WHAT I CHANGED:
; 1) updated Display and MuxSegPatterns to get 14 segment display
; File Description: This file contains some of the functions
;                   necessary to output strings/numbers to an 8 character
;                   display. Specifically, it contains functions that
;                   convert strings/numbers/hexadecimal numbers into
;                   segment patterns, and a function that can output those
;                   segment patterns via muxing to the display.
; Table of Contents
; 1) Display: converts string to segment patterns that can be outputted
;             to the display
; 2) DisplayNum: converts a 15 bit signed number to segment patterns that can be 
;             outputted to the display
; 3) DisplayHex: converts a 16 bit unsigned number to segment patterns that can be 
;             outputted to the display
; 4) InitDisplay: initializes segment buffer and index that points to 
;                   current segment to output to display in buffer
; 5) MuxSegPatterns; outputs segments to display from segment buffer one by one
;                    keeping track of which segments have been outputted in
;                    aforementioned index variable. 
; 
; Revision History:
;    10/29/16  Meera Krishnamoorthy   wrote code and debugged
;    10/30/16  Meera Krishnamoorthy   commented and debugged
;    12/8/16   Meera Krishnamoorthy   updated code to include 14 segment
;                                     display



CGROUP  GROUP   CODE
DGROUP  GROUP   DATA

$INCLUDE(DISPLAY.INC)
$INCLUDE(simpmac.inc)

CODE	SEGMENT PUBLIC 'CODE'
       EXTRN   ASCIISegTable:BYTE
       EXTRN   Dec2String:NEAR
       EXTRN   Hex2String:NEAR
       ASSUME  CS:CGROUP, DS:DGROUP
       ;NO_SEG_PAT equ $ - ASCIISegTable
       

; Display
;
; Description: This function is passed a string that is null terminated (str) 
;              and updates the buffer in the data segment with segment
;              patterns corresponding to the characters in the string. The
;              null terminated string is passed by reference in ES:SI. ES
;              is used so that the string can be in the code segment (it can
;              be a constant string) without needing to change the data segment.
;              The function converts each character in the string to a segment
;              pattern that represents the character, and saves that segment 
;              pattern so that it can be accessed by a function that writes
;              to the display 
;
; Operation: A string is a sequence of ASCII characters. The way that this 
;            function converts a string to something that can be displayed
;            on the LED display is by converting each ASCII character in
;            that string to a segment pattern. This is done using the 
;            "segtable.asm" file, which contains each ASCII character's 
;            representation as a segment pattern. This segment pattern table is 
;            ordered like an ASCII table so accessing each value in the table
;            would require finding the address that the ASCII character is
;            stored in and incrementing that address by a byte to access
;            each character individually. Then, a segment pattern from
;            each character is saved in an array in the data segment so that
;            it can be used by the event handler when an interrupt is 
;            signalled. This function stops when either the
;            number of characters it has converted to segment patterns
;            is 8 (because there is space for 8 characters on the LED display),
;            or if the it encounters a null character (indicating the end
;            of the string). At the end, the function adds a null character
;            to the buffer.
;
; Arguments: str (passed by reference in ad)
; Return Value:
;
; Local Variables: None.
; Shared Variables: segbuffer: used to store the segment pattern array
; Global Variables: None.
;
; Input: None.
; Output: None.
;
; Error Handling: None.
;
; Algorithms: None.
; Data Structures: ASCII Table stored in segtab14.asm. It is the same size
;                  as an ASCII table, and is R only (no writing is done
;                  to the table)
;
;
; Registers Changed: DI, SI, BX, AL
; Stack Depth: None.
;
; Limitations: the display can only display 8 ASCII characters at a time
;
; Author: Meera Krishnamoorthy
; Last Modified: 10/29/16
;

Display      PROC        NEAR
        PUBLIC      Display
PUSHA
MOV DI, 0 ;initialize index of segment buffer
          ; keeps track of how many elements have been added to buffer
          ; truncates the number at 8

NextChar: 
    CMP DI, BUFFER_LENGTH ; keeps function looping if the number of items
                        ; in the buffer is < buffer_length
    JE EndDisplay
    
Convert:
    ;LEA BX, ASCIISegTable
    MOV AH, 0
    MOV AL, byte ptr ES:[SI] ;gets current character from string stored in 
                             ; ES:SI to convert to a segment pattern
    CMP AL, ASCII_NULL ;if the character is null, that means the string
                       ; has ended. Thus the function can stop converting.
    JE AddNull
    
    ;XLAT CS:ASCIISegTable  ; looks up representation of AL in segment pattern table       
    SHL AL, 1 ; multiply by 2 to index word table (segtab14 is a word table)
    
    MOV BX, AX ; move index into BX (only BX can index tables)
    MOV AL, CS:ASCIISegTable[BX] ; move low byte of table element into AL
    
    INC BX
    MOV AH, CS:ASCIISegTable[BX] ; move low byte of table element into AH
    
    MOV segbuffer[DI], AX ; stores segment pattern in buffer
    
    INC SI ; increment address of the string we are looking at
    ADD DI, 2 ; add 2 to string buffer because added a word (AH + AL)
    JMP NextChar
    
AddNull:
    ; continues to add null characters to the string until the buffer's size
    ; is DISPLAY_LEN (the length of the display)
    MOV segbuffer[DI], ASCII_NULL
    INC DI
    CMP DI, BUFFER_LENGTH
    JL AddNull
    ;JE EndDisplay
    
EndDisplay:
    MOV segbuffer[DI], ASCII_NULL ; adds null character to end of buffer
                                  ; indicating string is terminated
POPA                         
    RET

Display	ENDP


; DisplayNum
;
; Description:The function is passed a 16 bit signed value (n). It must output
;             in decimal (at most 4 digits + negative sign) to the LED display.
;             n is passed in AX by value. 
;
; Operation: This is done by first moving the location of the string
;            containing the signed value from the string buffer into SI. The
;            value of SI is saved because Dec2String writes to SI, and increment
;            SI's value to point to the end of the string. Thus, SI's original
;            value is later restored so that SI points to the front
;            of the string. 
;            Then Dec2String is called.
;            It converts a number to an ASCII str9jg by offsetting each
;            digit with the ASCII representation of 0. Then, this function
;            changes from working in the data segment to the extra segment
;            register, because Display converts the string in ES:SI.
;            Finally, this function calls the Display function written above to 
;       convert the string
;            with the signed value to segment patterns that display each 
;            character, saving each segment pattern in the buffer.
;
; Arguments: str(passed by reference in address ES:SI)
; Return Value:
;
; Local Variables: None.
; Shared Variables: segbuffer: used to store the segment pattern array
; Global Variables: None.
;
; Input: None.
; Output: None.
;
; Error Handling: None.
;
; Algorithms: None.
; Data Structures: ASCII Table stored in segtable.asm. It is the same size
;                  as an ASCII table, and is R only (no writing is done
;                  to the table)
;
;
; Registers Changed: SI
; Stack Depth: None.
;
; Limitations:
;
; Author: Meera Krishnamoorthy
; Last Modified: 10/29/16
;

DisplayNum      PROC        NEAR
        PUBLIC      DisplayNum
PUSHA
MOV SI, offset(stringbuffer) ; set SI to point at front of string to convert
                             ; to seg patterns
PUSH SI ; save the value of SI because it will be changed in Dec2String
Call Dec2String ;convert signed value to ASCII string

PUSH DS ;Dec2String was working in the data segment, and the Display function
        ; works in the ES. Thus, we need to exchange DS and ES to work in the
        ; ES
POP ES
POP SI ;move SI to point at beginning of string again

Call Display ; convert string to segment patterns to display
POPA
RET

DisplayNum	ENDP


; DisplayNum
;
; Description:The function is passed a 16 bit unsigned value (n). It must output
;             in hexadecimal (at most 5 digits) to the LED display.
;             n is passed in AX by value. 
;
; Operation: This is done by first moving the location of the string
;            containing the signed value from the string buffer into SI. The
;            value of SI is saved because SI is the address of the string.
;            Hex2String writes to SI, and increment SI's value to point to the 
;            end of the string. Thus, SI's original value is later restored 
;            so that SI points to the front of the string. 
;            Then Hex2String is called.
;            It converts a number to its hexadecimal representation in
;            ASCII by offsetting each digit with the ASCII representation of 0
;            if it is less than 9, or the ASCII representation of A - 10 for
;            numbers greater than or equal to 10 (the "-10" comes from
;            the offset of the digit itself being greater than or equal
;            to 10).
;            Then, this function changes from working in the data segment to the 
;            extra segment register, because Display converts the string in ES:SI.
;            Finally, this function calls the Display function written above to 
;            convert the string with the hexadecimal value to segment patterns
;            that display each character, saving each segment pattern in the 
;            buffer.
;
; Arguments: str(passed by reference in address ES:SI)
; Return Value:
;
; Local Variables: None.
; Shared Variables: segbuffer: used to store the segment pattern array
; Global Variables: None.
;
; Input: None.
; Output: None.
;
; Error Handling: None.
;
; Algorithms: None.
; Data Structures: ASCII Table stored in segtable.asm. It is the same size
;                  as an ASCII table, and is R only (no writing is done
;                  to the table)
;
;
; Registers Changed: SI
; Stack Depth: None.
;
; Limitations:
;
; Author: Meera Krishnamoorthy
; Last Modified: 10/29/16

DisplayHex      PROC        NEAR
        PUBLIC      DisplayHex
PUSHA        
MOV SI, offset(stringbuffer) ; set SI to point at front of string to convert
                             ; to seg patterns
PUSH SI ; save the value of SI because it will be changed in Hex2String
Call Hex2String ;convert signed value to ASCII string
PUSH DS ;Hex2String was working in the data segment, and the Display function
        ; works in the ES. Thus, we need to exchange DS and ES to work in the
        ; ES
POP ES
POP SI ;move SI to point at beginning of string again
Call Display ; convert string to segment patterns to display
POPA
RET

DisplayHex	ENDP

; InitDisplay
;
; Description: This procedure clears the segment buffer, and also clears the 
;              variable used to track the index of the segment buffer up to which
;              the mux function has outputted to the LED display (Digit)
;
; Operation: This function does this by setting the first 8 values in the
;            segment buffer to 0 (because the display length is 8)
;
; Arguments:
; Return Value:
;
; Local Variables: None.
; Shared Variables: -segbuffer: used to store the segment pattern array;
;                   -digit: points to each index in the segbuffer to 
;                   track how many patterns have been outputted to the display
; Global Variables: none
;
; Input: None.
; Output: None.
;
; Error Handling: None.
;
; Algorithms: None.
; Data Structures:
;
;
; Registers Changed: None.
; Stack Depth: None.
;
; Limitations:
;
; Author: Meera Krishnamoorthy
; Last Modified: 10/29/16
;

InitDisplay      PROC        NEAR
        PUBLIC      InitDisplay
        
PUSHA
MOV BX, 0 

ClearBuffer:
        ; loops through the segment buffer for as many element as can be
        ; displayed on the display, setting each value in the buffer to 0
        MOV segbuffer[BX], 0
        INC BX
        CMP BX, BUFFER_LENGTH
        JL ClearBuffer
        ;JE EndInitVariable
EndInitVariable:
        ; clears the digit variable (so that it points to the first
        ; digit in the segment buffer)
        MOV     AX, Digit
        XOR     Digit, AX
        
POPA
        RET
        
InitDisplay	ENDP


; MuxSegPatterns
;
; Description: This procedure puts the segment patterns we find in Display,
;              DisplayNum, and DisplayHex on the display using a timer
;              interrupt. It outputs a segment pattern stored in the array
;              in the data segment on to the display each time it is called, 
;              going in the order that the patterns were stored in the buffer.
; Operation: The function looks at segbuffer (the array in the data segment
;            that Display/DisplayNum/DisplayHex have been storing segment
;            patterns). It outputs one segment pattern to the display at a time
;            keeping track of the number of digits it has outputted to the 
;            display with the variable "digit". Once digit is equal to the 
;            maximum number of digits that can be displayed on the display,
;            it is set to 0, and the current segment buffer is displayed
;            on the display. 
;
; Arguments:
; Return Value:
;
; Local Variables: None.
; Shared Variables: -segbuffer: used to store the segment pattern array;
;                   -digit: points to each index in the segbuffer to 
;                   track how many patterns have been outputted to the display
; Global Variables: none

; Input: None.
; Output: None.
;
; Error Handling: None.
;
; Algorithms: None.
; Data Structures:
;
;
; Registers Changed: None.
; Stack Depth: None.
;
; Limitations:
;
; Author: Meera Krishnamoorthy
; Last Modified: 10/29/16
;

MuxSegPatterns      PROC        NEAR
        PUBLIC      MuxSegPatterns
MOV BX, 0   
MuxPatterns:
    ; the segment buffer has 16 patterns to show 8 characters
    ; this code first takes the 8 + nth element of the segment
    ; buffer and displays it in the 8 + nth place on the display
    
    MOV     BX, Digit
    SHL     BX, 1       ; get 8 + nth element
    MOV     AX, segbuffer[BX]  ; accesses current segment pattern to put on display
    
    MOV DX, LEDHighByteAdd ; move high byte of segment pattern into the 
                           ; address LEDHighByteAdd
    XCHG AL, AH
    OUT DX, AL
    
    ; then, this code first takes the nth element of the segment
    ; buffer and displays it in the nth place on the display
    
    
    SHR     BX, 1               ; get back nth element
    ADD     BX, LEDDisplay     ;get the display address by offsetting original
                               ; address of display with the current index of 
                               ; the buffer being outputted
    MOV     DX, BX             ; segment can only be outputted to DX 
                               ; (reason for register change)
                               
    XCHG AL, AH
    OUT     DX, AL             ;output segment

    MOV    BX, Digit           ; increment Digit so that next time this
                               ; function is called, it outputs the next
                               ; element in the segment buffer
    INC    BX
    CMP    BX, DISPLAY_LEN
    JGE    ZeroDigit           ; accounts for potential wrapping
    JL     EndMux
    
ZeroDigit:
    MOV BX, 0
    JMP EndMux
    
EndMux:
    MOV   Digit, BX
    RET

MuxSegPatterns	ENDP

CODE    ENDS

DATA    SEGMENT PUBLIC  'DATA'

segbuffer       DW BUFFER_LENGTH  DUP (?) 
stringbuffer    DB DISPLAY_LEN DUP (?)
Digit           DW                  ?               ;the current digit number

DATA    ENDS


END
