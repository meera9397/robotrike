        NAME    CONVERTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   CONVERTS                                 ;
;                             Conversion Functions                           ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description: This file contains two conversion functions. One function
; converts from a decimal value to a decimal representation of that value in
; ASCII string, and the other converts from a decimal value to a hexadecimal
; representation of that value in ASCII string. 
;
; Table of Contents
; 1) Revision History: This contains a brief description of who has changed 
;                      this file, and what they have done.
; 2) Functional Specification for Dec2String: This contains a description of 
;                      what the function Dec2String does, the registers
;                       is uses, the types of variables it uses, the arguments
;                       it takes in, and the algorithm it uses. 
; 3) Pseudocode for Dec2String: This contains a more in depth description
;                        of what Dec2String does -- the pseudocode
;                        has a list of steps that Dec2String must accomplish, 
;                        and those steps are translated into assembly code. 
; 4) Actual code for Dec2String: This contains the commented assembly code for 
;                        the Dec2String function. 
; 5) Functional Specification for Hex2String: This contains a description of 
;                      what the function Hex2String does, the registers
;                       is uses, the types of variables it uses, the arguments
;                       it takes in, and the algorithm it uses. 
; 6) Pseudocode for Hex2String: This contains a more in depth description
;                        of what Hex2String does -- the pseudocode
;                        has a list of steps that Dec2String must accomplish, 
;                        and those steps are translated into assembly code. 
; 7) Actual code for Hex2String: This contains the commented assembly code for 
;                        the Hex2String function. 
;
; Revision History:
;     1/26/06  Glen George              initial revision
;    10/15/16  Meera Krishnamoorthy     wrote code and documentation
;    10/15/16  Meera Krishnamoorthy     debugged code
;    10/16/16  Meera Krishnamoorthy     demoed code and added missing documentation
;

$ INCLUDE(converts.inc)
CGROUP  GROUP   CODE

CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP




; Dec2String
;
; Description: This function converts a 16-bit signed decimal value to an ASCII string. 
; This function can represent any number between -(2^15) and (2^15)-1. 
; The signed decimal value is the number ‘n’ passed to the function, and the ASCII representation 
; of the decimal should be stored in ‘a’, the second number passed to the function.
; This is done by adding 48, or the ASCII value of the number 0, to each digit of the decimal value,
; and ‘null’ to the string after all the digits have been added to that string.
; If the number passed to the function is signed, a negative character will be added to the 
; ASCII string, and the number will be converted to its 2's complement. That 2's complement will
; then be converted from decimal to ASCII. 
;
; Operation: The function starts by checking if the number is negative by checking its first bit. 
; If the first bit is set, the number is signed, and we negate that number and translate that result 
; into an ASCII string. We also add a ‘-‘ to the string to represent that the number is negative.
; Then, we divide the number by successive powers of 10, starting at 10^5, as the largest and smallest 
; numbers that this function can represent are of the order of 10^5. 
; Each division of 10 produces a digit in that number. We add 48 (the ASCII value corresponding to the
; number 0) to each digit to convert it to ASCII, and we store each digit in the passed address a.
; Each time we store a digit in a, we increment a to move to the next memory location.
;
; Arguments: 
; 1) AX – binary value to convert to decimal representation in ASCII
; 2) SI – address where the ASCII representation of 16 bit signed decimal value should be put

; Return Value: NONE
;
; Local Variables: 
; 1) pwr10: current power of 10 being computed (CX)
; 2) digit: current digit necessary to convert to ascii (BX)
; 3) argMOD: the remainder of arg/ pwr10 (DX)

; Shared Variables: None
; Global Variables: None
;
; Input: None
; Output: None
;
; Error Handling: None
;
; Algorithms: Repeatedly divide by powers of 10 to get each digit. 
;             Add 48 (ASCII representation of 0) to each character to get each digit, and 
;             store the result in SI.

; Data Structures: NONE
;
; Registers Changed: AX, BX, CX, DX, SI
; Stack Depth: None
;
; Author: Meera Krishnamoorthy
; Last Modified: 10/15/16

Dec2String      PROC        NEAR
                PUBLIC      Dec2String



DecToStringInit:
    MOV CX, INIT_POWER ; CX is where the power of 10 of each digit is stored
    CLC ; clears all the flags before a comparison to make sure no extraneous flag values are there
    CMP AX, 0 ; AX is the number to be converted to ASCII. compare it to 0 to know if it is signed or not
              ; a comparison effectively subtracts the number after the ',' from the number before
              ; the ','. This result will be less than 0 if the number is negative, and greater than 
              ; or equal to to 0 if the number is positive or 0. 
              ; Thus, the carry flag will be set if AX < 0, and if AX >= 0, the carry flag will not be set
              ; We can use the carry flag as a condition here.
    JL Negation ; If the number is signed, we have to take the 2's complement of it, so we move to a
                ; different loop
    JAE CalculateASCII ; if number is not signed, we can continue

Negation:
    NEG AX ; this takes the 2's complement of the number
    MOV byte ptr[SI], ASCII_negsign ; This moves a negative sign into the address where the ASCII string should be stored
                                    ; Thus, we can represent a decimal value as negative. 
    INC SI ; The address is incremented (something is stored in SI, store the next thing in SI + 1)
 
CalculateASCII:
    MOV DX, 0 ; clears out DX -- this is where the remainder is stored, so it should be 0
              ; in order for accurate division to happen. 
    DIV CX ; divides the argument by the current power of 10 to obtain a digit of that number
    MOV BX, DX ; copies the remainder of that division into a register -- that remainder
               ; contains the rest of the digits to be converted to ASCII, so we will
               ; need to set that to our argument in the future loop. 
    
    ADD AX, ASCII_ZERO ; convert AX to an ascii character by adding 48, or the ASCII representation
                       ; of 0 to it
    MOV byte ptr[SI], AL ; because each digit is at most a byte long, we only need to store AL (the
                         ; lower 2 bits of AX) in SI. 
    INC SI  ; The address is incremented (something is stored in SI, store the next thing in SI + 1)
    
    MOV DX, 0 ; clears out DX -- this is where the remainder is stored, so it should be 0
              ; in order for accurate division to happen. 

    ; The way we find digits is by successively dividing a number by 10, and taking the integer value
    ; of each division as a digit. This part of the code finds the next power of 10 we will need to divide
    ; the number by
    
    MOV AX, CX ;A number can only be divided when it is in AX. Thus, CX (the power of 10 variable) will be 
               ; moved to AX
    MOV CX, DIV_POWER ; now that the CX value is stored in AX, we can store the dividing factor (DIV_POWER, or 10)
                      ; in CX
    DIV CX ; dividing AX by CX, we divide the current power of 10 by 10, getting the new power of 10 we will need to divide
           ; by
    MOV CX, AX ; now that we have our new power of 10, we can replace CX with that power
    MOV AX, BX ; we move the remainder of the previous division by 10 into AX, as it holds the rest of the digits
               ; we need to find
    
    CLC ; clears all the flags before a comparison to make sure no extraneous flag values are there
    CMP CX, 0 ; here we check the condition that we need to exit out of this division by 10: if the divider is
              ; greater than 0. if it is, we can keep dividing. if not, we have to stop dividing because
              ; division by 0 is illegal.
    JA CalculateASCII ; if the power of 10 variable is still greater than 0, we can continue to find digits
    JBE LastStep ; if it is not, we can terminate by moving to our last step

LastStep:
    MOV byte ptr[SI], ASCII_NULL ; this last step terminates the string by adding a null character to it
	RET

Dec2String	ENDP




; Hex2String
;
; Description: This function converts a 16-bit signed decimal value to its hexadecimal equivalent 
; as an ASCII string. 
; This function can represent any number between 0 and (16^4) - 1. 
; The signed decimal value is the number ‘n’ passed to the function, and the ASCII representation 
; of the decimal should be stored in ‘a’, the second number passed to the function.
; This is done by adding 48, or the ASCII value of the number 0, to each digit of the decimal value
; if that digit is less than 10. If that digit is greater than or equal to 10, then 65, or the ASCII
; value of the letter 'A' will be added to that digit. Then, 10 will be subtracted from that digit because
; 'A' is 10 in hexadecimal. Thus, 55 is effectively added to a digit if it is greater than 10.
; Then, ‘null’ is added to the string after all the digits have been added to that string.

;
; Operation: We start by dividing the number by successive powers of 16, starting at 16^3, because the largest
; number that can be represented is (16^4)-1, which is of the order of (16^3). 
; Each division of 16 produces a digit in that number. We add 48 (the ASCII value corresponding to the
; number 0) to each digit to convert it to ASCII if that digit is less than 10, and 55 (65 - the offset of 10) is added 
; to each digit if it is greater than 10. Then, we store each digit in the passed address a.
; Each time we store a digit in a, we increment a to move to the next memory location.
;
; Arguments: 
; 1) AX – binary value to convert to hexadecimal representation in ASCII
; 2) SI – address where the ASCII representation of 16 bit signed decimal value should be put

; Return Value: NONE
;
; Local Variables: 
; 1) pwr10: current power of 10 being computed (CX)
; 2) digit: current digit necessary to convert to ascii (BX)
; 3) argMOD: the remainder of arg/ pwr10 (DX)
; Shared Variables: NONE
; Global Variables: NONE
;
; Input: NONE
; Output: NONE
;
; Error Handling: None
;
; Algorithms: Repeatedly divide by powers of 16 to get each digit, add by 48 or 55 to convert to ASCII value, and store in a. 
; Data Structures:
;
; Registers Changed: AX, BX, CX, DX, SI
; Stack Depth: None
; 
; Pseudocode
; Arg = n
; pwr16 = 4096
; WHILE (Pwr16 > 0)
;   digit = arg/pwr16
;   Store remainder of arg/pwr16 in argMOD
;   IF (digit < 10) 
;       STORE(digit + ‘0’, a)
;   ELSE
;       STORE(digit + ‘A’, a)
;   ENDIF
;   pwr16 = pwr16 / 16
;   arg = argMOD
; ENDWHILE
; Add null character to address a

; Author: Meera Krishnamoorthy
; Last Modified: 10/15/16

Hex2String      PROC        NEAR
                PUBLIC      Hex2String


HexToStringInit:
    MOV CX, INIT_HEX_POWER ; CX is where the power of 16 of each digit is stored 

CalculateDigit:
    MOV DX, 0 ; clear out area where remainder will be stored
    DIV CX ; divide number by current power of 10 to obtain current digit
    MOV BX, DX ;copy remainder of division into a different register to save
               ; this value will be used to find the rest of the digits
               ; in the number
    ;JMP ConvertDigitASCII
    
ConvertDigitASCII: ; this converts digits to their ascii equivalents 
    CLC ; clears all the flags before a comparison to make sure no extraneous flag values are there
    CMP AX, NUM_TO_DIG ; compare the number with 10
    JB AddZero ; if the number is less than 10, we add '0' to it
    JAE AddLetters ; if the number is greater than or equal to 10, we add 'A' to it
    
AddZero:
    ADD AX, ASCII_ZERO ; adds the ascii representation of 0 (48) to the digit
    MOV byte ptr[SI], AL ; stores that digit in SI
                         ; again,  because each digit is at most a byte long, we only need to store AL (the
                         ; lower 2 bits of AX) in SI. 
    INC SI ; The address is incremented (something is stored in SI, store the next thing in SI + 1)
    JMP ContinueASCIIHex
    
AddLetters:
    ADD AX, ASCII_A_offset ; adds the ascii representation of A (65) to the digit. then it subtracts
                           ; 10 from that digit because A is 10 in hex. Thus, ASCII_A_offset is
                           ; 65 - 10, or 55. 
    MOV byte ptr[SI], AL ; stores digit in SI
    INC SI ; The address is incremented (something is stored in SI, store the next thing in SI + 1)
    ;JMP ContinueASCIIHex
    
 ContinueASCIIHex: 
    MOV DX, 0 ; clear out area where remainder will be stored
    
    ; The way we find digits is by successively dividing a number by 16, and taking the integer value
    ; of each division as a digit. This part of the code finds the next power of 16 we will need to divide
    ; the number by. 
    
    MOV AX, CX ; division can only occur with AX as the quotient. Thus the power of 16 variable, CX, is stored in AX
    MOV CX, DIV_HEX_POWER ; the power of 16 variable needs to be divided by by 16. thus, 16 is being stored in
                          ; CX after the CX value is stored in AX.
    DIV CX  ; pwr of 16 variable is divided by 16
    MOV CX, AX ; now that we have our new power of 16, that value is moved back into CX to be used at the beginning
               ; of the ConvertDigitASCII loop
    
    MOV AX, BX ; this puts the remainder of the AX/CX division into AX, so that this process can continue
    
    CLC ; clears all the flags before a comparison to make sure no extraneous flag values are there
    CMP CX, 0 ; here we check the condition that we need to exit out of this division by 16: if the divider is
              ; greater than 0. if it is, we can keep dividing. if not, we have to stop dividing because
              ; division by 0 is illegal.
    JA CalculateDigit ; if we can still divide our power of 16 variable, we continue looping through this code to
                      ; find digits
    JBE EndHex ; if not, we can move on to our last step
    
EndHex:
    MOV byte ptr[SI], ASCII_NULL ; this last step terminates the string by adding a null character to it
    RET

Hex2String	ENDP



CODE    ENDS



        END
