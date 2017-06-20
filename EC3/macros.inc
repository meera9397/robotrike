$NOLIST


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 MACROS.INC                                ;
;                            Simple Macro Examples                           ;
;                                 Include File                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains useful macros.
; Table of contents
; CLR(reg): clears register reg
; SETBIT(reg, bit): sets #bit in value stored in register regs
; CLRBIT(reg, bit): clears #bit in value stored in register regs
; COMBIT(reg, bit): sets #bit to its complement in register regs
; TESTBIT(reg, bit): sets zero flag if #bit is not set in register regs
; 					 and resets it if bit is set
; XLATW: looks up table representation of AX (address of table is in BX)
; READPCB(addr): reads in value pcb register stored in addr
; WRITEPCB(addr, val): writes value val to pcb register stored in addr
;
; Revision History
;    11/20/16  Meera Krishnamoorthy wrote macros


; local include files
;   none




; CLR
;
; Description:       This macro clears the register reg (sets it to 0). It is
; 					 assumed that reg is not a segment register.
; 					 ex: CLR(AX) sets AX to 0
;
; Operation:         sets register to 0
;
; Arguments:         reg - register to clear
;
; Registers Changed: regs
; Stack Depth:       0 words
;
; Author:            Meera Krishnamoorthy
; Last Modified:     11/21/16

%*DEFINE(CLR(reg))   (
        MOV %reg, 0 ; clears register
)




; SETBIT
;
; Description:       This macro sets bit #bit in register reg.
;                    It is assumed that reg is not a segment register.
;
; Operation:         value with #bit set or-ed with #reg to set bit in reg
;
; Arguments:         reg - register to change
;                    bit - bit to set in register
;
; Registers Changed: AX
; Stack Depth:       0 words
;
; Author:            Meera Krishnamoorthy
; Last Modified:     11/21/16

%*DEFINE(SETBIT(reg, bit))   (
		MOV DX, %reg
        MOV AX, 1 
        SHL AX, %bit ; only bit set in AX is "bit"
        OR DX, AX ; ensures that "bit" is set in AX because anythign or-ed
				  ; with 1 is 1
		MOV %reg, DX
)




; CLRBIT
;
; Description:       This macro clears bit #bit in register reg
;
; Operation:         This is done by setting all bits except #bit 
; 					 and anding that value with #regs to clear #bit in regs
;
; Arguments:         reg - register to change
;                    bit - bit to set in register
;
; Registers Changed: IF
; Stack Depth:       0 words
;
; Author:            Meera Krishnamoorthy
; Last Modified:     11/21/16

%*DEFINE(CLRBIT(reg, bit))   (
	MOV DX, %reg
    MOV AX, 1 
    SHL AX, %bit ; only bit set in AX is #bit
	NOT AX ; sets AX to its complement so all bits are set except AX
    AND DX, AX ; anding something with 1 retains its value, and anding
			   ; something with 0 sets its value to 0. this ensures all
			   ; bits remain the same except #bit (because all other
			   ; bits are anded with 1), and that #bit is anded with
			   ; 0
	MOV %reg, DX
)




; COMBIT
;
; Description:       This macro complements bit #bit in register reg
;
; Operation:         This function XORS the register to a value that has 
; 					 all bits cleared except for #bit. The xor will set reg to 
; 					 have a value opposite to the current value of #bit. 
;
; Arguments:         reg - register to change
;                    bit - bit to set in register
;
; Registers Changed: IF
; Stack Depth:       0 words
;
; Author:            Meera Krishnamoorthy
; Last Modified:     11/21/16

%*DEFINE(COMBIT(reg, bit))   (
	MOV DX, %reg
    MOV AX, 1
    SHL AX, %bit ; only bit set in AX is #bit
    XOR DX, AX   ; anything x-ored with 0 retains its value.
				 ; AX is 0 except for at #bit. 
                 ; xor will make sure everything stays the same except
                 ; #bit. if bit is 0, it will be 1, and if 1, it will be 0
	MOV %reg, DX
)




; TESTBIT
;
; Description:       This macro sets the zero flag based on bit #bit in 
; 					 register reg
;
; Operation:         This is done by masking all bits except #bit in #regs and 
;					 comparing that value to a register that is set to 
; 					 contain all bits cleared except #bit.
;
; Arguments:         reg - register to change
;                    bit - bit to set in register
;
; Registers Changed: AX
; Stack Depth:       0 words
;
; Author:            Meera Krishnamoorthy
; Last Modified:     11/21/16

%*DEFINE(TESTBIT(reg, bit))   (
        MOV AX, 1
        SHL AX, %bit ;all bits cleared in AX except #bit
        MOV BX, %reg 
        AND BX, AX ; masks all bits except #bit
				   ; bx will be 0 if #bit is not set, and 1 if it is
		MOV CX, 0
        CMP BX, CX ; sets zero flag if bit of AX is 0 and resets flag if bit
                   ; is 1

)




; XLATW
;
; Description:       This macro translate the 16-bit value in AX to another 16
; 					 bit value (updating AX) using the table pointed to by BX.
;
; Operation:		This is done by setting AX equal to the value pointed to
; 				    by the table address + AX ([BX + AX])
;
; Arguments:         AX: value to translate
; 					 BX: address of table
;
; Registers Changed: AX
; Stack Depth:       0 words
;
; Author:            Meera Krishnamoorthy
; Last Modified:     11/21/16

%*DEFINE(XLATW)   (
		ADD BX, AX ; adds AX to address of table because that is the location
				   ; of the table value corresponding to AX
		MOV AX, word ptr CS:[BX] ; sets AX to offset of AX in table 
								 ; (pointed to by BX)
)




; READPCB
;
; Description:       This macro read the PCB register at address addr and 
;                    returns the value in AX. 
;
; Operation:         This is done by using the in function with addr in DX
;
; Arguments:         addr: address of register
;
; Registers Changed: AX, DX
; Stack Depth:       0 words
;
; Author:            Meera Krishnamoorthy
; Last Modified:     11/21/16

%*DEFINE(READPCB(addr))   (
		MOV DX, %addr
        IN AX, DX ; reads in value at address DX
)




; WRITEPCB
;
; Description:       This macro writes the 16-bit value val to the PCB
;                    register at address addr
;
; Operation:         This is done by using the out function with val in AX and addr
;                    in DX.
;
; Arguments:         addr: address in register
; 					 val: value to set register to
;
; Registers Changed: AX, DX
; Stack Depth:       0 words
;
; Author:            Meera Krishnamoorthy
; Last Modified:     11/21/16

%*DEFINE(WRITEPCB(addr, val))   (
		MOV DX, %addr
		MOV AX, %val
        OUT DX, AX ; outputs value in AX to address DX
)


$LIST
