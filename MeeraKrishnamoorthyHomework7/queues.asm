        NAME    QUEUES

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   Queues                                   ;
;   Functions to create a queue, and add and remove things from the queue    ;
;                                   EE/CS 51                                 ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; File Description: This file contains all the functions necessary to create
;                   and use a queue. The queue itself is a structure, which
;                   is created in the include file (queue.inc).
;                   This file has a function that initializes
;                   the queue and variables associated with the queue (the
;                   length of the queue, the head and tail pointers, the size
;                   of the elements in the queue, and an array to hold the
;                   elements of the queue). It has functions that check if the
;                   queue is empty or full based on the location of the head
;                   and tail pointers. And finally, it has functions that
;                   will enqueue or dequeue values to/ from the queue. 
;                   These functions will be sued 
;
; Table of Contents:
; 1) QueueInit: initializes all variables associated with the queue structure
; 2) QueueEmpty: sets zero flag if queue is empty, resets zero flag if
;                queue is not empty
; 3) QueueFull: sets zero flag if queue is full, resets zero flag if queue is 
;               not full
; 4) Dequeue: removes a value from the front of the queue. if queue is empty,
;             enters into infinite loop until an item is added on queue. 
; 5) Enqueue: adds a value to the end of the queue. f queue is full, enters
;             into infinite loop until an item is removed from queue. 
; 
; Revision History:
;     1/26/06  Glen George            initial revision
;    10/17/16  Meera Krishnamoorthy   wrote functional specification
;    10/22/16  Meera Krishnamoorthy     wrote code and documentation
;    10/22/16  Meera Krishnamoorthy     debugged code


$INCLUDE(queues.inc) ; includes constants

CGROUP  GROUP   CODE


CODE	SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP

; QueueInit
;
; Description: This function does all the necessary initialization to prepare
;             for queue use. After calling this procedure, the queue should be
;             empty and ready to be filled with values of a predetermined size.
;             Three variables are passed to this function: l, s, and a.
;             l (passed by AX in value), is the maximum number of items that can
;             be stored in this queue. s (passed by BL in value) specifies
;             whether each entry in the queue is a byte or a word -- if s is
;             true, the entries are words, and if s is false, the elements
;             are bytes. The queue is stored at address a which is passed in SI
;             by value. "l" is ignored because the size of the queue is set
;             to be "queue_size" in the main file (queue_size is defined in the
;             include file. 
; Operation:  After getting a, s, and l, this function initializes the queue by
;             creating the "struct" queue (defined above) with:
;              1) a length of l (predetermined in "main.asm" file to be 
;                 queue_size
;              2) an element size described by the boolean variable s(where if s
;                 is true, the queue elements are words, and if s is false,
;                 the queue elements are bytes)
;              3) an array (elements) that holds the elements of the queue, that
;                 is set to be the length "l". The type of the array is
;                 determined by the boolean variable s.
;              4) a variable representing the index of the array that is the
;                 front of the queue (int front), which is initialized to be 0
;              5) a variable representing the index of the array that is the
;                 back of the queue (int rear), which is initialized to be 0
;             This element creates that queue using the previously defined
;             struct.
;
; Arguments: a (address of queue). in register SI, size = 16 bits, type = word
;            s (boolean variable describing size of each queue element) which
;             is stored in BL, size = 8 bits, type = byte; 
;            l (total length that queue can be) which is stored in the register
;             AX, size = 16 bits, type = word
; Return Value: None.
;
; Local Variables: None.
; Shared Variables: None.
; Global Variables: None.
;
; Input: None.
; Output: None.
;
; Error Handling: None.
;
; Algorithms: None.
; Data Structures: Structure Queue
; Description: This structure defines a queue and all of its attributes:
;   1) length: the total length that the queue can be
;   2) element_size: if this variable is true, the element size is word (2
;                    bytes). if this variable is false, the element size is
;                    one byte.
;    3) front: describes the element of the array that is the front of the queue
;    4) rear: describes the element of the array that is the back of the queue
;    5) elements: an array with all the elements of the queue stored in it
;
;
; Registers Changed: None.
; Stack Depth: None.
;
; Limitations: queue can only hold bytes and words, assume valid address in
;              a, queue size has to be even so that queue can hold words
;
; Author: Meera Krishnamoorthy
; Last Modified: 10/17/16
;



QueueInit      PROC        NEAR
               PUBLIC      QueueInit

MOV [SI].front, 0 ; set the head pointer to 0
                  ; the head pointer is set to point at the first value in
                  ; the queue -- no values are in the queue yet; the head
                  ; pointer is pointed at 0
MOV [SI].rear, 0 ; set the tail pointer to 0
                  ; the tail pointer is set to point at the last value in
                  ; the queue -- no values are in the queue yet; the tail
                  ; pointer is pointed at 0
                  
MOV [SI].len, queue_size ; set the queue size to be the length of the array
    
CMP BL, 0 ; BL stores the element size stored in the queue. if it is 0, the
          ; element size to be stored in the queue is bytes, 
JE Bytes  ; thus, we jump to the bytes label
   
; the element size is set in relation to the byte size. thus, the element size
; is 1 if the elements in the queue are to be bytes, and are 2 for words,
; because a word is 2x the size of a byte
Words:
    MOV [SI].element_size, word_len
    JMP EndInit
    
Bytes:
    MOV [SI].element_size, byte_len
    
EndInit:
    RET
QueueInit	ENDP



; QueueEmpty
;
; Description: This function is passed one argument: the address of the queue
;              to be checked (which is stored in a (in register SI)).
;              This function sets the zero flag if the queue is empty, and resets
;              the zero flag if the queue has elements in it.
;
; Operation: This is done by comparing the value of the head and tail pointers.
;            If the head and tail pointers are equal, then the queue is 
;            empty, because there has to be at least one empty space in the 
;            queue when elements are in it, so the pointers are not equal
;            when the queue is full. 
;            If the head and tail pointers are equal, the comparison will set 
;            the zero flag. If the head and tail pointers are not equal, then 
;            the queue is not empty, and the comparison will reset the zero flag
; 
; Arguments: a (address of queue). in register SI, size = 16 bits, type = word
; Return Value: None.
;
; Local Variables: None.
; Shared Variables: None. 
; Global Variables: None.
;
; Input: None.
; Output: None.
;
; Error Handling: None.
;
; Algorithms: None.
; Data Structures: Structure Queue
; Description: This structure defines a queue and all of its attributes:
;   1) length: the total length that the queue can be
;   2) element_size: if this variable is true, the element size is word (2
;                    bytes). if this variable is false, the element size is
;                    one byte.
;    3) front: describes the element of the array that is the front of the queue
;              (head pointer)
;    4) rear: describes the element of the array that is the back of the queue
;               (tail pointer)
;    5) elements: an array with all the elements of the queue stored in it
;
; Limitations: queue can only hold bytes and words, assume valid address in
;              a, queue size has to be even so that queue can hold words
; Registers Changed: BX, CX, flags
; Stack Depth: None.
;
; Author: Meera Krishnamoorthy
; Last Modified: 10/17/16
;

QueueEmpty      PROC        NEAR
                PUBLIC      QueueEmpty
           
MOV BX, [SI].rear ; finds head pointer

MOV CX, [SI].front ; finds tail pointer


CMP CX, BX  ; compares head and tail pointers
            ; if the head pointer and tail pointer are set to the same
            ; place, then the queue is empty. this function compares the head
            ; and tail pointers, and sets the zero flag = 1 if the head
            ; and tail pointers are equal. 
            ; It sets the the zero flag = 0 if they are not
           

RET
QueueEmpty	ENDP


; QueueFull
;
; Description: This function is passed one argument: the address of the queue
;              to be checked (which is stored in a (in the register SI)).
;              If the queue is full, the zero flag is set. If the queue is not
;              full, the zero flag is reset.
;
; Operation: This is done by checking if the value of the head pointer and the
;            the tail pointer. If adding 1 to the tail pointer gets it to be 
;            the value of the head pointer, that means that the queue was
;            full, because the tail pointer points to the end of the queue
;            and the head pointer points to the front of the queue.
;            To account for any potential "wrapping" (the front of the array
;            is the middle of the queue, so the middle to end of the queue
;            is located at the beginning of the queue), the remainder
;            of the tail pointer + 1 divided by the length of the queue 
;            should be equal to the head pointer. 
;            Thus, the condition for the queue being full is if 
;            the tail pointer + 1 MOD the length of the array = the head 
;            pointer.
;            If this is not true, the zero flag is reset.
;
; Arguments: a (address of queue). in register SI, size = 16 bits, type = word
; Return Value: None
;
; Local Variables: None
; Shared Variables: None
; Global Variables: None
;
; Input: None
; Output: None
;
; Error Handling: None
;
; Algorithms: None
; Data Structures: Structure Queue
; Description: This structure defines a queue and all of its attributes:
;   1) length: the total length that the queue can be
;   2) element_size: if this variable is true, the element size is word (2
;                    bytes). if this variable is false, the element size is
;                    one byte.
;    3) front: describes the element of the array that is the front of the queue
;    4) rear: describes the element of the array that is the back of the queue
;    5) elements: an array with all the elements of the queue stored in it
;
; Registers Changed: AX, DX, BX, flags
; Stack Depth: None.
;
; Author: Meera Krishnamoorthy
; Last Modified:  10/17/16
;
;

QueueFull      PROC        NEAR
                PUBLIC      QueueFull

; find (tail pointer + 1 MOD length of queue)       
MOV AX, [SI].rear 
MOV DX, 0
ADD AX, byte_len
DIV [SI].len

MOV BX, [SI].front ; finds head pointer

CMP BX, DX ; this compares the head pointer to the tail pointer + 1 mod 
           ; queue length
           ; if the head pointer and tail pointer are offset by 1 (specifically,
           ; if the head pointer is at one index greater than the tail pointer),
           ; then the queue is full. this function compares the head
           ; and tail pointers, and sets the zero flag = 1 if the head
           ; and tail pointers are offset by one like described earlier
           ; It sets the the zero flag = 0 if they are not

RET
        
QueueFull	ENDP



; Dequeue
;
; Description: This function is passed the variable a (in SI), which is the address
;              the queue is stored in. If the queue is not empty, the  function
;              removes the first 8-bit value that was put on the queue (because
;              queues are FIFO structures) if the element size variable is
;              false, indicating that the elements on the queue are byte
;              sized. If the element size variable is true, the first 16-bit
;              value that was put on the queue would be removed because the
;              elements on the queue are word sized. The value of the element
;              removed will be returned in AL if the queue has byte sized
;              elements, or in AX if the queue has word sized elements.
;              This function will enter into an infinite loop if there are
;              no elements on the queue (and thus is a blocking function),
;              and it will only exit out of this loop when it can remove
;              a value from the queue.
;
; Operation: This function dequeues the queue by first calling the QueueEmpty
;            function, and then checking the flags set by the
;            QueueEmpty function. If the QueueEmpty function sets the zero flag,
;            this indicates that the queue is empty, and the function will
;            enter into an infinite loop that it cannot exit out of unless
;            it can remove an element from the queue. If the queue is not empty,
;            the function will increment the variable denoting the front
;            of the queue (the head pointer.
;            It will increment the variable by 2 if the element size
;            is words, and increment the variable by 1 if the element size is 
;            bytes.
;            The front of the queue is set to be the front of the queue
;            MOD the queue length, so that the head pointer will be
;            set to 0 when it is incremented after being previously set
;            to be the end of the queue.
;            Then, the queue will return the variable that was previously the
;            front of the queue by saving that variable before incrementing the
;            front variable and storing it in AH if the element size variable is
;            1 (the queue stores bytes), or AX if the element size variable
;            is 2 (the queue stores words).
;
; Arguments: a (address of queue). in register SI, size = 16 bits, type = word
; Return Value: None.
;
; Local Variables: dequeued_value (stores element at the front of the queue
;                  that is to be "removed")
; Shared Variables: one
; Global Variables: None
;
; Input: None
; Output: None
;
; Error Handling: None
;
; Algorithms: None
; Data Structures: Structure Queue
; Description: This structure defines a queue and all of its attributes:
;   1) length: the total length that the queue can be
;   2) element_size: if this variable is true, the element size is word (2
;                    bytes). if this variable is false, the element size is
;                    one byte.
;    3) front: describes the element of the array that is the front of the queue
;    4) rear: describes the element of the array that is the back of the queue
;    5) elements: an array with all the elements of the queue stored in it
;
;
; Registers Changed: AX, BX, CX, DX, flags
; Stack Depth: None.
; Limitations: queue can only hold bytes and words, assume valid address in
;              a, queue size has to be even so that queue can hold words
; 
; Author: Meera Krishnamoorthy
; Last Modified: 10/17/16
;


Dequeue      PROC        NEAR
                PUBLIC      Dequeue

QueueStillEmpty: ;blocking -- this creates an infinite loop that continues
                 ; to run if the queue is empty. this loop will only be
                 ; exited out of if a value is enqueued to the queue
    Call QueueEmpty
    JE QueueStillEmpty
    
QueueNotEmpty:
    CMP [SI].element_size, byte_len ;check element size
    JE RemoveByte ; if it's 1, dequeue a byte from the array
                  ; if it's 2, dequeue a word from the array
    
RemoveWord: ; dequeue word from array
    ; save the value currently in the head pointer in CX by:
    ; 1) moving the higher byte of the element in the head pointer into CH
    MOV BX, [SI].front 
    MOV CH, [SI].elements[BX]
    
    ; 2) moving the lower byte of the element in the head pointer into CL
    ADD BX, byte_len
    MOV CL, [SI].elements[BX]
    
    ; increment the head pointer (account for wrapping by taking the mod
    ; of the head pointer divided by queue length)
    ; this "erases" the value that was previously at the head pointer
    ; in the queue, by setting the front of the queue to the next element
    ; in the queue
    MOV BX, [SI].front
    ADD BX, word_len ; increment the head pointer by 2 because the elements
                      ; array is a byte array, and a word = 2 bytes.
                      ; thus dequeue-ing this queue effectively removes
                      ; two elements from the array
    MOV DX, 0
    MOV AX, BX
    DIV [SI].len
    MOV BX, DX
    
    MOV [SI].front, BX
    ; put the old value attached to the head pointer into AX (return)
    MOV AX, CX
    
    JMP End_Dequeue
  
RemoveByte: ; dequeue byte from array

    ; save value currently in the head pointer in CL (needs to be returned)
    MOV BX, [SI].front
    MOV CL, [SI].elements[BX]

    ; increment the head pointer (account for wrapping by taking the mod
    ; of the head pointer divided by queue length)
    ; this "erases" the value that was previously at the head pointer
    ; in the queue, by setting the front of the queue to the next element
    ; in the queue
    
    MOV BX, [SI].front
    ADD BX, byte_len
    MOV DX, 0
    MOV AX, BX
    DIV [SI].len
    MOV BX, DX
    
    MOV [SI].front, BX
    
    ; put old value attached to the head pointer into AL (return)
    MOV AL, CL
    
    Call QueueEmpty
    ;JMP End_Dequeue
 
End_Dequeue:
    RET

Dequeue	ENDP


; Enqueue
;
; Description: This function is passed two variables. One is a (in SI), the address
;              of the queue. The other variable is v (in AX), the value to be added
;              to the queue. If the element size is bytes, v is 8 bits (and stored
;              in AL). If the element size is words, v is 16 bits (and stored
;              in AX). This function adds v to the end of the queue, which is
;              found by using the element in the queue struct that
;              denotes the array index of last variable added to the queue. If the
;              queue is full, this function enters an infinite loop that it
;              does not exit out of until the queue is has an empty spot
;              in which v can be added. Thus, this function is also a blocking
;              function.
;
; Operation: This function dequeues the queue by first calling the QueueFull
;            function, and then checking the flags set by the QueueFull function.
;            If the zero flag is set, the queue is full, and the function
;            enters an infinite loop that it will not exit out of until
;            it can enqueue "v" to the queue. If the zero flag is reset,
;            then the queue is not empty and a value can be added to it.
;
;            The variable describing the end of the struct is incremented (if the
;            variable describes the last element in the array holding the
;            queue elements), then it is set to describe the first element in
;            the array (it wraps around). This is done by setting
;            the tail pointer equal to the tail pointer MOD the length of
;            the queue, so that it wraps around.
; 
;            Before the rear variable is incremented, if the size variable
;            is 1 (the elements in the queue are bytes), AL will be stored
;            as the last element in the queue. Else, AX will be stored
;            as the last element in the queue.
;
; Arguments: v (value to be added to the tail of the queue -- 
;            in register AX, size = 16 bits if queue size is words, 8 bits if
;            queue size is bytes, type = byte or words dependent on
;            type of array), a (address of queue, size = 16 bits, type = words)
; Return Value: None.
;
; Local Variables: None.
; Shared Variables: None
; Global Variables: None.
;
; Input: None.
; Output: None.
;
; Error Handling: None.
;
; Algorithms: checks if zero flag is reset. if it is, sets rear element
;             of queue to argument v, and then increments variable
;             representing back of queue and increments variable representing
;             size of queue. sets rear element of queue to AL if v is a byte,
;             or AX if v is a word.
;
; Data Structures: Structure Queue
; Description: This structure defines a queue and all of its attributes:
;   1) length: the total length that the queue can be
;   2) element_size: if this variable is true, the element size is word (2
;                    bytes). if this variable is false, the element size is
;                    one byte.
;    3) front: describes the element of the array that is the front of the queue
;    4) rear: describes the element of the array that is the back of the queue
;    5) elements: an array with all the elements of the queue stored in it
;
; Registers Changed: AX, BX, CX, DX, flags
; Stack Depth: none
;
; Limitations: queue can only hold bytes and words, assume valid address in
;              a, queue size has to be even so that queue can hold words
; 
; Author: Meera Krishnamoorthy
; Last Modified: 10/22/16
;
;


Enqueue      PROC        NEAR
                PUBLIC      Enqueue
MOV CX, AX ; transfers the variable to be enqueued because it is stored
           ; in AX, and AX may be changed over the course of the code before
           ; we get to put it in the queue
           
QueueStillFull:;blocking -- this creates an infinite loop that continues
                ; to run if the queue is full. this loop will only be
                ; exited out of if a value is dequeued from the queue
    Call QueueFull
    JE QueueStillFull

QueueNotFull:
    MOV BX, [SI].element_size ; checks the element size
    CMP BX, byte_len
    JE AddByte ; enqueues a byte if the element size is 1, enqueues a word
               ; if the element size is 0
    
AddWord:
    ; because the queue only holds bytes, the higher and lower bytes of the
    ; word to be enqueued will have to be added to the queue one by one
    
    
	MOV BX, [SI].rear ; get array index
	MOV [SI].elements[BX], CH ; adds the higher byte of the value to 
                              ; be enqueued to the array
    
    
    ADD BX, byte_len ; increment array index
    MOV [SI].elements[BX], CL ; add the lower byte of the value to be 
                              ; enqueued to the array
    
    ;increment tail pointer to increase the size of the queue (which is
    ; |tail pointer - head pointer| )
    MOV BX, [SI].rear
    MOV DX, 0
	ADD BX, word_len ; increment tail pointer by 2 because the queue holds
                     ; bytes, and a word is 2 bytes, so the queue is 
                     ; technically storing 2 bytes here
	MOV AX, BX
	DIV [SI].len ; accounts for wrapping (tail pointer = queue length + 2 = 0)
	MOV BX, DX
    
    MOV [SI].rear, BX
    JMP End_Enqueue
  
AddByte:  
	MOV BX, [SI].rear    ; get array index
	MOV [SI].elements[BX], CL ; add value to be enqueued to array to array
    
    ;increment tail pointer to increase the size of the queue (which is
    ; |tail pointer - head pointer| )
    MOV BX, [SI].rear
    MOV DX, 0
	ADD BX, byte_len
	MOV AX, BX
	DIV [SI].len ; accounts for wrapping (tail pointer = queue length + 1 = 0)
	MOV BX, DX    
    
    MOV [SI].rear, BX
    
    JMP End_Enqueue

End_Enqueue:
    RET

   
Enqueue	ENDP

CODE    ENDS


        END
