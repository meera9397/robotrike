;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  QUEUES.INC                                ;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains constants for the functions needed to initialize and use
; a queue (which are defined in queues.asm)
;
; Revision History:
;    10/22/16  Meera Krishnamoorthy updated constants/ queue structure


queue_size equ 513 ; this variable describes the size of the queue
word_len equ 2 ; the size of a word is 2x the size of a bytes
byte_len equ 1 ; the size of a byte is set to "1"

; The following structure creates a queue with the following attributes:
; 1) len: the total length the queue can be
; 2) element_size: if this variable is 0, the element size is bytes. if this
;                  variable is 1, then the element size is words.
; 3) front: head pointer for queue
; 4) rear: tail pointer for queue
; 5) elements: the array to put all elements of the queue in 
; 
QueueStruct         STRUC
    len             DW      ?
    element_size    DW      ?
    front           DW      ?
    rear            DW      ?
    elements        DB      (queue_size) DUP (?)
QueueStruct ENDS