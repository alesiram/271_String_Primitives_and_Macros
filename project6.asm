TITLE String Primitives - Macros     (Proj6_Vasquem2.asm)

; Author: Marisela Vasquez 
; Last Modified: 08/18/2023
; OSU email address: Vasquem2@oregonstate.edu
; Course number/section: 400   CS271 Section 400
; Project Numser:    6      Due Date: 08/18/2023
; Description: 
;	This MASM program contains 2 macros mGetString and mDisplayString, 
;	2 procedures ReadVal and WriteVal. The program accepts 10 user inputs, it then 
;	validates the inputs, ensuring they meet the specified requirements. It then converst the 
;	unser inputs into numeric SDWORD value  to a string of ASCII digits for displaying. 
;	The program lastly displays an array of user entered inputs, the sum and the average 
;	of those numbers. 

INCLUDE Irvine32.inc

;---------------------------------------------------------------------------
; Name: mGetString 
; 
; The mGetString macro displays a prompt, then get the user’s keyboard input into a memory location. 
; 
; Preconditions: 
;	- stack must be set up correctly and 4 paramenters must be pushed to the stack 
; 
; Receives: 
;	- inputMsg: prompt to enter number 
;	- numsString: user number is stored within this variable 
;	- inputLenAllowed: amount of bytes that should saved 
;	- stringLength	: variabel that holds length of input 
;
; Returns:
;	- numsString: number that was entered 
;	- stringLength: number length 
;---------------------------------------------------------------------------
mGetString MACRO inputMsg, numsString, inputLenAllowed, stringLength
	; Saving registers
	PUSH			EAX
	PUSH			ECX
	PUSH			EDX

	; print input message with mDisplayString
	; Get and save user input
	mDisplayString	inputMsg
	MOV				EDX, numsString
	MOV				ECX, inputLenAllowed
	CALL			ReadString
	MOV				stringLength, EAX

	; Restoring registers
	POP				EDX
	POP				ECX
	POP				EAX
ENDM

;---------------------------------------------------------------------------
; Name: mDisplayString 
; 
; Macro prints the parameter passed which is a string 
; 
; Preconditions:  
;	- Stack needs to be set up correctly and string must be pushed to the stack
; 
;Receives: 
;	- string at addres [EBP + 8]
; 
; Returns: 
;	- message printed to the console 
;	- no registes changed as they are saved and restored 
;---------------------------------------------------------------------------
mDisplayString		MACRO string
	PUSH			EDX
	MOV				EDX, string
	CALL			WriteString			
	POP				EDX

ENDM

	; program constant
	MAXLENGTH = 12

.data
	; program general prompts
	progTitle		BYTE	"PROGRAMMING ASSIGNMENT 6: Designing low-level I/O procedures", 13, 10, 0
	progName		BYTE	"Written by:  Marisela Vasquez", 13, 10, 13, 10, 0
	intro1			BYTE	"Please provide 10 signed decimal integers. ", 13, 10,0
	intro2			BYTE	"Each number needs to be small enough to fit inside a 32 bit register.", 13, 10,0
	intro3			BYTE	"After you have finished inputting the raw numbers I will display a ",13,10,0
	intro4			BYTE	"list of the integers you entered, their sum, and their average value.",13, 10, 13, 10,0
	goodbye			BYTE	13,10,13, 10, 13,"Thanks for playing!", 13, 10, 0
	space			BYTE	" ",0
	comma			BYTE	",", 0

	; data passed on the stack  
	inputMsg		BYTE	"Please enter a signed number: ", 0
	errorMsg		BYTE	"ERROR: You did not enter a signed number or your number was too big.", 13, 10, 0
	userInput		BYTE	MAXLENGTH DUP(?)
	inputLen		DWORD	0
	errorFlag		DWORD	0
	numArray		SDWORD	10 DUP(?)
	numberSign		SDWORD	1

	; data to be printed
	totalNumsMsg	BYTE	13,10,"You entered the following numbers: ", 13, 10, 0
	totalSumMsg		BYTE	13,10,"The sum of these numbers is: ", 0
	averageMsg		BYTE	13,10,"The truncated average is: ", 0
	stringArray		BYTE	MAXLENGTH DUP(?)


.code
main PROC

	; display introduction prompts
	mDisplayString	OFFSET progTitle				
	mDisplayString	OFFSET progName
	mDisplayString	OFFSET intro1
	mDisplayString	OFFSET intro2
	mDisplayString	OFFSET intro3
	mDisplayString	OFFSET intro4

	; Get 10 valid integers from the user. 
	; ReadVal will be called within the loop. 
	mov				ECX, 10									; loop counter 
	mov				EDI, OFFSET numArray

_inputLoop:
	; current values 
	PUSH			ECX										; stack address [EBP + 68]
	PUSH			EDI										; stack address [EBP + 64]
	
	; procedure data variables 
	PUSH			OFFSET stringArray						; stack address [EBP + 60]
	PUSH			OFFSET numArray							; stack address [EBP + 56]
	PUSH			OFFSET inputLen							; stack address [EBP + 52]
	PUSH			OFFSET userInput						; stack address [EBP + 48]

	; procedure messages and signs 
	; update to next element in array 
	PUSH			OFFSET errorFlag						; stack address [EBP + 44]
	PUSH			OFFSET numberSign						; stack address [EBP + 40]
	PUSH			OFFSET errorMsg							; stack address [EBP + 36]
	PUSH			OFFSET inputMsg							; stack address [EBP + 32]
	CALL			ReadVal
	ADD				EDI, TYPE SDWORD			
	LOOP			_inputLoop

	; program titles to display 
	push			OFFSET space							; stack address [EBP + 52]
	PUSH			OFFSET comma							; stack address [EBP + 48]
	PUSH			OFFSET averageMsg						; stack address [EBP + 44]
	PUSH			OFFSET totalNumsMsg						; stack address [EBP + 40]
	PUSH			OFFSET totalSumMsg						; stack address [EBP + 36]

	; program data to display 
	PUSH			OFFSET numArray							; stack address EBP + 32]
	PUSH			OFFSET stringArray						; stack address [EBP + 28]
	CALL			display									; procedure calls WriteVal from within procedure

	; display goodbye message 
	mDisplayString	offset goodbye

	Invoke ExitProcess,0									; exit to operating system
main ENDP

;---------------------------------------------------------------------------
; Name: ReadVal 
;  
; The ReadVal procedure invokes the mGetString macro to collect a string of digits as 
; user input. This string is then converted into a numeric value (SDWORD) through string
; manipulation, with validation to ensure it contains only valid numeric characters. 
; The resulting numeric value is stored in a memory variable, acting as an output 
; parameter by reference.
;
; Preconditions: 
;   - The stack should be set up correctly, with the required parameters pushed 
;     onto the stack in the correct order.
;
; Postconditions:
;	- None, all registers are restored. 
;
; Receives: 
;	- ECX at stack address [EBP + 68]
;	- EDI at stack address [EBP + 64]
;	- stringArray at stack address [EBP + 60]
;	- numArray at stack address [EBP + 56]
;	- inputLen at stack address [EBP + 52]
;	- userInput at stack address [EBP + 48]
;	- errorFlag	 at address [EBP + 44]
;	- numberSign at stack address [EBP + 40]
;	- errorMsg at stack address [EBP + 36]
;	
; Returns: 
;	- ECX at stack address [EBP + 68]
;	- inputLen at stack address [EBP + 52]
;	- userInput at stack address [EBP + 48]
;	- errorOccurred at address [EBP + 44]
;	- numberSign at stack address [EBP + 40]
;---------------------------------------------------------------------------
ReadVal PROC USES EAX EBX ECX EDX ESI EDI
	 ; Set the stack frame
	PUSH			EBP
	MOV				EBP, ESP

_startInputLoop:
	; Set registers
	MOV				ECX, EAX							; moving eax val into ecx
	MOV				ESI, [EBP + 56]						; set numArray 
	MOV				EBX, 0								
			
	; call mGetString macro to get user input string
	mGetString	[EBP + 32], [EBP + 48], MAXLENGTH,  [EBP + 52], [EBP + 60]

	; call sub-procedure to validate user input 
	PUSH			[EBP + 40]							; numberSign
	PUSH			[EBP + 44]							; errorFlag
	PUSH			[EBP + 52]							; inputLen
	PUSH			[EBP + 48]							; userInput
	CALL			inputValidator
	JMP				_prepConversion

; converts string to interger 
; calls charProcessing 
_prepConversion:
	MOV				EAX, [EBP + 44]						; moving errorFlag for comparison with 0
	MOV				EAX, [EAX]
	CMP				EAX, 0
	JNE				_error

	; pushing registers to call charProcessing procedure 
	push			[EBP + 64]							; EDI
	push			[EBP + 52]							; inputLen
	push			[EBP + 48]							; userInput
	push			[EBP + 44]							; errorFlag
	push			[EBP + 40]							; numberSign
	call			charProcessing

	; Check if the numeric value is zero
	mov				EAX, [EBP + 44]						; Load the value of inputsign
	mov				EAX, [EAX]							; Dereference to obtain the actual value
	cmp				EAX, 0								; Compare with zero
	JE				_stop								; Jump to _stop if the value is zero

_error:
	; using mDisplayString macro to display error message
	mDisplayString	[EBP + 36]
	MOV				EAX, [EBP + 44]
	MOV				DWORD PTR [EAX], 0					; Reset error flag: errorFlag
	JMP				_startInputLoop						; branch to start of loop 
		
_stop:
	; restore registers 
	POP				EBP
	RET				44
ReadVal ENDP

;---------------------------------------------------------------------------
; Name: inputValidator
;  
;	inputValidator is a ReadVal sub-procedure, it validates all entered data. 
;	data is valid if it meets the following criteria: 
;		1. must be numeric input
;		2. the length is within the range 
;		3. any leading sign (+, -, or no sign) is correctly handled.
;	
; Preconditions:
;	- Stack must be set up correctly before calling this sub-procedure. 
; 
; Postconditions: 
;	- EAX ECX EDX ESI are modified but restored before proecudure ends.
; 
; Receives: 
;	- numberSign at stack address [EBP + 36]					
;	- errorFlag at stack address  [EBP + 32]							
;	- inputLen at stack address  [EBP + 28]							
;	- userInput at stack address [EBP + 24]						
;
; Returns:
;	- numberSign at stack address [EBP + 36]						
;	- errorFlag at stack address  [EBP + 32]
;---------------------------------------------------------------------------
inputValidator PROC USES EAX ECX EDX ESI
	; set the stack 
	push			EBP
	mov				EBP, ESP

	; get user value and input length 
	; length check 
	MOV				ESI, [EBP + 24]							; userInput
	MOV				ECX, [EBP + 28]							; inputLen
	CMP				ECX, 0
	JLE				_notInRange								; branch if less than or equal 								
	CMP				ECX, 12
	JGE				_notInRange								; branch if greater than
	
	; prepping for sub-procedure
	; loading first ASCII character
	XOR				EAX, EAX								; setting EAX to zero
	CLD
	LODSB
	PUSH			[EBP + 36]								; numberSign 
	PUSH			[EBP + 32]								; errorOccurred 
	PUSH			EAX
	CALL			firstChar								; call sub-proecedure to check sign of num
	
	; length check
	DEC				ECX
	CMP				ECX, 0
	JLE				_exit									; branch to exit if needed 

_nextChar:
	XOR				 EAX, EAX								; setting EAX to zero
	CLD														; Clear the direction flag for LODSB to increments source pointer
	LODSB													; Load the byte at the address pointed to by the source pointer (ESI) into AL and increment ESI                                            
			
    ; number range check: 48 = 0, 57 = 9
    CMP				EAX, 30h
    JB				_notInRange
    CMP				EAX, 39h
    JA				_notInRange								; branch to not in range 
    JMP				_continueLoop							; while valid, branch to next 
	
_notInRange:
    ; updating error flag 
    MOV				EAX, [EBP + 32]
    MOV				DWORD ptr [EAX], 1

_continueLoop:
    ; if invalid branch exit 
	; if valid go to next char 
    MOV				 EAX, 0
    MOV				 EDX, [EBP + 32]
    CMP				 EAX, [EDX]
    JNE				_exit
    LOOP			_nextChar								; if valid branch to next character 
	JMP				_exit

_exit:
	; restore register
	pop				EBP
	ret				16

inputValidator ENDP

;---------------------------------------------------------------------------
; Name: firstChar 
;  
;	Sub-procedure validates the first character of the input.  
;	Procedure process characters +, -, or numerical inputs 	
; 
; Preconditions: 
;	- Stack must be set up correctly before this procedure is called. 
; 
; Postconditions: None. 
; 
; Receives: 
;	- numberSign at stack address [EBP + 24]
;	- errorOccurred at stack address [EBP + 20]
;		
; Returns: 
;	- numberSign at stack address [EBP + 24]
;	- errorOccurred at stack address [EBP + 20]
;---------------------------------------------------------------------------
firstChar PROC uses EAX EDX
	; set the stack 
	PUSH			EBP
	MOV				EBP, ESP

	; comparison block for first character
	MOV				EAX, [EBP + 16]						; ASCII character 
	CMP				EAX, 2Dh							; Compare with '-'
	JE				_negChar
	CMP				EAX, 2Bh							; Compare with '+'		
	JE				_exit	
	CMP				EAX, 30h							; Compare with '0'			
	JB				_notInRange
	CMP				EAX, 39h							; Compare with '9'		
	JA				_notInRange
	JMP				_exit						
			
; if out of range 
_notInRange:
	MOV				EAX, [EBP + 20]
	MOV				DWORD ptr [EAX], 1					; update errorFlag

; if character is negative
; accept it
_negChar:
	MOV				EAX, [EBP + 24]
	MOV				EDX, -1
	MOV				[EAX], EDX
	JMP				_exit
			
_exit:
	; restoring register 
	pop				EBP
	RET				12

firstChar ENDP


;---------------------------------------------------------------------------
; Name: charProcessing 
;  
;	Sub-procedure responsible for converting the string input to a signed double word (SDWORD).
; 
; Preconditions: 
;	- stack should be correctly set up before being called. 
; 
; Postconditions: None 
; 
; Receives:
;	- EDI at stack address [EBP + 48] 
;	- inputLen at stack address [EBP + 44] 
;	- userInput at stack address [EBP + 40]
;	- errorFlag at stack address [EBP + 36]
;	- numberSign at stack address [EBP + 32] 
;
; Returns: 
;	- EDI at stack address [EBP + 48] 
;	- userInput at stack address [EBP + 40]			
;---------------------------------------------------------------------------
charProcessing PROC uses EAX EBX ECX EDX ESI EDI
	; set the stack 
	PUSH			EBP
	MOV				EBP, ESP

	; set registers 
	MOV				EDI, [EBP + 48]						; EDI 
	MOV				ESI, [EBP + 40]						; userInput
	MOV				ECX, [EBP + 44]						; inputLen


	; set edx and ebx registers 
	MOV				EDX, 1								
	MOV				EBX, 0								; val at 0
	jmp				_startLoop							; branch to start loop 
	
_startLoop:
	; initializes the loop for 
	; processing the input string
	MOV				EAX, ECX 
	DEC				EAX 
	ADD				ESI, EAX  

_processLoop:
	XOR				 EAX, EAX							; setting EAX to zero
	STD											
	LODSB												; Load the byte at the address pointed 

	; compare neg sign 
	; branch if neg 
	CMP				EAX, 2Dh		
	JE				_applySign

	; compare pos sign
	; branch if pos
	CMP				EAX, 2Bh		 
	JE				_applySign
	

	; multiplying it by power of 10, 
	; and adding it to the accumulated sum. 
	; branches to next segment 
	SUB				EAX, 30h							; Convert ASCII digit to numerical value
	PUSH			EDX									; Save the previous power of 10
	IMUL			EDX	
	ADD				EBX, EAX							; add to the total sum  
	JO				_error
	JMP				_next

_next:
	; Pop the current power of 10 from the stack
	POP				EDX										
	MOV				EAX, EDX

	;Set EDX as the multiplier
	MOV				EDX, 10									
	IMUL			EDX

	; Update the new power of 10 in EDX
	MOV				EDX, EAX								
	LOOP			_processLoop

_applySign:
	; applies the appropriate sign to the accumulated
	; integer value based on the sign flag
	MOV				EAX, EBX

	; set numberSign 
	MOV				EBX, [EBP + 32]					
	MOV				EBX, [EBX]
	IMUL			EBX						
	JMP				_reset
	
_reset:
	; sets the sign to positive
	; saves the processed integer value 
	; to the array
	MOV				EBX, [EBP + 32]	
	MOV				SDWORD PTR [EBX], 1
	MOV				[EDI], EAX
	JMP				_exit

_error:
	; update error flag 
	pop				EDX
	mov				EAX, [EBP + 36]
	mov				DWORD PTR [EAX], 1

_exit:
	; restore registers
	pop				EBP
	ret				20
charProcessing ENDP

;---------------------------------------------------------------------------
; Name: WriteVal 
;  
;	Procedure converts a numeric SDWORD value (input parameter, by value) to a string 
;	of ASCII digits. It invokes the mDisplayString macro to print the ASCII representation 
;	of the SDWORD value to the output.
;				
; Preconditions: 
;   - The stack must be correctly set before being called. 
;
; Postconditions: 
;   - EAX EBX ECX EDX EDI are modified but restored before proecudure ends.
; 
; Receives: 
;	- numArray at stack address [EBP + 32]
;	- stringArray at stack address [EBP + 28]
;
; Returns: 
;	- numArray at stack address [EBP + 32]
;---------------------------------------------------------------------------
WriteVal PROC USES EAX EBX ECX EDX EDI
	; set the stack 
	PUSH			EBP
	MOV				EBP, ESP	

	; Set the divisor for string conversion to 1 billion (10^9)
	; Set the maximum number of loops required for conversion to 10
	MOV				EBX, 1000000000				
	MOV				ECX, 10						; Set loop count

	; set pointers 
	; branch to negate or nextVal 
	MOV				EDI, [EBP + 32]				; stringArray
	MOV				EDX, [EBP + 28]				; numArray
	CMP				EDX, 0						; Compare EDX with zero
	JL				_negate						
	JMP				_nextval					

; Handle Negative Number
_negate:
    NEG				 EDX						; Negate the value in EDX 
    MOV				 EAX, '-'					; Load ASCII code for '-' into EAX
    STOSB										; Store AL (EAX's lower byte) at [EDI], increment EDI	

_nextval:
	; set Eax and save the remainder
	MOV				EAX, EDX
	CDQ
	DIV				EBX									
	PUSH			EDX

	; compare with zero to see if you keep edx 
	CMP				EAX, 0
	JNE				_keepValue
	CMP				ECX, 1
	JE				_keepValue
	PUSH			EAX
	PUSH			EBX
	MOV				EAX, [EBP + 32]				; set stringArray 

_processChar:
    ; Compare if the previously written 
	; value was not zero
	; If previous character was zero
	; move to the next character
    MOV				BL, BYTE PTR [EAX]			; Load the byte from memory into BL
    CMP				BL, 31h						; Compare BL with ASCII value '1'
    JGE				_restoreAfterNonZero		; Jump if BL is greater than or equal to '1'
    INC				EAX							; Move to next char in stringArray
    CMP				EDI, EAX             
    JLE				_popRegisters				; branch if less than or equal 
    JMP				_processChar				

_restoreAfterNonZero:
	; Restore registers
	POP				EBX
	POP				EAX

_popRegisters:
	; Restore registers 
	POP				EBX
	POP				EAX
	JMP				_processLoop				; branch to processLoop 

_keepValue:
	ADD				EAX, 30h					; convert eax to 0
	STOSB

_processLoop:
	; prep for division: Copy EBX to EAX,
	; sign extend EAX to EDX:EAX
    MOV				EAX, EBX   
    CDQ                

    ; Divide EDX:EAX by 10: Quotient in EAX, 
	; Remainder in EDX
    MOV				EBX, 10						 ; Divisor
    DIV				EBX        

    ; Update EBX with the quotient (leading digit)
	; Store the quotient in EBX for further processing
    MOV				EBX, EAX   


	; restore register before branching 
    POP				EDX							 
    LOOP			_nextVal
    JMP				_exit						 ; Continue to the exit routine
		
_exit:
   ; end process 
    MOV				EAX, 0
    STOSB
    mDisplayString [EBP + 32]					 ; print stringArray 

    ; reset 
    MOV				ECX, MAXLENGTH
    MOV				EDI, [EBP + 32]
    MOV				EAX, 0
    REP				STOSB

	; final restore registers 
    pop				EBP
    ret				 8
WriteVal ENDP

;---------------------------------------------------------------------------
; Name: display
;  
;	display s a sub-procedure of Write-val. It prints the numbers from the array. 
;   It then displays the average and the sum of the those numbers. 
; 
; Preconditions: 
;	- stack should be properly set up before calling this procedure. 
; 
; Postconditions: None. 
; 
; Receives:
;	- space at stack address [EBP + 52]
;	- comma at stack address [EBP + 48]
;	- averageMsg at stack address [EBP + 44]
;	- totalNumsMsg at stack address [EBP + 40]
;	- totalSumMsg at stack address  [EBP + 36]
;	- numArray at stack address	 [EBP + 32]
;	- stringArray at stack address [EBP + 28]
;
; Returns: 
;	- averageMsg at stack address [EBP + 44]
;---------------------------------------------------------------------------
display PROC USES EAX EBX ECX EDX ESI
	; set the stack 
	push			EBP
	mov				EBP, ESP

	; set pointers
	; display total nums entered message 
	mov				ECX, 10
	mov				ESI, [EBP + 32]						; numArray
	mDisplayString	[EBP + 40]							; totalNumMsg

_showNums:
	; prep for push and set next index 
	LODSD
	PUSH			[EBP + 28]							; stringArray
	PUSH			EAX								
	CALL			WriteVal

	; if greater or equal branch totalSum
	CMP				ECX, 1									
	JE				_totalSum
	mDisplayString [EBP + 48]							; display comma 
	mDisplayString [EBP + 52]							; display space 
	LOOP			_showNums							; return to top of label 

_totalSum:
	; add to totalSum 
	MOV				ECX, 10
	MOV				ESI, [EBP + 32]
	MOV				EBX, 0		
	
_processSum:
	; store next val into eax for processing 
	LODSD
	ADD				EBX, EAX
	LOOP			_processSum
	JMP				_totalSumMsg

_totalSumMsg:
	; display totalSum message	
	; display actual sum value 
	mDisplayString	[EBP + 36]								; message 
	MOV				EAX, EBX
	PUSH			[EBP + 28]								
	PUSH			EAX										; sum val 
	CALL			WriteVal

_processAverage:
	; Use max input allowed to divide total sum value 
	MOV				EBX, 10
	CDQ
	IDIV			EBX		
	JMP				_averageMsg

_averageMsg:
	; display average message
	; display average 
	mDisplayString	[EBP + 44]							; averageMsg
	PUSH			[EBP + 28]						
	PUSH			EAX									; num to be displayed 
	CALL			WriteVal
	JMP				_exit

_exit:
	; restore registers 
	pop				EBP
	ret				16
display ENDP

END main