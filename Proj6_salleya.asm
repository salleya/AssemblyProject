TITLE String Primitives and Macros     (Proj6_salleya.asm)

; Author: Amy Salley
; Last Modified: 4 December 2020
; OSU email address: salleya@oregonstate.edu
; Course number/section:   CS271 Section 400
; Project Number:      6           Due Date: 6 December 2020
; Description: Invokes the mGetString macro to get user input in the form of a string
;	of digits.  Converts the string of ascii digits to its numeric value representation,
;	validating the user's input is a valid number.  Converts a numeric value to a string
;	of ascii digits, and invokes the mDisplayString macro to print the ascii representation
;	of the SDWORD value to the output.  Stores the numeric values in an array, displays the
;	integers, their sum, and their average as a string of characters.

INCLUDE Irvine32.inc

; Macro definitions

;-----------------------------------------------------------------------------------------------
; Name: mGetString
;
; Displays a prompt (input parameter, by reference).
; Stores the user's keyboard input into a memory location (output parameter, by reference).
; 
; Preconditions: none
;
; Receives:	userPrompt = byte array address
;			userInput = byte array address
;			inputLength = array length
;
; Returns: userInput = generated string address
;			inputLength = length of input string
;----------------------------------------------------------------------------------------------
mGetString	MACRO	userPrompt, userInput, inputLength
	PUSHAD							; Save registers

	MOV		EDX, userPrompt
	CALL	WriteString

	MOV		ECX, 33
	MOV		EDX, userInput
	CALL	ReadString
	MOV		inputLength, EAX		; Length of the input string

	POPAD							; Restore registers
	
ENDM

;-----------------------------------------------------------------------------------------------
; Name: mDisplayString
;
; Prints a string (input parameter, by reference). 
; 
; Preconditions: none
;
; Receives:	inputString = string address
;
; Returns: none
;----------------------------------------------------------------------------------------------
mDisplayString	MACRO	inputString
	PUSH	EDX						; Save registers

	MOV		EDX, inputString
	CALL	WriteString

	POP		EDX						; Restore registers
ENDM

; Constant definitions
INT_COUNT = 10						; Number of integer to be entered by the user

.data

; Variable definitions
intro1			BYTE		"Project 6 - String Primitives and Macros", 13,10,0
intro2			BYTE		"Written by: Amy Salley", 13,10,0
extraCredit		BYTE		"**EC: Use WriteVal to number each line of user input and display a running", 10
				BYTE		"subtotal of the user's valid numbers.**", 13,10,0
instruction		BYTE		"Please provide 10 signed decimal integers.", 13,10
				BYTE		"Each number needs to be small enough to fit inside a 32 bit register. After", 10
				BYTE		"you have finished inputting the raw numbers, I will display a list of integers,", 10
				BYTE		"their sum, and their average value.", 13,10,0
prompt			BYTE		". Please enter a signed number: ", 0
error			BYTE		"ERROR: You did not enter a signed number or your number was too big.", 13,10,0
answerTxt		BYTE		"You entered the following numbers: ", 13,10,0
comma			BYTE		", ", 0
sumTxt			BYTE		"The sum of these numbers is: ", 0
avgTxt			BYTE		"The rounded average is: ", 0
goodbye			BYTE		"Thank you for playing!", 13,10,0

enteredString	BYTE		33 Dup(?)			; User input string
stringLen		DWORD		?					; Length of user input string
validInt		SDWORD		?
integerArray	SDWORD		INT_COUNT Dup(?)	; Array of valid integers
numInt			DWORD		0					; Initialize number character counter
isNegative		DWORD		0
numString		BYTE		33 Dup(?)			; String of ascii digits to be displayed
total			SDWORD		?
average			SDWORD		?
inputCount		DWORD		1					; Initialize the input counter (for extra credit)

.code

;----------------------------------------------------------------------------------------
; Name: main
;
; Description:	Calls procedures to display the introduction, and read user input.
;	Calls a procedure to write the user values. Calculates and displays the sum of the
;	values.  Calculates and displays the rounded average of the values. Call a 
;	procedure to display a farewell message.
;----------------------------------------------------------------------------------------
main PROC

	; Display the introduction
	PUSH		OFFSET extraCredit
	PUSH		OFFSET instruction
	PUSH		OFFSET intro1
	PUSH		OFFSET intro2
	CALL		introduction
	CALL		CrLf

	;---------------------------------------------------------------------------------
	; Test program in main which uses the ReadVal and WriteVal procedures to:
	; 1. Get 10 valid integers from the user.
	; 2. Store these numeric values in an array.
	; 3. Display the integers, thier sum, and their average.
	;---------------------------------------------------------------------------------
	
	; Get input from the user 
	MOV			ECX, INT_COUNT				; Set counter to number of integer to get from the user
	MOV			EDI, OFFSET integerArray	; Array to store validated integers

_inputLoop:
	PUSH		OFFSET numString
	PUSH		inputCount
	PUSH		isNegative
	PUSH		OFFSET error
	PUSH		numInt
	PUSH		OFFSET validInt
	PUSH		OFFSET prompt
	PUSH		OFFSET enteredString
	PUSH		stringLen
	CALL		ReadVal

	MOV			EAX, validInt
	STOSD									; Store the validated integers in integerArray
	INC			inputCount					; Line counter for extra credit
	LOOP		_inputLoop
	CALL		CrLf

	; Display user input by looping through the array and calling WriteVal for each integer
	mDisplayString OFFSET answerTxt

	MOV			ESI, OFFSET integerArray	; Array of integers to be displayed
	MOV			ECX, INT_COUNT

_displayArray:
	PUSH		OFFSET numString
	PUSH		[ESI]						; One integer of the array
	CALL		WriteVal
	
	mDisplayString OFFSET comma				; Separate the values with a comma ', '
	ADD			ESI, TYPE integerArray
	LOOP		_displayArray
	CALL		CrLf

	; Calculate the sum of the integers in the integer array
	MOV			ESI, OFFSET integerArray
	MOV			ECX, INT_COUNT
	MOV			EAX, 0
_sumLoop:
	ADD			EAX, [ESI]
	ADD			ESI, TYPE integerArray		
	LOOP		_sumLoop
	MOV			total, EAX

	; Call WriteVal to display the sum
	mDisplayString OFFSET sumTxt
	PUSH		OFFSET numString
	PUSH		total
	CALL		WriteVal
	CALL		CrLf

	; Calculate the average
	MOV			EAX, total
	MOV			EBX, INT_COUNT
	CDQ
	IDIV		EBX
	MOV			average, EAX

	; Call WriteVal to display the average
	mDisplayString OFFSET avgTxt
	PUSH		OFFSET numString
	PUSH		average
	CALL		WriteVal
	CALL		CrLf
	CALL		CrLf

	; Display the farewell message
	PUSH		OFFSET goodbye
	CALL		farewell
	CALL		CrLf

	Invoke ExitProcess,0	; exit to operating system
main ENDP

; Additional procedures

;----------------------------------------------------------------------------------
; Name: introduction
;
; Description:	Uses the mDisplayString macro to display the program title, author,
;	extra credit completed, and introduce the program to the user. 
;
; Preconditions: extraCredit, BYTE array (reference, input)
;				instruction, BYTE array (reference, input)
;				intro1, BYTE array (reference, input)
;				intro2, BYTE array (reference, input)
;
; Postconditions: none
;
; Receives: [EBP+20] = address of extraCredit, byte array
;			[EBP+16] = address of instruction, byte array
;			[EBP+12] = address of intro1, byte array 
;			[EBP+8] = address of intro2, byte array
;
; Returns: none
;---------------------------------------------------------------------------------
introduction PROC

	PUSH		EBP
	MOV			EBP, ESP					; Base Pointer
	
	; Display the title and author
	mDisplayString [EBP+12]
	mDisplayString [EBP+8]
	CALL		CrLf

	; Display the extra credit completed
	mDisplayString [EBP+20]
	CALL		CrLf

	; Display the intructions
	mDisplayString [EBP+16]

	POP			EBP
	RET			16      

introduction ENDP

;----------------------------------------------------------------------------
; Name: ReadVal
;
; Description:	Invokes the mGetString macro to get user input in the form of
;	a string of digits.  Converts the string of ascii digits to its numeric
;	value representation, validating the user's input is a valid number.
;	Stores the value in a memory variable, validInt.
;
; Preconditions: numString, BYTE array (reference, input), to be passed to WriteVal
;				inputCount, initialized to 1 (value input), to be passed to WriteVal
;				isNegative, initialized to 0 (value input)
;				error, BYTE array (reference, input)
;				numInt, DWORD, conversion counter initialized to 0 (value input)
;				validInt, SDWORD (output parameter, by reference)
;				prompt, BYTE array (reference, input)
;				enteredString,  BYTE array (reference, input) to be entered by user
;				stringLen, length of string (value input) entered by user
;
; Postconditions: none
;
; Receives: [EBP+40] = address of numString
;			[EBP+36] = inputCount
;			[EBP+32] = isNegative
;			[EBP+28] = address of error, byte array
;			[EBP+24] = numInt, integer conversion counter
;			[EBP+20] = address of validInt, output SDWORD
;			[EBP+16] = address of prompt, byte array
;			[EBP+12] = address of enteredString, byte array entered by user
;			[EBP+8] = stringLen
;
; Returns: validInt
;----------------------------------------------------------------------------
ReadVal PROC
	PUSH		EBP
	MOV			EBP, ESP					; Base pointer
	PUSHAD									; Save registers

_getInput:
	; Extra Credit: Use WriteVal to number the line of user input
	PUSH		[EBP+40]					; String of ascii digits to be displayed
	PUSH		[EBP+36]					; Counter for line of user input
	CALL		WriteVal
	
	; Invoke the mGetString macro to get user input in the form of a string of digits.
	mGetString [EBP+16], [EBP+12], [EBP+8]

	MOV			ECX, [EBP+8]				; Set counter to string length
	MOV			ESI, [EBP+12]				; Move input string to ESI
	MOV			EDI, [EBP+20]				; Validated integer, output by reference
	CLD										; Clear direction flag

	;-------------------------------------------------------------------------------------
	; Put a byte to verify in AL. If the byte is the first character, The sign will be
	; checked ('-' or '+'). All characters are checked to be valid digits.
	;-------------------------------------------------------------------------------------
_computeInt:
	LODSB

	CMP			ECX, 12					
	JAE			_inputError					; Will definitely overflow if 12 or more characters are entered
	MOV			EBX, [EBP+8]
	CMP			EBX, ECX					; Check first character for sign
	JNE			_continueValidation					
	CMP			AL, 45						; Negative '-' first character
	JE			_negativeNumber
	CMP			AL, 43						; Positive '+' first character
	JE			_nextCharacter
	JMP			_continueValidation

_negativeNumber:
	MOV			EBX, 1
	MOV			[EBP+32], EBX
	JMP			_nextCharacter

_continueValidation:
	CMP			AL, 48
	JL			_inputError					; Character less than '0'
	CMP			AL, 57
	JG			_inputError					; Character greater than '9'

	;----------------------------------------------------------------------------------------
	; Convert the numerical character string to an integer. For each digit character, 
	; converts the character to the number it represents by subtracting 48, then adding
	; it to 10-times the current total. 
	;-------------------------------------------------------------------------------------
	SUB			AL, 48
	MOVSX		EAX, AL
	PUSH		EAX

	MOV			EAX, [EBP+24]				; Integer conversion counter
	MOV			EBX, 10
	IMUL		EBX

	POP			EBX
	JO			_inputError					; Check for overflow
	ADD			EAX, EBX
	MOV			[EBP+24], EAX
	JO			_inputError					; Check for overflow

_nextCharacter:
	LOOP		_computeInt

	MOV			EBX, 1
	CMP			[EBP+32], EBX				; Check for negative integer
	JNE			_notNegative
	NEG			EAX

_notNegative:
	JMP			_validated

_inputError:
	mDisplayString [EBP+28]					; Display error message
	MOV			EBX, 0						
	MOV			[EBP+24], EBX				; Reset integer conversion counter
	MOV			[EBP+32], EBX				; Reset isNegative
	JMP			_getInput

_validated:
	MOV			[EDI], EAX					; Store the validated number in validInt

	POPAD									; Restore registers
	POP			EBP
	RET			36

ReadVal	ENDP

;----------------------------------------------------------------------------
; Name: WriteVal
;
; Description:	Converts a numeric value to a string of ascii digits.
;	Invokes the mDisplayString macro to print the ascii representation of the
;	value to the output.
;
; Preconditions: numString, BYTE array to be displayed
;				numeric SDWORD value (input parameter, by value)
;
; Postconditions: none
;
; Receives: [EBP+12] = address of numString, byte array to be displayed
;			[EBP+8] = integer to be converted to a string of ascii digits
;
; Returns: numString
;----------------------------------------------------------------------------
WriteVal PROC
	PUSH		EBP
	MOV			EBP, ESP					; Base Pointer
	PUSHAD									; Save registers

	MOV			ESI, [EBP+8]				; Integer to display as a string
	MOV			EDI, [EBP+12]				; String of ascii digits to be calculated
	
	MOV			ECX, 0						; Count numeric characters in the integer

	MOV			EAX, ESI
	CMP			EAX, 0						; Check for a negative integer
	JGE			_repeat

	PUSH		EAX							; For negative integers add '-' to the string
	MOV			AL, 45	
	STOSB

	POP			EAX
	NEG			EAX							; Procede with the absolute value

	;-------------------------------------------------------------------------------
	; Converts the integer to a string of ascii digits by dividing by 10, adding
	; 48 to the remainder, and pushing the remainder to the stack.  Keeps track
	; of the number of values pushed to the stack.  Pops the remainders from the
	; stack and adds the values to the string array.
	;-------------------------------------------------------------------------------
_repeat:
	MOV			EBX, 10						; Divide by 10
	CDQ
	IDIV		EBX

	ADD			EDX, 48
	PUSH		EDX							; Push the remainder to the stack
	INC			ECX							; Increase the numeric character counter

	CMP			EAX, 0
	JE			_endOfInteger
	JMP			_repeat
	
_endOfInteger:

_reverseNumber:
	POP			EAX							; Pop the numeric characters (the remainders)
	STOSB									; Add character to the string array
	LOOP		_reverseNumber

	MOV			AL, 0						; Null terminate the string
	STOSB		

	; Invoke the mDisplayString macro to print the ascii representation of the
	; SDWORD value to the output.
	mDisplayString [EBP+12]

	POPAD									; Restore used registers
	POP			EBP
	RET			8

WriteVal ENDP

;----------------------------------------------------------------------------
; Name: farewell
;
; Description: Uses the mDisplayString macro to display a goodbye message 
;	for the user.
;
; Preconditions: goodbye, BYTE array (reference, input)
;
; Postconditions:	none
;
; Receives: [EBP+8] = address of goodbye, byte array 
;
; Returns: none
;----------------------------------------------------------------------------
farewell PROC

	PUSH		EBP
	MOV			EBP, ESP					; Base pointer

	mDisplayString [EBP+8]

	POP			EBP						
	RET			4      

farewell ENDP

END main
