;==================================================================================
;Copyright (C) 2016-2017 by Miodrag 'MITCH' Lalovic <mitch90ml@gmail.com>
;==================================================================================
;This software is released under Creative Commons (CC BY-NC 2.0) License;
;For legal stuff please see:
;https://creativecommons.org/licenses/by-nc/2.0/legalcode
;==================================================================================
;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;THE SOFTWARE.
;==================================================================================
;REGISTER ORIENTED SD CARD IN SPI MODE ROUTINES FOR Z80 CPU 
;THERE IS COUPLE OF SPOTS WHERE WE COULD SHAVE OFF FEW BYTES TO REDUCE CODE SIZE 
;HOWEVER CODE MIGHT BE LESS DESCRIPTIVE
;==================================================================================
;readSdBlock and writeSdBlock ROUTINES are not register safe
;use stack to preserve your registers
;==================================================================================

SD_IO_DATA		.EQU	$70
SD_IO_XFER		.EQU	$60
SD_CS_LO		.EQU	$50
SD_CS_HI		.EQU	$40

.ORG $0000

;==============================================================================
;SD CARD INIT
;==============================================================================
sdInit:

		CALL	sdUnknown
		OUT		(SD_CS_HI),A	;de-assert /SD_CS

		LD 		B,$0A			;80 cycles !!!!!! 
								;very important !!!!!!
sdInit1:
		CALL	sdXfer
		DJNZ	sdInit1			;run 80 cycles
		OUT		(SD_CS_LO),A	;assert /SD_CS

		LD		B,$FF			;safety timeout
								;so we do not spin forever

sdInit2:
		PUSH	BC
		CALL	zeroABCDE
		CALL	sdCmd
		CP		$01
		POP		BC				;restore BC in the case we are going to break

		JP		Z,sdInit3		;if 1 break

		DJNZ	sdInit2			;else loop
		LD		H,$01			;error code 1
		JP		sdInitERR		;timeout error

sdInit3:
		CALL	zeroABCDE
		LD		A,$08
		LD		D,$01
		LD		E,$AA
		CALL	sdCmd
		LD		B,$04

sdInit4:
		LD		A,$FF
		CALL	sdXfer
		DJNZ	sdInit4
		CP		$AA				;check for response
		LD		H,$02			;set err code if not $AA
		JP		NZ,sdInitERR	;break out

sdInit5:
		CALL	zeroABCDE
		LD		A,$37
		CALL	sdCmd
		CP		$05
		JR		Z,sdInitT1
		CP		$01
		JR		Z,sdInitT2

sdInitT1:
		LD		A,$01
		LD		(SD_Type),A		;card type 1
		JR		sdInit6

sdInitT2:
		LD		A,$02
		LD		(SD_Type),A		;card type 2		

sdInit6:
		CP		$02				;test for type 2
		JR		Z,sdInit7		;if type 2
		LD		B,$00			;else arg = $00000000 type 1 arg
		JR		Z,sdInit8

sdInit7:
		LD		B,$40			;arg = $40000000 type 2 arg

sdInit8:						;look here for code reduction
		XOR		A
		LD		C,A
		LD		D,A
		LD		E,A
		LD		H,$FF

sdInit9:
		LD		A,$29			;acmd = $29
		CALL	sdAppCmd
		CP		$00
		JR		Z,sdInit10		;if $00 break
		DEC		H
		JR		NZ,sdInit9

sdInit10:
		LD		A,(SD_Type)
		CP		$02
		JR		Z,sdInit11
		JP		sdInit16

sdInit11:

		CALL	zeroABCDE
		LD		A,$3A
		CALL	sdCmd

sdInit12:
		LD		A,$FF
		CALL	sdXfer
		CP		$C0
		JR		Z,sdInit13
		JR		sdInit14

sdInit13:
		LD		A,$03			;set
		LD		(SD_Type),A		;card type 3

sdInit14:
		LD		B,$03

sdInit15:
		LD		A,$FF			;discard rest of ocr
		CALL	sdXfer
		DJNZ	sdInit15

sdInit16:
		CALL	zeroABCDE		;set block size to $200
		LD		D,$02
		LD		A,$50
		CALL	sdCmd
		OUT		(SD_CS_HI),A	;de-assert /SD_CS
		XOR		A				;return no error
		RET


sdInitERR:
		OUT		(SD_CS_HI),A	;de-assert /SD_CS
		LD		A,H				;return error code
		RET

;==============================================================================
;SET SD CARD FLAG TO UNKNOWN
;==============================================================================
sdUnknown:
		PUSH	AF
		XOR		A
		LD		(SD_Type),A		;card type unknown
		POP		AF
		RET

;==============================================================================
;ZERO TO REGISTERS 
;==============================================================================
zeroABCDE:
		XOR		A		
		LD		B,A
		LD		C,A
		LD		D,A
		LD		E,A
		RET

;==============================================================================
;data in A, write A to port 0x70, read from port 0x60 (transfer),
;read from port 0x70, return data in A
;==============================================================================
sdXfer:
		OUT		(SD_IO_DATA),A
		IN		A,(SD_IO_XFER)
		IN		A,(SD_IO_DATA)
		RET

;==============================================================================
;Wait fo SD READY flag
;A contains input timeout parameter
;==============================================================================
waitNotBusy:

		PUSH	BC
		PUSH	DE
		PUSH	HL

		LD		B,A					;save timeout 
		LD		L,$FF
waitNotBusy1:
		LD		A,L
		CALL	sdXfer
		CP		L
		JR		Z,waitNotBusy3

		XOR		A
		LD		C,L					;load dummy counter

waitNotBusy2:
		DEC		C
		CP		C
		JR		NZ,waitNotBusy2		;dummy loop do nothing

		DJNZ	waitNotBusy1		;check for timeout
		XOR		A					;return 0, timeout
		JR		waitNotBusy4

waitNotBusy3:
		LD		A,$01				;return 1, no timeout

waitNotBusy4:
		POP		HL
		POP		DE
		POP		BC

		RET
		

;==============================================================================
;CMD in A, ARG in B,C,D,E ; B MS Byte ; E LS Byte
;L used for CRC 
;==============================================================================
sdCmd:

		PUSH	AF

		CP		$00				;is CMD $00
		JR		Z,sdCmdCRC95
		CP		$08				;is CMD $08
		JR		Z,sdCmdCRC87
		LD		L,$FF			;else CRC $FF
		JR		sdEndCRC		;dummy CRC

sdCmdCRC95:
		LD		L,$95
		JR		sdEndCRC
sdCmdCRC87:
		LD		L,$87
		JR		sdEndCRC



sdEndCRC:
		LD		A,$FF			;load timeout for waitnot busy

		CALL	waitNotBusy		;let's hope for the best

		POP		AF				;get CMD
		OR		$40				;set bit 7

		CALL	sdXfer			;send command
		LD		A,B				;load byte 4
		CALL	sdXfer			;send byte 4
		LD		A,C				;load byte 3
		CALL	sdXfer			;send byte 3
		LD		A,D				;load byte 2
		CALL	sdXfer			;send bhte 2
		LD		A,E				;load byte 1
		CALL	sdXfer			;send byte 1
		LD		A,L				;load CRC
		CALL	sdXfer			;send CRC

		LD		B,$FF			;load timeout loop counter

sdCmdloop:
		LD		A,$FF
		CALL	sdXfer
		CP		$FF
		JR		Z,sdCmdloop1	;if $80 break
		DJNZ	sdCmdloop		;else loop until timeout

sdCmdloop1:
		CALL	sdXfer
		RET
;==============================================================================
;APP CMD, ARG in LBA0-LBA3 CMD in A
;==============================================================================
sdAppCmd:		
;		EX		AF,AF'			;option to use shadow registers
;		EXX						;it saves some stack space

		PUSH	AF				;put comments on these out if you are using
		PUSH	BC				;shadow registers
		PUSH	DE				;
		PUSH	HL				;

		CALL	zeroABCDE
		LD		A,$37
		CALL	sdCmd

;		EX		AF,AF'
;		EXX	

		POP		HL
		POP		DE
		POP		BC
		POP		AF
		CALL	sdCmd
		RET

;==============================================================================
;READ BLLOCK, ARG in LBA0-LBA3
;==============================================================================
readSdBlock:
		OUT		(SD_CS_LO),A		;assert /SD_CS
		LD		A,(lba3)
		LD		B,A
		LD		A,(lba2)
		LD		C,A
		LD		A,(lba1)
		LD		D,A
		LD		A,(lba0)
		LD		E,A
		LD		A,$11
		CALL	sdCmd

		LD		B,$FF
readSdBlock1:
		LD		A,$FF
		CALL	sdXfer
		CP		$FE
		JR		Z,readSdBlock2
		DJNZ	readSdBlock1
		LD		A,$22				;set err code $22 was for debugging
		JR		readSdBlockERR		;set appropriate code if you need 

readSdBlock2:
		LD		B,$00				;counter value to $00 since DJNZ
									;first decrements and then compares
		LD		C,$FF
		LD 		HL,(dmaAddr)		;load pointer to data buffer

readSdBlock3:
		LD		A,C
		CALL	sdXfer				;read 2 bytes, so
		LD 		(HL),A				;we can get away with
		INC		HL					;8 bit counter
		LD		A,C					;
		CALL	sdXfer				;
		LD 		(HL),A				;
		INC		HL					;
		DJNZ	readSdBlock3		;rinse and repeat

		LD		A,C					;ignore the rest
		CALL	sdXfer				;
		LD		A,C					;could be CRC
		CALL	sdXfer				;

		OUT		(SD_CS_HI),A		;de-assert /SD_CS
		XOR		A					;return $00
		RET

readSdBlockERR:
		OUT		(SD_CS_HI),A		;de-assert /SD_CS
		RET							;Huston, we have problem....

;==============================================================================
;WRITESDDATA
;used by writeSdBlock
;==============================================================================
writeSdData:
		LD 		HL,(dmaAddr)	;buffer pointer
		XOR		A		
		LD		B,A				;set counter

		LD		A,$FE			;set token
		CALL	sdXfer

writeSdData1:
		LD		A,(HL)
		CALL	sdXfer
		INC		HL
		LD		A,(HL)
		CALL	sdXfer
		INC		HL
		DJNZ	writeSdData1

		LD		B,$03
		LD		C,$FF		
writeSdData2:
		LD		A,C				;ignore CRC
		CALL	sdXfer			;get response
		DJNZ	writeSdData2

		AND		$1F
		CP		$05
		JR		NZ,writeSdDataERR
		LD		A,C				;return not zero
		RET


writeSdDataERR:
		OUT		(SD_CS_HI),A	;de-assert /SD_CS
		XOR		A				;set err code
		RET

;==============================================================================
;CMD, ARG in LBA0-LBA3
;==============================================================================
writeSdBlock:
		OUT		(SD_CS_LO),A;assert /SD_CS
		LD		A,(lba3)
		LD		B,A
		LD		A,(lba2)
		LD		C,A
		LD		A,(lba1)
		LD		D,A
		LD		A,(lba0)
		LD		E,A
		LD		A,$18
		CALL	sdCmd
		CP		$00		
		JP		NZ,writeSdBlockERR

		CALL	writeSdData
		CP		$00
		JP		Z,writeSdBlockERR

		CALL	zeroABCDE
		LD		A,$0D
		CALL	sdCmd
		LD		C,A			;save response
		LD		A,$FF
		CALL	sdXfer
		OR		C
		CP		$00
		LD		A,$33		;set err code
		JR		NZ,writeSdBlockERR
		OUT		(SD_CS_HI),A;de-assert /SD_CS
		XOR		A		;no error
		RET

writeSdBlockERR:
		OUT		(SD_CS_HI),A;de-assert /SD_CS
		LD		A,$01
		RET



;==============================================================================
;DEPENDING ON THE SD TYPE WE HAVE TO SHIFT LBA3-LBA0 9 BITS LEFT (type 1 and 2)
;THIS SUBROUTINE SHOULD BE CALLED BEFORE readSdBlock or writeSdBlock ROUTINES
;==============================================================================

shiftLBA9Bits:
		LD		A,(SD_Type)	
		CP		$03
		JP		Z,shiftLBA9Bits1	;JR should work also

		LD		A,(blba1)
		LD		B,A
		LD		A,(blba2)
		RL		B
		RLA
		LD		(lba3),A

		LD		A,(blba0)
		LD		B,A
		LD		A,(blba1)
		RL		B
		RLA
		LD		(lba2),A

		OR		A
		LD		A,(blba0)
		RLA
		LD		(lba1),A

		XOR		A
		LD		(lba0),A

		JR		shiftLBA9BitsEnd

shiftLBA9Bits1					;transfer buffered lba to lba
								;no shifting for type 3 SD

		LD		A,(blba0)
		LD		(lba0),A
		LD		A,(blba1)
		LD		(lba1),A
		LD		A,(blba2)
		LD		(lba2),A
		LD		A,(blba3)
		LD		(lba3),A
		
shiftLBA9BitsEnd:
		RET	

;==============================================================================
;VARIABLES
;==============================================================================
dmaAddr		.BLOCK	2		;2 BYTE POINTER for SD I/O (address of the buffer)
SD_Type		.DB	00h
lba0		.DB	00h
lba1		.DB	00h
lba2		.DB	00h
lba3		.DB	00h
blba0		.DB	00h			;use these for your I/O parameters
blba1		.DB	00h			;reason for these is so we could
blba2		.DB	00h			;re-use Grant's code without much
blba3		.DB	00h			;modifications, these are not mandatory
							;it adds some safety against possible
							;errors and it helps with debug process
FINIS		.END


