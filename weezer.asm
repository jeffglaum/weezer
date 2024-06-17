;;
;;  Motorola MC68HC11 SBC Monitor Program ("Weezer")
;;  Jeff Glaum
;;
;;  Revision	Date		Comment
;;  ------------------------------------------------------------------
;;  0.4		2012-Mar-10	Clean-up & bug fixes
;;  0.5		2012-Mar-18	Added suppport for dump, enter, regs
;;                               (tested on MC68HC11 simulator)
;;
;;
;;   Memory Map:
;;
;;	$0000	+---------------+
;;              |  256 bytes	|
;;              |  on-chip RAM	| <- Stack (lower) & Monitor variables (upper)
;;	$00FF	+---------------+
;;              |               |
;;              |    Unused     |
;;	$1000	+---------------+
;;              |   96 bytes	|
;;              |   registers	|
;;	$105F	+---------------+
;;              |               |
;;              |               |
;;              |               |
;;              |    Unused     |
;;              |               |
;;      $C000   +---------------+
;;              | LCD registers |
;;              +---------------+
;;              |               |
;;              |               |
;;	$D000	+---------------+
;;              |               |
;;              |  8192 bytes	|
;;              | off-chip RAM	| <- Free program memory
;;              |               |
;;              |               |
;;	$F000	+---------------+
;;              |               |
;;              |  4096 bytes	|
;;              | off-chip ROM	| <- Boot code and interrupt vectors
;;              |               |
;;              |               |
;;	$FFFF	+---------------+
;;


;; Addresses
;;
STACK_BASE  EQU     $80     ; Stack pointer base
PROGRAM_RAM EQU     $D000   ; Start of program RAM (free memory)

;; Serial Registers
;;
BAUD        EQU     $102B   ; SCI BAUD register
SCCR1       EQU     $102C   ; SCI control register #1
SCCR2       EQU     $102D   ; SCI control register #2
SCSR        EQU     $102E   ; SCI control status register
SCDR        EQU     $102F   ; SCI data register

;; SCI Constants
;;
BAUD_RATE   EQU     $30     ; 9600 baud
SCCR2_VAL   EQU     $0C     ; SCCR2 value
TDRE        EQU     $80     ; Transmit data register empty

;; ASCII Constants
;;
SPACE       EQU     32      ; Space
CR          EQU     13      ; CR
LF          EQU     10      ; LF
DEL         EQU     127     ; DEL
BS          EQU     8       ; BS
BELL        EQU     7       ; BELL

;; LCD Registers
;;
DISPC       EQU     $C000   ; LCD control register
DISPD       EQU     $C001   ; LCD data register

;; LCD Constants
;;
LCD_CONFIG  EQU     $38     ; Interface len=8, 2 lines, 5x7 font
LCD_DISPON  EQU     $C      ; Display on, cursor off, cursor blink off
LCD_HOME    EQU     $1      ; Clear all display and return cursor home
LCD_CRSCFG  EQU     $6      ; Increment cursor and don't shift display
LCD_RAMADDR EQU     $80     ; Set DDRAM address to position 0.
LCD_WAITFLG EQU     $80     ; LCD command complete wait flag

;; S-Record Constants
;;
SRECHDR_S1  EQU     1       ; S1 (data) record
SRECHDR_S9  EQU     2       ; S9 (jump address) record


;; Memory Variables 
;;
            ORG     $90
INBUFF      RMB     $50     ; Input buffer
INBUFFLEN   RMB     $1      ; Input buffer length
TEMP        RMB     $1      ; Temporary variable
SRECADDR    RMB     $2      ; Current line's S-Record address


;; Main routine
;;
            ORG     $F000           ; Locate program at $F000
_WEEZER     LDS     #STACK_BASE     ; Locate stack pointer
            JSR     _SERINIT        ; Initialize SCI port
            JSR     _LCDINIT        ; Initialize LCD display
            LDX     #WELCOMESTR     ; Display welcome string
            JSR     _SERWRITES      ;  * serial port
            JSR     _LCDWRITES      ;  * LCD
            LDX     #COPYRGHTSTR    ; Display copyright string
            JSR     _SERWRITES      ;  * serial port
            JSR     _SERCLEAR       ; Clear SCI input register
            CLI                     ; Enable interrupts
_PROMPT     LDX     #PROMPTSTR      ; Display input prompt
            JSR     _SERWRITES      ;  * serial port
            JSR     _CLEARINPUT     ; Clear input string buffer and length
            JSR     _SERREADS       ; Get user's input
            JSR     _PARSECMDS      ; Determine command specified
            CMPA    #0              ; If command not found - don't execute
            BEQ     _PROMPT         ;
            JSR     _EXECUTECMD     ; Execute command
            BRA     _PROMPT         ;


;; Read SCI receive buffer until empty 
;;
_SERCLEAR   PSHA
_SERCLR1    LDAA    SCSR
            ANDA    #$20
            BEQ     _SERCLRDONE
            LDAA    SCDR
            BRA     _SERCLR1
_SERCLRDONE PULA
            RTS
            
            
;; Clear command input string 
;;
_CLEARINPUT PSHA                    ; Save A
            LDAA    #0              ;
            STAA    INBUFF          ; Clear (terminate) input buffer
            STAA    INBUFFLEN       ; Zero input buffer length
            PULA                    ; Restore A
            RTS


;; Initialize serial port 
;;
_SERINIT    PSHA                    ; Save A
            LDAA    #BAUD_RATE      ; Set baud rate to 9600 (8 MHz xtal)
            STAA    BAUD            ;
            LDAA    #0              ; Clear SCCR1 (1 start, 8 data, 1 stop)
            STAA    SCCR1           ;
            LDAA    #SCCR2_VAL      ; Enable TxD and RxD
            STAA    SCCR2           ;
            PULA                    ; Restore A
            RTS                     ; Done


;; Write serial port string                
;; IN: X = NULL-terminated string to print 
;;
_SERWRITES  PSHX                    ; Save X
            PSHB                    ; Save B
_SERSTRLOOP LDAB    0,X             ; Get next character to be transmitted
            BEQ     _SERSTRDONE     ; If NULL terminator, done
            JSR     _SERWRITEC      ; Write character to SCI
            INX                     ; Next character
            BRA     _SERSTRLOOP     ; Continue
_SERSTRDONE PULB                    ; Restore B
            PULX                    ; Restore X
            RTS                     ; Done


;; Write serial port character 
;; IN: B = character to print  
;;
_SERWRITEC  PSHA                    ; Save A
_SERCHRWAIT LDAA    SCSR            ; Transmit register empty?
            ANDA    #TDRE           ;
            BEQ     _SERCHRWAIT     ;
            STAB    SCDR            ; Transmit contents of B
            PULA                    ; Restore A
            RTS                     ; Done


;; Read string from serial port                                           
;; OUT: INBUFF & INBUFFLEN contain the received data (CR or LF end input) 
;;
_SERREADS   PSHA                    ; Save A
            PSHB                    ; Save B
            PSHX                    ; Save X
_SERREAD1   JSR     _SERREADC       ; Read character from SCI
            CMPA    #BS             ; Backspace - delete last character
            BEQ     _SERBKSPACE     ;
            CMPA    #CR             ; Carriage return - NULL terminate string
            BNE     _SERREAD15      ;
            LDAA    #0              ; NULL-terminate
            BRA     _SERREAD2       ;
_SERREAD15  CMPA    #LF             ; Line feed - NULL terminate string
            BNE     _SERREAD2       ;
            LDAA    #0              ; NULL-terminate
_SERREAD2   LDX     #INBUFF         ; Get input buffer
            LDAB    INBUFFLEN       ; Get input buffer length
            ABX                     ; Offset past current data
            STAA    0,X             ; Store character in buffer
            INCB                    ; Increment string length
            STAB    INBUFFLEN       ; Store string length
            CMPA    #0              ; If NULL (end of string), execute command
            BNE     _SERREAD1       ; If NOT CR - continue
            BRA     _SERRDDONE      ; CR - Done
_SERBKSPACE LDAA    INBUFFLEN       ; Get character string length
            BEQ     _SERREAD1       ; If string length is zero - done
            DECA                    ; Decrement string length
            STAA    INBUFFLEN       ; Store new string length
            BRA     _SERREAD1       ; Continue
_SERRDDONE  PULX                    ; Restore X
            PULB                    ; Restore B
            PULA                    ; Restore A
            RTS                     ; Done


;; Read raw input from the serial port 
;; IN: B = number of chars to read
;;
_SERREADRAW PSHA                    ; Save A
            PSHB                    ; Save B
            PSHX                    ; Save X
            LDX     #INBUFF         ; Get input buffer
_SERREADR1  CMPB    #0              ; No more characters to read?
            BEQ     _SERREADDN      ;
            JSR     _SERREADCx      ; Read byte from SCI (no echo)
            CMPA    #LF             ; Line-feed return - ignore
            BEQ     _SERREADR1      ;
            CMPA    #CR             ; Carriage return - ignore
            BEQ     _SERREADR1      ;
            STAA    0,X             ; Store byte in buffer
            INX                     ; Increment to next character position
            DECB                    ; Decrement byte count
            BRA     _SERREADR1      ; Continue reading...
_SERREADDN  LDAA    #0              ; Terminate the string
            STAA    0,X             ;
            PULX                    ; Restore X
            PULB                    ; Restore B
            PULA                    ; Restore A
            RTS                     ; Done


;; Read character from serial port  
;; OUT: A = character read from SCI
;;
_SERREADC  PSHB                    ; Save B
_SERREADC1 LDAA    SCSR            ; Check for received character
           ANDA    #$20            ;
           BEQ     _SERREADC1      ; No characters - wait
           LDAA    SCDR            ;
           PSHA                    ; Copy A -> B
           PULB                    ;
           JSR     _SERWRITEC      ; Echo character to remote console
           PULB                    ; Restore B
           RTS                     ; Done

;; Read character from serial port (no echo)
;; OUT: A = character read from SCI        
;;
_SERREADCx LDAA    SCSR            ; Check for received character
           ANDA    #$20            ;
           BEQ     _SERREADCx      ; No characters - wait
           LDAA    SCDR            ;
           RTS                     ; Done


;; Determine command specified by user    
;; INTX X = NULL-terminated command string
;; OUT: A = command index number          
;;
_PARSECMDS  PSHB                    ; Save B
            PSHX                    ; Save X
            PSHY                    ; Save Y
            JSR     _SERPRTCR       ; Start a newline
            LDAB    #1              ; Initialize command array counter
            LDX     #INBUFF         ; Load pointer to input buffer string
            LDAA    0,X             ;
            CMPA    #0              ; If it's an empty string, exit
            BEQ     _PARSEDONE      ;
            LDY     #COMMANDS       ; Load pointer to command array string
_EXECLOOP   JSR     _STRCMP         ; Compare command strings
            CMPA    #0              ;
            BEQ     _FOUNDCMD       ; Found command (command number in B)
            INCB                    ; Increment command array counter
_NEXTCMD    INY                     ;
            LDAA    0,Y             ;
            BNE     _NEXTCMD        ; Loop until next command
            INY                     ;
            LDAA    0,Y             ;
            BEQ     _BADCMD         ; End of command array and not found - done
            BRA     _EXECLOOP       ;
_FOUNDCMD   PSHB                    ; Copy B -> A
            PULA                    ;
            BRA     _PARSEDONE      ; 
_BADCMD     LDX     #UNKWNCMDSTR    ; Load invalid command string text
            JSR     _SERWRITES      ; Write message to serial port
            LDAA    #0              ; Bad command id
_PARSEDONE  PULY                    ; Restore Y
            PULX                    ; Restore X
            PULB                    ; Restore B
            RTS                     ; Done - return to input handler loop


;; Dispatch to chosen command
;; IN: A = Command id        
;;
_EXECUTECMD PSHA                    ; Save A
            PSHB                    ; Save B
            PSHX                    ; Save X
            CMPA    #1              ; #1: Dump
            BEQ     _CMD1           ;
            CMPA    #2              ; #2: Enter
            BEQ     _CMD2           ;
            CMPA    #3              ; #3: Go
            BEQ     _CMD3           ;
            CMPA    #4              ; #4: Srec
            BEQ     _CMD4           ;
            CMPA    #5              ; #5: Reset
            BEQ     _CMD5           ;
            CMPA    #6              ; #6: API  routines list
            BEQ     _CMD6           ;
            CMPA    #7              ; #7: Registers
            BEQ     _CMD7           ;
            CMPA    #8              ; #7: Help
            BEQ     _CMD8           ;
            CMPA    #9              ; #8: ?
            BEQ     _CMD9           ;
            LDX     #UNKWNCMDSTR    ; Invalid command
            JSR     _SERWRITES      ;
            BRA     _CMDDISPDN      ;
_CMD1       JSR     _DUMPMEM        ; Dump memory contents
            BRA     _CMDDISPDN      ;
_CMD2       JSR     _ENTERMEM       ; Enter memory contents
            BRA     _CMDDISPDN      ;
_CMD3       JSR     _JUMPTO         ; Go
            BRA     _CMDDISPDN      ;
_CMD4       JSR     _SRECDOWNLD     ; Download & execute S-Record code
            BRA     _CMDDISPDN      ;
_CMD5       JSR     _WEEZER         ; Reset
            BRA     _CMDDISPDN      ;
_CMD6       LDX     #APIRTNSTXT     ; Monitor routines list
            JSR     _SERWRITES      ;
            BRA     _CMDDISPDN      ;
_CMD7       JSR     _DUMPREGS       ; Registers
            BRA     _CMDDISPDN      ;
_CMD8       NOP                     ;
_CMD9       LDX     #HELPTEXT       ; ?/Help
            JSR     _SERWRITES      ;
_CMDDISPDN  PULX                    ; Restore X
            PULB                    ; Restore B
            PULA                    ; Restore A
            RTS                     ; Done


;; S-Record download & execute 
;;
_SRECDOWNLD PSHA                    ; Save A
            PSHB                    ; Save B
            PSHX                    ; Save X
            PSHY                    ; Save Y
            LDX     #RECEIVESTR     ; Load receive string for display
            JSR     _SERWRITES      ; Write string to serial port
_SRECGETLN  LDAB    #2              ; Read the first two characters (Sn header)
            JSR     _SERREADRAW     ;
            LDX     #INBUFF         ; Load pointer to S-Record line
            LDY     #SREC_S1HDR     ; Load pointer to S1 header string
            JSR     _STRCMP         ; Compare command strings
            CMPA    #0              ;
            BEQ     _FOUNDS1HDR     ; Found S1 header
            LDY     #SREC_S9HDR     ; Load pointer to S9 header string
            JSR     _STRCMP         ; Compare command strings
            CMPA    #0              ;
            BEQ     _FOUNDS9HDR     ; Found S9 header
            LDX     #SRECERRSTR     ; ERROR: unrecognized S-Record header
            JSR     _LCDCLEAR       ; Clear the LCD display
            JSR     _LCDWRITES      ; Write string to serial port
            JSR     _SERWRITES      ; Write string to serial port
            BRA     _SRECDONE       ;
_FOUNDS1HDR LDAA    #SRECHDR_S1     ; Keep track of the current line type (S1)
            PSHA                    ;
            BRA     _SRECCOMMON     ;
_FOUNDS9HDR LDAA    #SRECHDR_S9     ; Keep track of the current line type (S9)
            PSHA                    ;
_SRECCOMMON LDAB    #2              ; Get the number of remaining bytes in the line
            JSR     _SERREADRAW     ;
            JSR     _ASCII2HEX      ; Convert ASCII byte count to hex (result in A)
            PSHA                    ; Save number of bytes
_SRECADDR   LDAB    #4              ; Get the address
            JSR     _SERREADRAW     ;
            JSR     _ASCII2HEX      ; Convert ASCII addrH to hex (result in A)
            TAB                     ; A (addrH) -> B
            INX                     ; Index to the address Low field
            INX                     ;
            JSR     _ASCII2HEX      ; Convert ASCII addrL to hex (result in A)
            PSHA                    ; Push AddrL onto stack
            PSHB                    ; Push AddrH onto stack
            PULY                    ; Pull AddrH & AddrL (16-bits) into Y
            STY     SRECADDR        ; Store the address for later
            PULB                    ; Decrement the byte count by two bytes (address)
            DECB                    ;
            DECB                    ;
            DECB                    ; Decrement by one byte (checksum) to get number of data bytes
            CMPB    #0              ; No data bytes (ex S9 record), skip to checksum
            BEQ     _SRECCHK        ;
            PSHB                    ; Save byte count
            LSLB                    ; x2 since each byte is two characters
            JSR     _SERREADRAW     ; Read the S-Record data bytes
            PULB                    ; Retrieve byte count
            LDX     #INBUFF         ; Load pointer to start of S-Record line
_STORESREC  JSR     _ASCII2HEX      ; Convert ASCII byte to hex (result in A)
            STAA    0,Y             ; Store the converted byte value
            INX                     ;
            INX                     ;
            INY                     ;
            DECB                    ; Decrement the byte count
            BNE     _STORESREC      ; Continue until we're finished
_SRECCHK    LDAB    #2              ; Read the S-Record checksum byte
            JSR     _SERREADRAW     ; 
;;
;; NOTE: we're ignoring the checksum - may want to fix this in the future.
;;
            PULA                    ; Recall the S-Record type from the stack
            CMPA    #SRECHDR_S1     ; Is it an S1 header?  If so, there's more to read...
            BEQ     _SRECGETLN      ; Found S1 header, continue reading.
_SRECLASTLN LDX     #JUMPINGSTR     ; Load jumping string for display
            JSR     _SERWRITES      ; Write string to serial port
            LDY     SRECADDR        ; Recall the last address (should be S9 jump address)
            JSR     0,Y             ; Jump to the address in Y
_SRECDONE   PULY                    ; Restore Y
            PULX                    ; Restore X
            PULB                    ; Restore B
            PULA                    ; Restore A
            RTS                     ; Done

;; Enter memory            
;;
_ENTERMEM   PSHA                    ; Save A
            PSHB                    ; Save B
            PSHX                    ; Save X
            PSHY                    ; Save Y
            LDX     #GETADDRSTR     ; Prompt for address
            JSR     _SERWRITES      ;
            JSR     _CLEARINPUT     ;
            JSR     _SERREADS       ;
            LDX     #INBUFF         ; Get a pointer to the input buffer
            JSR     _ASCII2HEX      ; Convert ASCII addrH to hex (result in A)
            TAB                     ; A (addrH) -> B
            INX                     ; Index to the address Low field
            INX                     ;
            JSR     _ASCII2HEX      ; Convert ASCII addrL to hex (result in A)
            PSHA                    ; Push AddrL onto stack
            PSHB                    ; Push AddrH onto stack
            PULY                    ; Pull AddrH & AddrL (16-bits) into Y
            LDX     #ENTERSTR       ; Display enter data string
            JSR     _SERWRITES      ;
_ENTERLOOP  JSR     _CLEARINPUT     ; Clear input buffer and length
            JSR     _SERREADS       ; Read serial string (i.e. byte text)
            JSR     _SERPRTCR       ; Print CR to SCI
            LDX     #INBUFF         ; Check for 0 (indicating done)
            LDAA    0,X             ;
            BEQ     _ENTERDONE      ;
            JSR     _ASCII2HEX      ; Convert ASCII byte to hex
            STAA    0,Y             ; Store data byte
            INY                     ;
            BRA     _ENTERLOOP      ; Continue with input stream
_ENTERDONE  PULY                    ; Restore Y
            PULX                    ; Restore X
            PULB                    ; Restore B
            PULA                    ; Restore A
            RTS                     ; Done


;; Get input buffer address 
;;
_GETINBADDR LDX     #INBUFF
            RTS


;; Get input buffer address 
;;
_GETINBLEN  LDX     #INBUFFLEN
            RTS


;; Convert text to upper case   
;; IN: A = character to convert
;;
_TOUPPER    CMPA    #'a'            ; If ASCII digit isn't a character, skip
            BLO     _UPCASEDONE     ;
            CMPA    #'z'            ;
            BHI     _UPCASEDONE     ;
            SUBA    #$20            ; Convert lower case to upper case
_UPCASEDONE RTS                     ; Done


;; Convert text to binary               
;; IN:  LDX = input string (two digits)
;; OUT: A   = converted byte value    
;;
_ASCII2HEX  PSHX                    ; Save X
            LDAA    0,X             ;
            JSR     _TOUPPER        ; If character, convert to uppercase
            CMPA    #$41            ; Is digit a character?
            BLO     _ISNUMBER       ; No - assume number
_ISALPHA    SUBA    #$37            ; Convert character to hex value
            BRA     _ASCII1         ;
_ISNUMBER   SUBA    #$30            ; Convert number to hex value
_ASCII1     ANDA    #$0F            ; Mask upper nibble bits
            LSLA                    ; First digit is upper nibble - shift
            LSLA                    ;
            LSLA                    ;
            LSLA                    ;
            STAA    TEMP            ; Save number in temporary variable
_NEXTDIGIT  INX                     ; Move to second digit
            LDAA    0,X             ;
            JSR     _TOUPPER        ; If character, convert to uppercase
            CMPA    #$41            ; Is digit a character?
            BLO     _ISNUMBER2      ; No - assume number
_ISALPHA2   SUBA    #$37            ; Convert character to hex value
            BRA     _ASCII2         ;
_ISNUMBER2  SUBA    #$30            ; Convert number to hex value
_ASCII2     ANDA    #$0F            ; Mask upper nibble bits
            ORAA    TEMP            ; Or in upper nibble bits
            PULX                    ; Restore X
            RTS                     ; Done


;; Jump to specified address (prompted) 
;;
_JUMPTO     PSHA                    ; Save A
            PSHB                    ; Save B
            PSHX                    ; Save X
            LDX     #GETADDRSTR     ; Prompt for address
            JSR     _SERWRITES      ;
            JSR     _CLEARINPUT     ;
            JSR     _SERREADS       ;
            LDX     #INBUFF         ; Get a pointer to the input buffer
            JSR     _ASCII2HEX      ; Convert ASCII addrH to hex (result in A)
            TAB                     ; A (addrH) -> B
            INX                     ; Index to the address Low field
            INX                     ;
            JSR     _ASCII2HEX      ; Convert ASCII addrL to hex (result in A)
            PSHA                    ; Push AddrL onto stack
            PSHB                    ; Push AddrH onto stack
            PULY                    ; Pull AddrH & AddrL (16-bits) into Y
            PULX                    ; Restore X
            PULB                    ; Restore B
            PULA                    ; Restore A
            JSR     0,Y             ; Jump to address stored in Y
            RTS


;; Dump registers 
;;
;;
_DUMPREGS   PSHA                    ; Save A
            PSHB                    ; Save B
            PSHX                    ; Save X
            PSHY                    ; Save Y
            PSHX                    ;
            LDX     #DUMPREGSSTR    ; Dump registers title
            JSR     _SERWRITES      ;
            LDX     #REGLABELA      ; Register A label
            JSR     _SERWRITES      ;
            JSR     _SERPRTSPC      ; 
            PSHB                    ;
            TAB                     ;
            JSR     _SERPRTBYTE     ; A value
            JSR     _SERPRTCR       ; 
            LDX     #REGLABELB      ; Register B label
            JSR     _SERWRITES      ;
            JSR     _SERPRTSPC      ; 
            PULB                    ;
            JSR     _SERPRTBYTE     ; B value
            JSR     _SERPRTCR       ; 
            LDX     #REGLABELIX     ; Register IX label
            JSR     _SERWRITES      ;
            PULA                    ; Pull top of 16-bit register X into A
            PULB                    ; Pull bottom of 16-bit register X into B
            JSR     _SERPRTWORD     ; IX value
            JSR     _SERPRTCR       ; 
            PSHY                    ;
            LDX     #REGLABELIY     ; Register IY label
            JSR     _SERWRITES      ;
            PULA                    ; Pull top of 16-bit register Y into A
            PULB                    ; Pull bottom of 16-bit register Y into B
            JSR     _SERPRTWORD     ; IY value
            JSR     _SERPRTCR       ; 
            LDX     #REGLABELSP     ; Register SP label
            JSR     _SERWRITES      ;
            TSX                     ; Move SP into IX
            XGDX                    ; Exchange D and IX
            JSR     _SERPRTWORD     ; SP value
            JSR     _SERPRTCR       ; 
            XGDX                    ; Exchange D and IX back again
            LDX     #REGLABELCC     ; Register CC label
            JSR     _SERWRITES      ;
            TPA                     ; Move CC into A
            TAB                     ; Move A into B
            JSR     _SERPRTBYTE     ; B value
            JSR     _SERPRTCR       ; 
            JSR     _SERPRTCR       ; 
            PULY                    ;
            PULX                    ;
            PULB                    ;
            PULA                    ;
            RTS                     ;


;; Dump memory contents    
;;
_DUMPMEM    PSHA                    ; Save A
            PSHB                    ; Save B
            PSHX                    ; Save X
            LDX     #GETADDRSTR     ; Prompt for address
            JSR     _SERWRITES      ;
            JSR     _CLEARINPUT     ;
            JSR     _SERREADS       ;
            LDX     #INBUFF         ; Get a pointer to the input buffer
            JSR     _ASCII2HEX      ; Convert ASCII addrH to hex (result in A)
            TAB                     ; A (addrH) -> B
            INX                     ; Index to the address Low field
            INX                     ;
            JSR     _ASCII2HEX      ; Convert ASCII addrL to hex (result in A)
            PSHA                    ; Push AddrL onto stack
            PSHB                    ; Push AddrH onto stack
            LDX     #DUMPSTR        ; Load dump text
            JSR     _SERWRITES      ;
            LDAA    #0              ; Zero counter
            STAA    TEMP
            PULX                    ; Pull AddrH & AddrL (16-bits) into X
_DUMPMEM1   LDAA    #0
_DUMPLOOP   LDAB    0,X             ; Load byte from receive buffer
            JSR     _SERPRTBYTE     ; Print byte in ascii to SCI
            JSR     _SERPRTSPC      ; Print space
            INX                     ; Next byte
            INCA                    ; Increment byte count
            CMPA    #$10
            BNE     _DUMPLOOP       ;
            JSR     _SERPRTCR       ; Print CR to SCI
            ADDA    TEMP
            STAA    TEMP
            CMPA    #$A0
            BLO     _DUMPMEM1
_DUMPDONE   PULX                    ; Restore X
            PULB                    ; Restore B
            PULA                    ; Restore A
            RTS                     ; Done


;; Convert from binary to ASCII and output
;; IN: B = byte to convert                
;;
_OUTLEFTH   LSRB                    ; Shift data to right
            LSRB
            LSRB
            LSRB
_OUTRIGHTH  ANDB    #$0F            ; Mask top half
            ADDB    #$30            ; Convert to ascii
            CMPB    #$39
            BLE     _OUTA           ; Jump if 0-9
            ADDB    #$07            ; Convert to hex A-F
_OUTA       JSR     _SERWRITEC      ; Output character
            RTS


;; Print byte to serial port 
;; IN: B = byte to be printed 
;;
_SERPRTBYTE PSHB
            PSHB
            JSR     _OUTLEFTH       ; Output left half
            PULB                    ; Retrieve copy
            JSR     _OUTRIGHTH      ; Output right half
            PULB
            RTS


;; Print word to serial port 
;; IN: D = word to be printed 
;;
_SERPRTWORD PSHA
            PSHB
            TAB
            JSR     _SERPRTBYTE     ; Print top byte
            PULB                 
            JSR     _SERPRTBYTE     ; Print low byte
            PULA
            RTS


;; Print LF & CR to serial port
;;
_SERPRTCR   PSHB
            LDAB    #CR
            JSR     _SERWRITEC
            LDAB    #LF
            JSR     _SERWRITEC
            PULB
            RTS


;; Print space to serial port 
;;
_SERPRTSPC  PSHB
            LDAB    #SPACE
            JSR     _SERWRITEC
            PULB
            RTS


;; String compare routine                            
;;                                                   
;; IN:  X = Pointer to string #1                     
;; IN:  Y = Pointer to string #2                     
;; OUT: A = compare status (0 = match, 1 = NO match)
;;
_STRCMP     PSHB                    ; Save B
            PSHX                    ; Save X
            PSHY                    ; Save Y
            LDAB    #0              ;
_SCMPLOOP   LDAA    0,X             ; Load character from first string
            CMPA    #0              ; NULL?
            BEQ     _SCMPLPDONE     ;
            CMPA    0,Y             ; Compare characters
            BNE     _SCMPLPDONE     ; No match
            INCB                    ;
            INX                     ; Next character
            INY                     ; 
            BRA     _SCMPLOOP       ; Check next character
_SCMPLPDONE LDAA    0,X             ; If end of string - consider it a match
            BEQ     _SCMPMATCH      ;
            LDAA    0,Y             ;
            BEQ     _SCMPMATCH      ;
_SCMPNOMAT  LDAA    #1              ; No match
            BRA     _SCMPDONE       ;
_SCMPMATCH  CMPB    #0              ;
            BEQ     _SCMPNOMAT      ;
            LDAA    #0              ;
_SCMPDONE   PULY                    ; Restore Y
            PULX                    ; Restore X
            PULB                    ; Restore X
            RTS                     ; Done


;; Clear LCD display 
;;
_LCDCLEAR   PSHA                    ; Save A
            LDAA    #$1             ; Clear all display and return cursor home
            STAA    DISPC           ;
            JSR     _LCDWAIT        ;
            PULA                    ; Restore A
            RTS                     ; Done


;; Simple ms delay routine                           
;;                                                   
;; IN: A = Number of ms to delay (1-255)            
;;                                                  
;; NOTE: *** Based on E=2 MHz (8 MHz xtal) ***     
;;
_DELAY_MS   PSHX
            LDX     #328
            BRA     _DELAY_MS_1
_DELAY_MS_0
            LDX     #332            ; 332*6 = 1992 cycles
_DELAY_MS_1
            DEX
            BNE     _DELAY_MS_1
            DECA
            BNE     _DELAY_MS_0
            PULX
            RTS


;; Initialize LCD display                            
;;                                                   
;; NOTE: From the Optrex LCD manual.                 
;;
_LCDINIT    PSHA                    ; Save A
            LDAA    #15             ; Wait 15ms to give Vcc time to stabilize
            JSR     _DELAY_MS       ;
            LDAA    #LCD_CONFIG     ; Interface len=8, 2 lines, 5x7 font
            STAA    DISPC           ;
            LDAA    #5              ; Wait 5ms
            JSR     _DELAY_MS       ;
            LDAA    #LCD_CONFIG     ; REPEAT: Interface len=8, 2 lines, 5x7 font
            STAA    DISPC           ;
            LDAA    #1              ; Wait 1ms
            JSR     _DELAY_MS       ;
            LDAA    #LCD_CONFIG     ; REPEAT: Interface len=8, 2 lines, 5x7 font
            STAA    DISPC           ;
            JSR     _LCDWAIT        ; Busy Flag can now be checked
            LDAA    #LCD_CONFIG     ; Interface len=8, 2 lines, 5x7 font
            STAA    DISPC           ;
            JSR     _LCDWAIT        ;
            LDAA    #LCD_DISPON     ; Display on, cursor off, cursor blink off
            STAA    DISPC           ;
            JSR     _LCDWAIT        ;
            LDAA    #LCD_HOME       ; Clear all display and return cursor home
            STAA    DISPC           ;
            LDAA    #2              ; Wait 2ms
            JSR     _DELAY_MS       ;
            JSR     _LCDWAIT        ;
            LDAA    #LCD_CRSCFG     ; Increment cursor and don't shift display
            STAA    DISPC           ;
            JSR     _LCDWAIT        ;
            LDAA    #LCD_RAMADDR    ; Set DDRAM address to position 0.
            STAA    DISPC           ;
            JSR     _LCDWAIT        ;
            PULA                    ; Restore A
            RTS                     ; Done


;; Write LCD string                       
;; IN: X = NULL-terimated string to print 
;;
_LCDWRITES  PSHX                    ; Save X
            PSHB                    ; Save B
_LCDSTRLOOP LDAB    0,X             ; Get next character to be written
            BEQ     _LCDSTRDONE     ; If NULL terminator, done
            JSR     _LCDWRITEC      ; Write character to SCI
            INX                     ; Next character
            BRA     _LCDSTRLOOP     ; Continue
_LCDSTRDONE PULB                    ; Restore B
            PULX                    ; Restore X
            RTS                     ; Done


;; Write LCD character        
;; IN: B = character to print 
;; NOTE: CR & LF are filtered 
;;
_LCDWRITEC  PSHB                    ; Save B
            CMPB    #CR             ; Don't do CRs
            BEQ     _LCDWRTDONE     ;
            CMPB    #LF             ; Don't do LFs
            BEQ     _LCDWRTDONE     ;
            STAB    DISPD           ; Write character to display
            JSR     _LCDWAIT        ;
_LCDWRTDONE PULB                    ; Restore B
            RTS                     ; Done


;; LCD wait loop 
;;
_LCDWAIT    PSHA                    ; Save A
_LCDWTLOOP  LDAA    DISPC           ; Wait for LCD to process last request
            ANDA    #LCD_WAITFLG    ;
            BNE     _LCDWTLOOP      ; Not ready to take new data
            PULA                    ; Restore A
            RTS                     ; Done


;; SWI handler                         
;; IN: A = command id to be executed   
;;
_SWIHDLR    PSHA                    ; Save A
            PSHB                    ; Save B
            PSHX                    ; Save X
            LDX     #APICMDTBL      ; Load API command table base
            TAB                     ; Copy A -> B
            LSLB                    ; Each offset is 2 bytes (multiply by 2)
            ABX                     ; Add B to X
            LDY     0,X             ; Get routine address
            PULX                    ; Restore X
            PULB                    ; Restore B
            PULA                    ; Restore A
            JSR     0,Y             ; Call API routine
            RTI                     ; Done


;;  Text strings  
;;

;; Welcome string
;;
WELCOMESTR  FCB CR
            FCB LF
            FCC "68MICRO Ver 0.5"
            FCB CR
            FCB LF
            FCB 0

;; Copyright string
;;
COPYRGHTSTR FCC "Copyright (c) 2012, Jeff Glaum.  All rights reserved."
            FCB CR
            FCB LF
            FCB 0

;; S-Record identifiers
;;
SREC_S1HDR  FCC "S1"
            FCB 0

SREC_S9HDR  FCC "S9"
            FCB 0

;; User commands
;;
COMMANDS    FCC "dump"              ; 1
            FCB 0
            FCC "enter"             ; 2
            FCB 0    
            FCC "go"                ; 3
            FCB 0    
            FCC "srec"              ; 4
            FCB 0
            FCC "reset"             ; 5
            FCB 0    
            FCC "api"               ; 6
            FCB 0
            FCC "regs"              ; 7
            FCB 0
            FCC "help"              ; 8
            FCB 0    
            FCC "?"                 ; 9
            FCB 0                   ; Must end in two NULLs
            FCB 0

DUMPSTR     FCB CR
            FCB LF
            FCC "Memory contents:"
            FCB CR
            FCB LF
            FCB CR
            FCB LF
            FCB 0

DUMPREGSSTR FCC "Registers:"
            FCB CR
            FCB LF
            FCB CR
            FCB LF
            FCB 0

REGLABELA   FCC "A: "
            FCB 0

REGLABELB   FCC "B: "
            FCB 0

REGLABELIX  FCC "IX: "
            FCB 0

REGLABELIY  FCC "IY: "
            FCB 0

REGLABELSP  FCC "SP: "
            FCB 0

REGLABELCC  FCC "CC: "
            FCB 0

ENTERSTR    FCB CR
            FCB LF
            FCC "Input each byte followed by enter.  To finish press enter twice."
            FCB CR
            FCB LF
            FCB 0

;; Resonse strings
;;
UNKWNCMDSTR FCC "Invalid command"
            FCB CR
            FCB LF
            FCB 0

;; Resonse string
;;
PROMPTSTR   FCC "> "
            FCB 0
         
;; Ready to receive string
;;
RECEIVESTR  FCC "Ready to receive S-Record data from host..."
            FCB CR
            FCB LF
            FCB 0

JUMPINGSTR  FCC "Jumping..."
            FCB CR
            FCB LF
            FCB 0

GETADDRSTR  FCC "Address: "
            FCB 0

SRECERRSTR  FCC "S-Record Error!"
            FCB 0

;; Help text
;;
HELPTEXT    FCB CR
            FCB LF
            FCC "Commands:"
            FCB CR
            FCB LF
            FCC "----------------------------------------------"
            FCB CR
            FCB LF
            FCC "dump     - display the contents of memory"
            FCB CR
            FCB LF
            FCC "enter    - write data to memory"
            FCB CR
            FCB LF
            FCC "go       - execute code in memory"
            FCB CR
            FCB LF
            FCC "srec     - download & execute s-record file"
            FCB CR
            FCB LF
            FCC "api      - display public monitor routines"
            FCB CR
            FCB LF
            FCC "regs     - display register values"
            FCB CR
            FCB LF
            FCC "reset    - reset the system"
            FCB CR
            FCB LF
            FCB 0

APIRTNSTXT  
            FCB CR
            FCB LF
            FCC "01: _ASCII2HEX"
            FCB CR
            FCB LF
            FCC "02: _CLEARINPUT"
            FCB CR
            FCB LF
            FCC "03: _DELAY_MS"
            FCB CR
            FCB LF
            FCC "04: _LCDCLEAR"
            FCB CR
            FCB LF
            FCC "05: _LCDINIT"
            FCB CR
            FCB LF
            FCC "06: _LCDWAIT"
            FCB CR
            FCB LF
            FCC "07: _LCDWRITEC"
            FCB CR
            FCB LF
            FCC "08: _LCDWRITES"
            FCB CR
            FCB LF
            FCC "09: _OUTLEFTH"
            FCB CR
            FCB LF
            FCC "0A: _OUTRIGHTH"
            FCB CR
            FCB LF
            FCC "0B: _SERCLEAR"
            FCB CR
            FCB LF
            FCC "0C: _SERINIT"
            FCB CR
            FCB LF
            FCC "0D: _SERPRTBYTE"
            FCB CR
            FCB LF
            FCC "0E: _SERPRTCR"
            FCB CR
            FCB LF
            FCC "0F: _SERPRTSPC"
            FCB CR
            FCB LF
            FCC "10: _SERREADC"
            FCB CR
            FCB LF
            FCC "11: _SERREADCx"
            FCB CR
            FCB LF
            FCC "12: _SERREADRAW"
            FCB CR
            FCB LF
            FCC "13: _SERREADS"
            FCB CR
            FCB LF
            FCC "14: _SERWRITEC"
            FCB CR
            FCB LF
            FCC "15: _SERWRITES"
            FCB CR
            FCB LF
            FCC "16: _STRCMP"
            FCB CR
            FCB LF
            FCC "17: _TOUPPER"
            FCB CR
            FCB LF
            FCB 0


;; Monitor command table
;;
APICMDTBL   EQU *
            FDB 0000             ; 00
            FDB _ASCII2HEX       ; 01
            FDB _CLEARINPUT      ; 02
            FDB _DELAY_MS        ; 03
            FDB _LCDCLEAR        ; 04
            FDB _LCDINIT         ; 05
            FDB _LCDWAIT         ; 06 
            FDB _LCDWRITEC       ; 07 
            FDB _LCDWRITES       ; 08 
            FDB _OUTLEFTH        ; 09
            FDB _OUTRIGHTH       ; 0A
            FDB _SERCLEAR        ; 0B
            FDB _SERINIT         ; 0C
            FDB _SERPRTBYTE      ; 0D
            FDB _SERPRTCR        ; 0E
            FDB _SERPRTSPC       ; 0F
            FDB _SERREADC        ; 10
            FDB _SERREADCx       ; 11
            FDB _SERREADRAW      ; 12
            FDB _SERREADS        ; 13
            FDB _SERWRITEC       ; 14
            FDB _SERWRITES       ; 15
            FDB _STRCMP          ; 16
            FDB _TOUPPER         ; 17


;; Interrupt vectors 
;;
            ORG $FFF6           ; SWI Vector
_SWI_VECT   FDB _SWIHDLR 

            ORG $FFFE           ; Reset vector
_RESET_VECT FDB _WEEZER

