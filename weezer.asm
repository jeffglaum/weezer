;;
;;  Motorola MC68HC11 SBC Monitor Program ("Weezer")
;;  Jeff Glaum
;;
;;  Revision	Date		Comment
;;  ------------------------------------------------------------------
;;  0.4		2012-Mar-10	Clean-up & bug fixes
;;  0.5		2012-Mar-18	Added suppport for dump, enter, regs
;;                        (tested on MC68HC11 simulator)
;;
;;
;;   Memory Map:
;;
;;	$0000	+---------------+
;;          |  256 bytes	|
;;          |  on-chip RAM	| <- Stack (lower) & Monitor variables (upper)
;;	$00FF	+---------------+
;;          |               |
;;          |    Unused     |
;;	$1000	+---------------+
;;          |   96 bytes	|
;;          |   registers	|
;;	$105F	+---------------+
;;          |               |
;;          |               |
;;          |               |
;;          |    Unused     |
;;          |               |
;;  $C000   +---------------+
;;          | LCD registers |
;;          +---------------+
;;          |               |
;;          |               |
;;	$D000	+---------------+
;;          |               |
;;          |  8192 bytes	|
;;          | off-chip RAM	| <- Free program memory
;;          |               |
;;          |               |
;;	$F000	+---------------+
;;          |               |
;;          |  4096 bytes	|
;;          | off-chip ROM	| <- Boot code and interrupt vectors
;;          |               |
;;          |               |
;;	$FFFF	+---------------+
;;


;; Program Addresses
;;
STACK_BASE  EQU     $80     ; Stack pointer base
PROGRAM_RAM EQU     $D000   ; Start of program RAM (free memory)

;; Serial UART Registers
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
INBUFF      RMB     $50     ; CLI Input buffer
INBUFFLEN   RMB     $1      ; CLI Input buffer length
TEMP        RMB     $1      ; Temporary variable
SRECADDR    RMB     $2      ; Current line's S-Record address


;; Main routine
;;
            ORG     $F000           ; Locate program at $F000
WEEZER      LDS     #STACK_BASE     ; Locate stack pointer
            JSR     SERINIT         ; Initialize SCI port
            JSR     LCDINIT         ; Initialize LCD display
            LDX     #WELCOMESTR     ; Display welcome string
            JSR     SERWRITES       ;  * serial port
            JSR     LCDWRITES       ;  * LCD
            LDX     #COPYRGHTSTR    ; Display copyright string
            JSR     SERWRITES       ;  * serial port
            JSR     SERCLEAR        ; Clear SCI input register
            CLI                     ; Enable interrupts
_PROMPT     LDX     #PROMPTSTR      ; Display input prompt
            JSR     SERWRITES       ;  * serial port
            JSR     CLEARINPUT      ; Clear input string buffer and length
            JSR     SERREADS        ; Get user's input
            JSR     PARSECMDS       ; Determine command specified
            CMPA    #0              ; If command not found - don't execute
            BEQ     _PROMPT         ;
            JSR     EXECUTECMD      ; Execute command
            BRA     _PROMPT         ;


;; Read SCI receive buffer until empty 
;;
SERCLEAR    PSHA
_SERCLR1    LDAA    SCSR
            ANDA    #$20
            BEQ     _SERCLRDONE
            LDAA    SCDR
            BRA     _SERCLR1
_SERCLRDONE PULA
            RTS
            
            
;; Clear command input string 
;;
CLEARINPUT  PSHA                    ; Save A
            LDAA    #0              ;
            STAA    INBUFF          ; Clear (terminate) input buffer
            STAA    INBUFFLEN       ; Zero input buffer length
            PULA                    ; Restore A
            RTS


;; Initialize serial port 
;;
SERINIT     PSHA                    ; Save A
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
SERWRITES   PSHX                    ; Save X
            PSHB                    ; Save B
_SERSTRLOOP LDAB    0,X             ; Get next character to be transmitted
            BEQ     _SERSTRDONE     ; If NULL terminator, done
            JSR     SERWRITEC       ; Write character to SCI
            INX                     ; Next character
            BRA     _SERSTRLOOP     ; Continue
_SERSTRDONE PULB                    ; Restore B
            PULX                    ; Restore X
            RTS                     ; Done


;; Write serial port character 
;; IN: B = character to print  
;;
SERWRITEC   PSHA                    ; Save A
_SERCHRWAIT LDAA    SCSR            ; Transmit register empty?
            ANDA    #TDRE           ;
            BEQ     _SERCHRWAIT     ;
            STAB    SCDR            ; Transmit contents of B
            PULA                    ; Restore A
            RTS                     ; Done


;; Read string from serial port                                           
;; OUT: INBUFF & INBUFFLEN contain the received data (CR or LF end input) 
;;
SERREADS    PSHA                    ; Save A
            PSHB                    ; Save B
            PSHX                    ; Save X
_SERREAD1   JSR     SERREADC        ; Read character from SCI
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
SERREADRAW  PSHA                    ; Save A
            PSHB                    ; Save B
            PSHX                    ; Save X
            LDX     #INBUFF         ; Get input buffer
_SERREADR1  CMPB    #0              ; No more characters to read?
            BEQ     _SERREADDN      ;
            JSR     SERREADCX       ; Read byte from SCI (no echo)
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
SERREADC   PSHB                    ; Save B
_SERREADC1 LDAA    SCSR            ; Check for received character
           ANDA    #$20            ;
           BEQ     _SERREADC1      ; No characters - wait
           LDAA    SCDR            ;
           PSHA                    ; Copy A -> B
           PULB                    ;
           JSR     SERWRITEC       ; Echo character to remote console
           PULB                    ; Restore B
           RTS                     ; Done

;; Read character from serial port (no echo)
;; OUT: A = character read from SCI        
;;
SERREADCX LDAA     SCSR            ; Check for received character
           ANDA    #$20            ;
           BEQ     SERREADCX      ; No characters - wait
           LDAA    SCDR            ;
           RTS                     ; Done


;; Determine command specified by user    
;; INTX X = NULL-terminated command string
;; OUT: A = command index number          
;;
PARSECMDS   PSHB                    ; Save B
            PSHX                    ; Save X
            PSHY                    ; Save Y
            JSR     SERPRTCR        ; Start a newline
            LDAB    #1              ; Initialize command array counter
            LDX     #INBUFF         ; Load pointer to input buffer string
            LDAA    0,X             ;
            CMPA    #0              ; If it's an empty string, exit
            BEQ     _PARSEDONE      ;
            LDY     #COMMANDS       ; Load pointer to command array string
_EXECLOOP   JSR     STRCMP          ; Compare command strings
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
            JSR     SERWRITES       ; Write message to serial port
            LDAA    #0              ; Bad command id
_PARSEDONE  PULY                    ; Restore Y
            PULX                    ; Restore X
            PULB                    ; Restore B
            RTS                     ; Done - return to input handler loop


;; Dispatch to chosen command
;; IN: A = Command id        
;;
EXECUTECMD  PSHA                    ; Save A
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
            JSR     SERWRITES       ;
            BRA     _CMDDISPDN      ;
_CMD1       JSR     DUMPMEM         ; Dump memory contents
            BRA     _CMDDISPDN      ;
_CMD2       JSR     ENTERMEM        ; Enter memory contents
            BRA     _CMDDISPDN      ;
_CMD3       JSR     JUMPTO          ; Go
            BRA     _CMDDISPDN      ;
_CMD4       JSR     SRECDOWNLD      ; Download & execute S-Record code
            BRA     _CMDDISPDN      ;
_CMD5       JSR     WEEZER          ; Reset
            BRA     _CMDDISPDN      ;
_CMD6       LDX     #APIRTNSTXT     ; Monitor routines list
            JSR     SERWRITES       ;
            BRA     _CMDDISPDN      ;
_CMD7       JSR     DUMPREGS        ; Registers
            BRA     _CMDDISPDN      ;
_CMD8       NOP                     ;
_CMD9       LDX     #HELPTEXT       ; ?/Help
            JSR     SERWRITES       ;
_CMDDISPDN  PULX                    ; Restore X
            PULB                    ; Restore B
            PULA                    ; Restore A
            RTS                     ; Done


;; S-Record download & execute 
;;
SRECDOWNLD  PSHA                    ; Save A
            PSHB                    ; Save B
            PSHX                    ; Save X
            PSHY                    ; Save Y
            LDX     #RECEIVESTR     ; Load receive string for display
            JSR     SERWRITES       ; Write string to serial port
_SRECGETLN  LDAB    #2              ; Read the first two characters (Sn header)
            JSR     SERREADRAW      ;
            LDX     #INBUFF         ; Load pointer to S-Record line
            LDY     #SREC_S1HDR     ; Load pointer to S1 header string
            JSR     STRCMP          ; Compare command strings
            CMPA    #0              ;
            BEQ     _FOUNDS1HDR     ; Found S1 header
            LDY     #SREC_S9HDR     ; Load pointer to S9 header string
            JSR     STRCMP          ; Compare command strings
            CMPA    #0              ;
            BEQ     _FOUNDS9HDR     ; Found S9 header
            LDX     #SRECERRSTR     ; ERROR: unrecognized S-Record header
            JSR     LCDCLEAR        ; Clear the LCD display
            JSR     LCDWRITES       ; Write string to serial port
            JSR     SERWRITES       ; Write string to serial port
            BRA     _SRECDONE       ;
_FOUNDS1HDR LDAA    #SRECHDR_S1     ; Keep track of the current line type (S1)
            PSHA                    ;
            BRA     _SRECCOMMON     ;
_FOUNDS9HDR LDAA    #SRECHDR_S9     ; Keep track of the current line type (S9)
            PSHA                    ;
_SRECCOMMON LDAB    #2              ; Get the number of remaining bytes in the line
            JSR     SERREADRAW      ;
            JSR     ASCII2HEX       ; Convert ASCII byte count to hex (result in A)
            PSHA                    ; Save number of bytes
_SRECADDR   LDAB    #4              ; Get the address
            JSR     SERREADRAW      ;
            JSR     ASCII2HEX       ; Convert ASCII addrH to hex (result in A)
            TAB                     ; A (addrH) -> B
            INX                     ; Index to the address Low field
            INX                     ;
            JSR     ASCII2HEX       ; Convert ASCII addrL to hex (result in A)
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
            JSR     SERREADRAW      ; Read the S-Record data bytes
            PULB                    ; Retrieve byte count
            LDX     #INBUFF         ; Load pointer to start of S-Record line
_STORESREC  JSR     ASCII2HEX       ; Convert ASCII byte to hex (result in A)
            STAA    0,Y             ; Store the converted byte value
            INX                     ;
            INX                     ;
            INY                     ;
            DECB                    ; Decrement the byte count
            BNE     _STORESREC      ; Continue until we're finished
_SRECCHK    LDAB    #2              ; Read the S-Record checksum byte
            JSR     SERREADRAW      ; 
;;
;; NOTE: we're ignoring the checksum - may want to fix this in the future.
;;
            PULA                    ; Recall the S-Record type from the stack
            CMPA    #SRECHDR_S1     ; Is it an S1 header?  If so, there's more to read...
            BEQ     _SRECGETLN      ; Found S1 header, continue reading.
_SRECLASTLN LDX     #JUMPINGSTR     ; Load jumping string for display
            JSR     SERWRITES       ; Write string to serial port
            LDY     SRECADDR        ; Recall the last address (should be S9 jump address)
            JSR     0,Y             ; Jump to the address in Y
_SRECDONE   PULY                    ; Restore Y
            PULX                    ; Restore X
            PULB                    ; Restore B
            PULA                    ; Restore A
            RTS                     ; Done

;; Enter memory            
;;
ENTERMEM   PSHA                    ; Save A
            PSHB                    ; Save B
            PSHX                    ; Save X
            PSHY                    ; Save Y
            LDX     #GETADDRSTR     ; Prompt for address
            JSR     SERWRITES       ;
            JSR     CLEARINPUT      ;
            JSR     SERREADS        ;
            LDX     #INBUFF         ; Get a pointer to the input buffer
            JSR     ASCII2HEX       ; Convert ASCII addrH to hex (result in A)
            TAB                     ; A (addrH) -> B
            INX                     ; Index to the address Low field
            INX                     ;
            JSR     ASCII2HEX       ; Convert ASCII addrL to hex (result in A)
            PSHA                    ; Push AddrL onto stack
            PSHB                    ; Push AddrH onto stack
            PULY                    ; Pull AddrH & AddrL (16-bits) into Y
            LDX     #ENTERSTR       ; Display enter data string
            JSR     SERWRITES       ;
_ENTERLOOP  JSR     CLEARINPUT      ; Clear input buffer and length
            JSR     SERREADS        ; Read serial string (i.e. byte text)
            JSR     SERPRTCR        ; Print CR to SCI
            LDX     #INBUFF         ; Check for 0 (indicating done)
            LDAA    0,X             ;
            BEQ     _ENTERDONE      ;
            JSR     ASCII2HEX       ; Convert ASCII byte to hex
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
GETINBADDR  LDX     #INBUFF
            RTS


;; Get input buffer address 
;;
GETINBLEN   LDX     #INBUFFLEN
            RTS


;; Convert text to upper case   
;; IN: A = character to convert
;;
TOUPPER     CMPA    #'a'            ; If ASCII digit isn't a character, skip
            BLO     _UPCASEDONE     ;
            CMPA    #'z'            ;
            BHI     _UPCASEDONE     ;
            SUBA    #$20            ; Convert lower case to upper case
_UPCASEDONE RTS                     ; Done


;; Convert text to binary               
;; IN:  LDX = input string (two digits)
;; OUT: A   = converted byte value    
;;
ASCII2HEX   PSHX                    ; Save X
            LDAA    0,X             ;
            JSR     TOUPPER         ; If character, convert to uppercase
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
            JSR     TOUPPER         ; If character, convert to uppercase
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
JUMPTO      PSHA                    ; Save A
            PSHB                    ; Save B
            PSHX                    ; Save X
            LDX     #GETADDRSTR     ; Prompt for address
            JSR     SERWRITES       ;
            JSR     CLEARINPUT      ;
            JSR     SERREADS        ;
            LDX     #INBUFF         ; Get a pointer to the input buffer
            JSR     ASCII2HEX       ; Convert ASCII addrH to hex (result in A)
            TAB                     ; A (addrH) -> B
            INX                     ; Index to the address Low field
            INX                     ;
            JSR     ASCII2HEX       ; Convert ASCII addrL to hex (result in A)
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
DUMPREGS    PSHA                    ; Save A
            PSHB                    ; Save B
            PSHX                    ; Save X
            PSHY                    ; Save Y
            PSHX                    ;
            LDX     #DUMPREGSSTR    ; Dump registers title
            JSR     SERWRITES       ;
            LDX     #REGLABELA      ; Register A label
            JSR     SERWRITES       ;
            JSR     SERPRTSPC       ; 
            PSHB                    ;
            TAB                     ;
            JSR     SERPRTBYTE      ; A value
            JSR     SERPRTCR        ; 
            LDX     #REGLABELB      ; Register B label
            JSR     SERWRITES       ;
            JSR     SERPRTSPC       ; 
            PULB                    ;
            JSR     SERPRTBYTE      ; B value
            JSR     SERPRTCR        ; 
            LDX     #REGLABELIX     ; Register IX label
            JSR     SERWRITES       ;
            PULA                    ; Pull top of 16-bit register X into A
            PULB                    ; Pull bottom of 16-bit register X into B
            JSR     SERPRTWORD      ; IX value
            JSR     SERPRTCR        ; 
            PSHY                    ;
            LDX     #REGLABELIY     ; Register IY label
            JSR     SERWRITES       ;
            PULA                    ; Pull top of 16-bit register Y into A
            PULB                    ; Pull bottom of 16-bit register Y into B
            JSR     SERPRTWORD      ; IY value
            JSR     SERPRTCR        ; 
            LDX     #REGLABELSP     ; Register SP label
            JSR     SERWRITES       ;
            TSX                     ; Move SP into IX
            XGDX                    ; Exchange D and IX
            JSR     SERPRTWORD      ; SP value
            JSR     SERPRTCR        ; 
            XGDX                    ; Exchange D and IX back again
            LDX     #REGLABELCC     ; Register CC label
            JSR     SERWRITES       ;
            TPA                     ; Move CC into A
            TAB                     ; Move A into B
            JSR     SERPRTBYTE      ; B value
            JSR     SERPRTCR        ; 
            JSR     SERPRTCR        ; 
            PULY                    ;
            PULX                    ;
            PULB                    ;
            PULA                    ;
            RTS                     ;


;; Dump memory contents    
;;
DUMPMEM     PSHA                    ; Save A
            PSHB                    ; Save B
            PSHX                    ; Save X
            LDX     #GETADDRSTR     ; Prompt for address
            JSR     SERWRITES       ;
            JSR     CLEARINPUT      ;
            JSR     SERREADS        ;
            LDX     #INBUFF         ; Get a pointer to the input buffer
            JSR     ASCII2HEX       ; Convert ASCII addrH to hex (result in A)
            TAB                     ; A (addrH) -> B
            INX                     ; Index to the address Low field
            INX                     ;
            JSR     ASCII2HEX       ; Convert ASCII addrL to hex (result in A)
            PSHA                    ; Push AddrL onto stack
            PSHB                    ; Push AddrH onto stack
            LDX     #DUMPSTR        ; Load dump text
            JSR     SERWRITES       ;
            LDAA    #0              ; Zero counter
            STAA    TEMP
            PULX                    ; Pull AddrH & AddrL (16-bits) into X
_DUMPMEM1   LDAA    #0
_DUMPLOOP   LDAB    0,X             ; Load byte from receive buffer
            JSR     SERPRTBYTE      ; Print byte in ascii to SCI
            JSR     SERPRTSPC       ; Print space
            INX                     ; Next byte
            INCA                    ; Increment byte count
            CMPA    #$10
            BNE     _DUMPLOOP       ;
            JSR     SERPRTCR        ; Print CR to SCI
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
OUTLEFTH    LSRB                    ; Shift data to right
            LSRB
            LSRB
            LSRB
OUTRIGHTH   ANDB    #$0F            ; Mask top half
            ADDB    #$30            ; Convert to ascii
            CMPB    #$39
            BLE     _OUTA           ; Jump if 0-9
            ADDB    #$07            ; Convert to hex A-F
_OUTA       JSR     SERWRITEC       ; Output character
            RTS


;; Print byte to serial port 
;; IN: B = byte to be printed 
;;
SERPRTBYTE  PSHB
            PSHB
            JSR     OUTLEFTH        ; Output left half
            PULB                    ; Retrieve copy
            JSR     OUTRIGHTH       ; Output right half
            PULB
            RTS


;; Print word to serial port 
;; IN: D = word to be printed 
;;
SERPRTWORD  PSHA
            PSHB
            TAB
            JSR     SERPRTBYTE      ; Print top byte
            PULB                 
            JSR     SERPRTBYTE      ; Print low byte
            PULA
            RTS


;; Print LF & CR to serial port
;;
SERPRTCR    PSHB
            LDAB    #CR
            JSR     SERWRITEC
            LDAB    #LF
            JSR     SERWRITEC
            PULB
            RTS


;; Print space to serial port 
;;
SERPRTSPC  PSHB
            LDAB    #SPACE
            JSR     SERWRITEC
            PULB
            RTS


;; String compare routine                            
;;                                                   
;; IN:  X = Pointer to string #1                     
;; IN:  Y = Pointer to string #2                     
;; OUT: A = compare status (0 = match, 1 = NO match)
;;
STRCMP      PSHB                    ; Save B
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
LCDCLEAR    PSHA                    ; Save A
            LDAA    #$1             ; Clear all display and return cursor home
            STAA    DISPC           ;
            JSR     LCDWAIT         ;
            PULA                    ; Restore A
            RTS                     ; Done


;; Simple ms delay routine                           
;;                                                   
;; IN: A = Number of ms to delay (1-255)            
;;                                                  
;; NOTE: *** Based on E=2 MHz (8 MHz xtal) ***     
;;
DELAY_MS    PSHX
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
LCDINIT     PSHA                    ; Save A
            LDAA    #15             ; Wait 15ms to give Vcc time to stabilize
            JSR     DELAY_MS        ;
            LDAA    #LCD_CONFIG     ; Interface len=8, 2 lines, 5x7 font
            STAA    DISPC           ;
            LDAA    #5              ; Wait 5ms
            JSR     DELAY_MS        ;
            LDAA    #LCD_CONFIG     ; REPEAT: Interface len=8, 2 lines, 5x7 font
            STAA    DISPC           ;
            LDAA    #1              ; Wait 1ms
            JSR     DELAY_MS        ;
            LDAA    #LCD_CONFIG     ; REPEAT: Interface len=8, 2 lines, 5x7 font
            STAA    DISPC           ;
            JSR     LCDWAIT         ; Busy Flag can now be checked
            LDAA    #LCD_CONFIG     ; Interface len=8, 2 lines, 5x7 font
            STAA    DISPC           ;
            JSR     LCDWAIT         ;
            LDAA    #LCD_DISPON     ; Display on, cursor off, cursor blink off
            STAA    DISPC           ;
            JSR     LCDWAIT         ;
            LDAA    #LCD_HOME       ; Clear all display and return cursor home
            STAA    DISPC           ;
            LDAA    #2              ; Wait 2ms
            JSR     DELAY_MS        ;
            JSR     LCDWAIT         ;
            LDAA    #LCD_CRSCFG     ; Increment cursor and don't shift display
            STAA    DISPC           ;
            JSR     LCDWAIT         ;
            LDAA    #LCD_RAMADDR    ; Set DDRAM address to position 0.
            STAA    DISPC           ;
            JSR     LCDWAIT         ;
            PULA                    ; Restore A
            RTS                     ; Done


;; Write LCD string                       
;; IN: X = NULL-terimated string to print 
;;
LCDWRITES   PSHX                    ; Save X
            PSHB                    ; Save B
_LCDSTRLOOP LDAB    0,X             ; Get next character to be written
            BEQ     _LCDSTRDONE     ; If NULL terminator, done
            JSR     LCDWRITEC       ; Write character to SCI
            INX                     ; Next character
            BRA     _LCDSTRLOOP     ; Continue
_LCDSTRDONE PULB                    ; Restore B
            PULX                    ; Restore X
            RTS                     ; Done


;; Write LCD character        
;; IN: B = character to print 
;; NOTE: CR & LF are filtered 
;;
LCDWRITEC   PSHB                    ; Save B
            CMPB    #CR             ; Don't do CRs
            BEQ     _LCDWRTDONE     ;
            CMPB    #LF             ; Don't do LFs
            BEQ     _LCDWRTDONE     ;
            STAB    DISPD           ; Write character to display
            JSR     LCDWAIT         ;
_LCDWRTDONE PULB                    ; Restore B
            RTS                     ; Done


;; LCD wait loop 
;;
LCDWAIT     PSHA                    ; Save A
_LCDWTLOOP  LDAA    DISPC           ; Wait for LCD to process last request
            ANDA    #LCD_WAITFLG    ;
            BNE     _LCDWTLOOP      ; Not ready to take new data
            PULA                    ; Restore A
            RTS                     ; Done


;; SWI handler                         
;; IN: A = command id to be executed   
;;
SWIHDLR     PSHA                    ; Save A
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
            FCC "68MICRO Ver 0.6"
            FCB CR
            FCB LF
            FCB 0

;; Copyright string
;;
COPYRGHTSTR FCC "Copyright (c) 2026, Jeff Glaum.  All rights reserved."
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
RECEIVESTR  FCC "Ready to receive s-record data from host..."
            FCB CR
            FCB LF
            FCB 0

JUMPINGSTR  FCC "Jumping..."
            FCB CR
            FCB LF
            FCB 0

GETADDRSTR  FCC "Address: "
            FCB 0

SRECERRSTR  FCC "S-record error!"
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
            FCC "01: ASCII2HEX"
            FCB CR
            FCB LF
            FCC "02: CLEARINPUT"
            FCB CR
            FCB LF
            FCC "03: DELAY_MS"
            FCB CR
            FCB LF
            FCC "04: LCDCLEAR"
            FCB CR
            FCB LF
            FCC "05: LCDINIT"
            FCB CR
            FCB LF
            FCC "06: LCDWAIT"
            FCB CR
            FCB LF
            FCC "07: LCDWRITEC"
            FCB CR
            FCB LF
            FCC "08: LCDWRITES"
            FCB CR
            FCB LF
            FCC "09: OUTLEFTH"
            FCB CR
            FCB LF
            FCC "0A: OUTRIGHTH"
            FCB CR
            FCB LF
            FCC "0B: SERCLEAR"
            FCB CR
            FCB LF
            FCC "0C: SERINIT"
            FCB CR
            FCB LF
            FCC "0D: SERPRTBYTE"
            FCB CR
            FCB LF
            FCC "0E: SERPRTCR"
            FCB CR
            FCB LF
            FCC "0F: SERPRTSPC"
            FCB CR
            FCB LF
            FCC "10: SERREADC"
            FCB CR
            FCB LF
            FCC "11: SERREADCx"
            FCB CR
            FCB LF
            FCC "12: SERREADRAW"
            FCB CR
            FCB LF
            FCC "13: SERREADS"
            FCB CR
            FCB LF
            FCC "14: SERWRITEC"
            FCB CR
            FCB LF
            FCC "15: SERWRITES"
            FCB CR
            FCB LF
            FCC "16: STRCMP"
            FCB CR
            FCB LF
            FCC "17: TOUPPER"
            FCB CR
            FCB LF
            FCB 0


;; Monitor command table
;;
APICMDTBL   EQU *
            FDB 0000            ; 00
            FDB ASCII2HEX       ; 01
            FDB CLEARINPUT      ; 02
            FDB DELAY_MS        ; 03
            FDB LCDCLEAR        ; 04
            FDB LCDINIT         ; 05
            FDB LCDWAIT         ; 06 
            FDB LCDWRITEC       ; 07 
            FDB LCDWRITES       ; 08 
            FDB OUTLEFTH        ; 09
            FDB OUTRIGHTH       ; 0A
            FDB SERCLEAR        ; 0B
            FDB SERINIT         ; 0C
            FDB SERPRTBYTE      ; 0D
            FDB SERPRTCR        ; 0E
            FDB SERPRTSPC       ; 0F
            FDB SERREADC        ; 10
            FDB SERREADCx       ; 11
            FDB SERREADRAW      ; 12
            FDB SERREADS        ; 13
            FDB SERWRITEC       ; 14
            FDB SERWRITES       ; 15
            FDB STRCMP          ; 16
            FDB TOUPPER         ; 17


;; Interrupt vectors 
;;
            ORG $FFF6           ; SWI Vector
_SWI_VECT   FDB SWIHDLR 

            ORG $FFFE           ; Reset vector
_RESET_VECT FDB WEEZER

