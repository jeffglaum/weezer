*/
*/   SBC Memory Map
*/
*/	$0000	+---------------+
*/              |  1024 bytes	|
*/              |  on-chip RAM	| <- Stack
*/	$03FF	|---------------|
*/              |               |
*/              |    Unused     |
*/	$1000	|---------------|
*/              |   96 bytes	|
*/              |   registers	|
*/	$105F	|---------------|
*/              |               |
*/              |               |
*/              |               |
*/              |    Unused     |
*/              |               |
*/              |               |
*/              |               |
*/              |               |
*/              |               |
*/	$D000	|---------------|
*/              |               |
*/              |  8192 bytes	|
*/              | off-chip RAM	| <- Program memory
*/              |               |
*/              |               |
*/	$F000	|---------------|
*/              |               |
*/              |  4096 bytes	|
*/              | off-chip ROM	| <- Boot code and interrupt vectors
*/              |               |
*/              |               |
*/	$FFFF	+---------------+
*/

*///////////////
*// Constants //
*///////////////

*// Registers
*//
SPCR        EQU $1028       ; SPI control register
BAUD        EQU $102B       ; SCI BAUD register
SCCR1       EQU $102C       ; SCI control register #1
SCCR2       EQU $102D       ; SCI control register #2
SCSR        EQU $102E       ; SCI control status register
SCDR        EQU $102F       ; SCI data register

DISPC       EQU $C000       ; LCD control register
DISPD       EQU $C001       ; LCD data register

*// SCI Constants
*//
BAUD_RATE   EQU $30         ; 9600 baud
SCCR2_VAL   EQU $0C         ; SCCR2 value
TDRE        EQU $80         ; Transmit data register empty

*// ASCII Constants
*//
SPACE       EQU 32          ; Space
CR          EQU 13          ; CR
LF          EQU 10          ; LF
DEL         EQU 127         ; DEL
BS          EQU 8           ; BS
BELL        EQU 7           ; BELL

*// S-Record Constants
*//
SR_MAX_LINE EQU 74          ; Maximum S-Record line size (S1PPAAAAdddd.ddCC)
SR_PAIROFST EQU 2           ; S-Record line character pair count offset
SR_ADDROFST EQU 4           ; S-Record address offset
SR_DATAOFST EQU 8           ; S-Record start of data offset


STACK_BASE  EQU $80         ; Stack pointer base

*// Input buffer and buffer length variables
*//
            ORG     $D000
TEMP        RMB     $1      ; Temporary variable
BIOSRTN     RMB     $2      ; Address of BIOS routine to execute
INBUFFLEN   RMB     $1      ; Input buffer length
INBUFF      RMB     $50     ; Input buffer
RECBUFFLEN  RMB     $1      ; Program receive buffer length
RECBUFF     RMB     256     ; Program receive buffer
FREERAM     EQU     *


*//////////////////
*// Main routine //
*//////////////////
            ORG     $F000           ; Locate program at $F000
WEEZER      LDS     #STACK_BASE     ; Locate stack pointer
>> f000:  8e 00 80
            JSR     CLRINSTRING     ; Clear user input string and length
>> f003:  bd f0 4d
            LDAA    #0
>> f006:  86 00
            STAA    RECBUFFLEN      ;
>> f008:  b7 d0 54
            STAA    RECBUFF         ;
>> f00b:  b7 d0 55
            JSR     SERIAL_INIT     ; Initialize SCI port
>> f00e:  bd f0 58
            JSR     LCD_INIT        ; Initialize LCD display
>> f011:  bd f3 18
            LDX     #WELCOMESTR     ; Load welcome string for display
>> f014:  ce f3 b2
            JSR     SERWRITESTR     ; Write string to serial port
>> f017:  bd f0 6a
            JSR     LCDWRITESTR     ; Write string to LCD
>> f01a:  bd f3 69
            LDX     #COPYRGHTSTR    ; Load copyright string for display
>> f01d:  ce f3 c6
            JSR     SERWRITESTR     ; Write string to serial port
>> f020:  bd f0 6a
            JSR     SERCLRINPUT     ; Clear SCI input register
>> f023:  bd f0 3e
PROMPT      JSR     CLRINSTRING     ; Clear input string buffer and length
>> f026:  bd f0 4d
            LDX     #PROMPTSTR      ; Load prompt string
>> f029:  ce f4 9e
            JSR     SERWRITESTR     ;
>> f02c:  bd f0 6a
            JSR     SERREADSTR      ; Get user's input
>> f02f:  bd f0 86
            JSR     PARSECMDSTR     ; Determine command specified
>> f032:  bd f0 d2
            CMPA    #0              ; If command not found - don't execute
>> f035:  81 00
            BEQ     PROMPT          ;
>> f037:  27 ed
            JSR     EXECUTECMD      ; Execute command
>> f039:  bd f1 08
            BRA     PROMPT          ;
>> f03c:  20 e8


*/////////////////////////////////////////
*// Read SCI receive buffer until empty //
*/////////////////////////////////////////
SERCLRINPUT PSHA
>> f03e:  36
SERCLR1     LDAA    SCSR
>> f03f:  b6 10 2e
            ANDA    #$20
>> f042:  84 20
            BEQ     SERCLRDONE
>> f044:  27 05
            LDAA    SCDR
>> f046:  b6 10 2f
            BRA     SERCLR1
>> f049:  20 f4
SERCLRDONE  PULA
>> f04b:  32
            RTS
>> f04c:  39
            
            
*///////////////////////////////
*/ Clear command input string //
*///////////////////////////////
CLRINSTRING PSHA                    ; Save A
>> f04d:  36
            LDAA    #0              ;
>> f04e:  86 00
            STAA    INBUFF          ; Clear input buffer
>> f050:  b7 d0 04
            STAA    INBUFFLEN       ; Zero input buffer length
>> f053:  b7 d0 03
            PULA                    ; Restore A
>> f056:  32
            RTS
>> f057:  39


*////////////////////////////
*// Initialize serial port //
*////////////////////////////
SERIAL_INIT PSHA                    ; Save A
>> f058:  36
            LDAA    #BAUD_RATE      ; Set baud rate to 9600 (assume 8Mhz xtal)
>> f059:  86 30
            STAA    BAUD            ;
>> f05b:  b7 10 2b
            LDAA    #0              ; Clear SCCR1 (1 start, 8 data, 1 stop)
>> f05e:  86 00
            STAA    SCCR1           ;
>> f060:  b7 10 2c
            LDAA    #SCCR2_VAL      ; Enable TxD and RxD
>> f063:  86 0c
            STAA    SCCR2           ;
>> f065:  b7 10 2d
            PULA                    ; Restore A
>> f068:  32
            RTS                     ; Done
>> f069:  39


*//////////////////////////////
*// Write serial port string //
*//////////////////////////////
SERWRITESTR PSHX                    ; Save X
>> f06a:  3c
            PSHB                    ; Save B
>> f06b:  37
SERSTRLOOP  LDAB    0,X             ; Get next character to be transmitted
>> f06c:  e6 00
            BEQ     SERSTRDONE      ; If NULL terminator, done
>> f06e:  27 06
            JSR     SERWRITECHR     ; Write character to SCI
>> f070:  bd f0 79
            INX                     ; Next character
>> f073:  08
            BRA     SERSTRLOOP      ; Continue
>> f074:  20 f6
SERSTRDONE  PULB                    ; Restore B
>> f076:  33
            PULX                    ; Restore X
>> f077:  38
            RTS                     ; Done
>> f078:  39


*/////////////////////////////////
*// Write serial port character //
*/////////////////////////////////
SERWRITECHR PSHA                    ; Save A
>> f079:  36
SERCHRWAIT  LDAA    SCSR            ; Transmit register empty?
>> f07a:  b6 10 2e
            ANDA    #TDRE           ;
>> f07d:  84 80
            BEQ     SERCHRWAIT      ;
>> f07f:  27 f9
            STAB    SCDR            ; Transmit contents of B
>> f081:  f7 10 2f
            PULA                    ; Restore A
>> f084:  32
            RTS                     ; Done
>> f085:  39


*//////////////////////////////////
*// Read string from serial port //
*//////////////////////////////////
SERREADSTR  PSHA                    ; Save A
>> f086:  36
            PSHB                    ; Save B
>> f087:  37
            PSHX                    ; Save X
>> f088:  3c
SERREAD1    JSR     SERREADCHR      ; Read character from SCI
>> f089:  bd f0 c0
            CMPA    #BS             ; Backspace - delete last character
>> f08c:  81 08
            BEQ     SERBKSPACE      ;
>> f08e:  27 21
            CMPA    #CR             ; Carriage return - NULL terminate string
>> f090:  81 0d
            BNE     SERREAD15       ;
>> f092:  26 04
            LDAA    #0              ; NULL-terminate
>> f094:  86 00
            BRA     SERREAD2        ;
>> f096:  20 06
SERREAD15   CMPA    #LF             ; Line feed - NULL terminate string
>> f098:  81 0a
            BNE     SERREAD2        ;
>> f09a:  26 02
            LDAA    #0              ; NULL-terminate
>> f09c:  86 00
SERREAD2    LDX     #INBUFF         ; Get input buffer
>> f09e:  ce d0 04
            LDAB    INBUFFLEN       ; Get input buffer length
>> f0a1:  f6 d0 03
            ABX                     ; Offset past current data
>> f0a4:  3a
            STAA    0,X             ; Store character in buffer
>> f0a5:  a7 00
            INCB                    ; Increment string length
>> f0a7:  5c
            STAB    INBUFFLEN       ; Store string length
>> f0a8:  f7 d0 03
            CMPA    #0              ; If NULL (end of string), execute command
>> f0ab:  81 00
            BNE     SERREAD1        ; If NOT CR - continue
>> f0ad:  26 da
            BRA     SERREADDONE     ; CR - Done
>> f0af:  20 0b
SERBKSPACE  LDAA    INBUFFLEN       ; Get character string length
>> f0b1:  b6 d0 03
            BEQ     SERREAD1        ; If string length is zero - done
>> f0b4:  27 d3
            DECA                    ; Decrement string length
>> f0b6:  4a
            STAA    INBUFFLEN       ; Store new string length
>> f0b7:  b7 d0 03
            BRA     SERREAD1        ; Continue
>> f0ba:  20 cd
SERREADDONE PULX                    ; Restore X
>> f0bc:  38
            PULB                    ; Restore B
>> f0bd:  33
            PULA                    ; Restore A
>> f0be:  32
            RTS                     ; Done
>> f0bf:  39


*/////////////////////////////////////
*/ Read character from serial port  //
*/ OUT: A = character read from SCI //
*/////////////////////////////////////
SERREADCHR  PSHB                    ; Save B
>> f0c0:  37
SERREADCHR1 LDAA    SCSR            ; Check for received character
>> f0c1:  b6 10 2e
            ANDA    #$20            ;
>> f0c4:  84 20
            BEQ     SERREADCHR1     ; No characters - wait
>> f0c6:  27 f9
            LDAA    SCDR            ;
>> f0c8:  b6 10 2f
            PSHA                    ; Copy A -> B
>> f0cb:  36
            PULB                    ;
>> f0cc:  33
            JSR     SERWRITECHR     ; Echo character to remote console
>> f0cd:  bd f0 79
            PULB                    ; Restore B
>> f0d0:  33
            RTS                     ; Done
>> f0d1:  39


*//////////////////////////////////////////
*// Determine command specified by user  //
*// OUT: A = command index number        //
*//////////////////////////////////////////
PARSECMDSTR PSHB                    ; Save B
>> f0d2:  37
            PSHX                    ; Save X
>> f0d3:  3c
            PSHY                    ; Save Y
>> f0d4:  18 3c
            LDAB    #1              ; Initialize command array counter
>> f0d6:  c6 01
            LDX     #INBUFF         ; Load pointer to input buffer string
>> f0d8:  ce d0 04
            LDY     #COMMANDS       ; Load pointer to command array string
>> f0db:  18 ce f4 04
EXECLOOP    JSR     STRCMP          ; Compare command strings
>> f0df:  bd f2 cd
            CMPA    #0              ;
>> f0e2:  81 00
            BEQ     FOUNDCMD        ; Found command (command number in B)
>> f0e4:  27 11
            INCB                    ; Increment command array counter
>> f0e6:  5c
NEXTCMD     INY                     ;
>> f0e7:  18 08
            LDAA    0,Y             ;
>> f0e9:  18 a6 00
            BNE     NEXTCMD         ; Loop until next command
>> f0ec:  26 f9
            INY                     ;
>> f0ee:  18 08
            LDAA    0,Y             ;
>> f0f0:  18 a6 00
            BEQ     BADCMD          ; End of command array and not found - done
>> f0f3:  27 06
            BRA     EXECLOOP        ;
>> f0f5:  20 e8
FOUNDCMD    PSHB                    ; Copy B -> A
>> f0f7:  37
            PULA                    ;
>> f0f8:  32
            BRA     PARSEDONE       ; 
>> f0f9:  20 08
BADCMD      LDX     #UNKWNCMDSTR    ; Load invalid command string text
>> f0fb:  ce f4 8a
            JSR     SERWRITESTR     ; Write message to serial port
>> f0fe:  bd f0 6a
            LDAA    #0              ; Bad command id
>> f101:  86 00
PARSEDONE   PULY                    ; Restore Y
>> f103:  18 38
            PULX                    ; Restore X
>> f105:  38
            PULB                    ; Restore B
>> f106:  33
            RTS                     ; Done - return to input handler loop
>> f107:  39


*////////////////////////////////
*// Dispatch to chosen command //
*// IN: A = Command id         //
*////////////////////////////////
EXECUTECMD  PSHA                    ; Save A
>> f108:  36
            PSHB                    ; Save B
>> f109:  37
            PSHX                    ; Save X
>> f10a:  3c
            CMPA    #$1             ; #1: Srec
>> f10b:  81 01
            BEQ     CMD1            ;
>> f10d:  27 24
            CMPA    #$2             ; #2: Dump
>> f10f:  81 02
            BEQ     CMD2            ;
>> f111:  27 25
            CMPA    #$3             ; #3: Enter
>> f113:  81 03
            BEQ     CMD3            ;
>> f115:  27 26
            CMPA    #$4             ; #4: Go
>> f117:  81 04
            BEQ     CMD4            ;
>> f119:  27 27
            CMPA    #$5             ; #5: Execute
>> f11b:  81 05
            BEQ     CMD5            ;
>> f11d:  27 28
            CMPA    #$6             ; #6: ?
>> f11f:  81 06
            BEQ     CMD6            ;
>> f121:  27 29
            CMPA    #$7             ; #7: Help
>> f123:  81 07
            BEQ     CMD7            ;
>> f125:  27 2d
            CMPA    #$8             ; #8: BIOS routines list
>> f127:  81 08
            BEQ     CMD8            ;
>> f129:  27 31
            LDX     #UNKWNCMDSTR    ; Invalid command
>> f12b:  ce f4 8a
            JSR     SERWRITESTR     ;
>> f12e:  bd f0 6a
            BRA     CMDDISPDONE     ;
>> f131:  20 31
CMD1        JSR     DOWNLD_SREC     ; Download & execute S-Record code
>> f133:  bd f1 68
            BRA     CMDDISPDONE     ;
>> f136:  20 2c
CMD2        JSR     DUMPMEM         ; Dump memory contents
>> f138:  bd f2 69
            BRA     CMDDISPDONE     ;
>> f13b:  20 27
CMD3        JSR     ENTERMEM        ; Enter memory contents
>> f13d:  bd f1 f4
            BRA     CMDDISPDONE     ;
>> f140:  20 22
CMD4        JSR     RECBUFF         ; Go
>> f142:  bd d0 55
            BRA     CMDDISPDONE     ;
>> f145:  20 1d
CMD5        JSR     WEEZER          ; Reset
>> f147:  bd f0 00
            BRA     CMDDISPDONE     ;
>> f14a:  20 18
CMD6        LDX     #HELPTEXT       ; ?
>> f14c:  ce f5 05
            JSR     SERWRITESTR     ;
>> f14f:  bd f0 6a
            BRA     CMDDISPDONE     ;
>> f152:  20 10
CMD7        LDX     #HELPTEXT       ; Help
>> f154:  ce f5 05
            JSR     SERWRITESTR     ;
>> f157:  bd f0 6a
            BRA     CMDDISPDONE     ;
>> f15a:  20 08
CMD8        LDX     #BIOSRTNSTXT    ; BIOS routines list
>> f15c:  ce f6 0c
            JSR     SERWRITESTR     ;
>> f15f:  bd f0 6a
            BRA     CMDDISPDONE     ;
>> f162:  20 00
CMDDISPDONE PULX                    ; Restore X
>> f164:  38
            PULB                    ; Restore B
>> f165:  33
            PULA                    ; Restore A
>> f166:  32
            RTS                     ; Done
>> f167:  39


*////////////////////////////////
*// S-Record receive & execute //
*////////////////////////////////
DOWNLD_SREC PSHA                    ; Save A
>> f168:  36
            PSHB                    ; Save B
>> f169:  37
            PSHX                    ; Save X
>> f16a:  3c
            PSHY                    ; Save Y
>> f16b:  18 3c
            LDX     #RECEIVESTR     ; Load receive string for display
>> f16d:  ce f4 ad
            JSR     SERWRITESTR     ; Write string to serial port
>> f170:  bd f0 6a
            LDX     #DOWNLOADSTR    ; Load downloading string for display
>> f173:  ce f4 db
            JSR     LCD_CLEAR       ; Clear the LCD display
>> f176:  bd f2 fc
            JSR     LCDWRITESTR     ; Write string to serial port
>> f179:  bd f3 69
SREC_GETLN  JSR     SERREADSTR      ; Get S-Record line
>> f17c:  bd f0 86
            LDX     #INBUFF         ; Load pointer to S-Record line
>> f17f:  ce d0 04
            LDY     #SREC_S1HDR     ; Load pointer to S1 header string
>> f182:  18 ce f3 fe
            JSR     STRCMP          ; Compare command strings
>> f186:  bd f2 cd
            CMPA    #0              ;
>> f189:  81 00
            BEQ     FOUND_S1HDR     ; Found S1 header
>> f18b:  27 16
            LDY     #SREC_S9HDR     ; Load pointer to S9 header string
>> f18d:  18 ce f4 01
            JSR     STRCMP          ; Compare command strings
>> f191:  bd f2 cd
            CMPA    #0              ;
>> f194:  81 00
            BEQ     FOUND_S9HDR     ; Found S9 header
>> f196:  27 36
            LDX     #SRECERRSTR     ; ERROR: unrecognized S-Record header
>> f198:  ce f4 f5
            JSR     LCD_CLEAR       ; Clear the LCD display
>> f19b:  bd f2 fc
            JSR     LCDWRITESTR     ; Write string to serial port
>> f19e:  bd f3 69
            BRA     SREC_DONE       ;
>> f1a1:  20 4b
FOUND_S1HDR LDAB    #SR_PAIROFST    ; Offset X to the character pair count
>> f1a3:  c6 02
            ABX                     ;
>> f1a5:  3a
            JSR     ASCII2HEX       ; Convert ASCII count to hex (result in A)
>> f1a6:  bd f2 39
            SUBA    #$3             ; Reduce the count by 3 (addr & checksum)
>> f1a9:  80 03
            INX                     ; Index to the address High field
>> f1ab:  08
            INX                     ;
>> f1ac:  08
            PSHA                    ; Save data byte count
>> f1ad:  36
            JSR     ASCII2HEX       ; Convert ASCII addrH to hex (result in A)
>> f1ae:  bd f2 39
            TAB                     ; A (addrH) -> B
>> f1b1:  16
            INX                     ; Index to the address Low field
>> f1b2:  08
            INX                     ;
>> f1b3:  08
            JSR     ASCII2HEX       ; Convert ASCII addrL to hex (result in A)
>> f1b4:  bd f2 39
            PSHA                    ; Push AddrL onto stack
>> f1b7:  36
            PSHB                    ; Push AddrH onto stack
>> f1b8:  37
            PULY                    ; Pull AddrH & AddrL (16-bits) into Y
>> f1b9:  18 38
            PULB                    ; Restore the data byte count (**into B**)
>> f1bb:  33
            INX                     ; Index to the start of the S-Record data
>> f1bc:  08
            INX                     ;
>> f1bd:  08
STORE_SREC  JSR     ASCII2HEX       ; Convert ASCII byte to hex (result in A)
>> f1be:  bd f2 39
            STAA    0,Y             ; Store the converted byte value
>> f1c1:  18 a7 00
            INX                     ;
>> f1c4:  08
            INX                     ;
>> f1c5:  08
            INY                     ;
>> f1c6:  18 08
            SUBB    #1              ; Decrement the byte count
>> f1c8:  c0 01
            BNE     STORE_SREC      ; Continue until we're finished
>> f1ca:  26 f2
*// NOTE: we're ignoring the checksum - may want to fix this in the future.
            BRA     SREC_GETLN      ; Read next line
>> f1cc:  20 ae
FOUND_S9HDR LDX     #JUMPINGSTR     ; Load jumping string for display
>> f1ce:  ce f4 ea
            JSR     LCD_CLEAR       ; Clear the LCD display
>> f1d1:  bd f2 fc
            JSR     LCDWRITESTR     ; Write string to serial port
>> f1d4:  bd f3 69
            LDX     #INBUFF         ; Load pointer to S-Record line
>> f1d7:  ce d0 04
            INX                     ; Index to the S-Record address field
>> f1da:  08
            INX                     ;
>> f1db:  08
            INX                     ;
>> f1dc:  08
            INX                     ;
>> f1dd:  08
            JSR     ASCII2HEX       ; Convert ASCII addrH to hex (result in A)
>> f1de:  bd f2 39
            TAB                     ; A (addrH) -> B
>> f1e1:  16
            INX                     ; Index to the address Low field
>> f1e2:  08
            INX                     ;
>> f1e3:  08
            JSR     ASCII2HEX       ; Convert ASCII addrL to hex (result in A)
>> f1e4:  bd f2 39
            PSHA                    ; Push AddrL onto stack
>> f1e7:  36
            PSHB                    ; Push AddrH onto stack
>> f1e8:  37
            PULY                    ; Pull AddrH & AddrL (16-bits) into Y
>> f1e9:  18 38
            JSR     0,Y             ; Jump to the address in Y
>> f1eb:  18 ad 00
SREC_DONE   PULY                    ; Restore Y
>> f1ee:  18 38
            PULX                    ; Restore X
>> f1f0:  38
            PULB                    ; Restore B
>> f1f1:  33
            PULA                    ; Restore A
>> f1f2:  32
            RTS                     ; Done
>> f1f3:  39

*//////////////////
*// Enter memory //
*//////////////////
ENTERMEM    PSHA                    ; Save A
>> f1f4:  36
            PSHB                    ; Save B
>> f1f5:  37
            PSHX                    ; Save X
>> f1f6:  3c
            JSR     SERCLRINPUT     ; Clear SCI receive buffer
>> f1f7:  bd f0 3e
            LDX     #ENTERSTR       ; Display enter data string
>> f1fa:  ce f4 43
            JSR     SERWRITESTR     ;
>> f1fd:  bd f0 6a
ENTERLOOP   JSR     CLRINSTRING     ; Clear input buffer and length
>> f200:  bd f0 4d
            JSR     SERREADSTR      ; Read serial string (i.e. byte text)
>> f203:  bd f0 86
            JSR     SERPRTCR        ; Print CR to SCI
>> f206:  bd f2 b8
            LDX     #INBUFF         ; Check for 0 (indicating done)
>> f209:  ce d0 04
            LDAA    0,X             ;
>> f20c:  a6 00
            BEQ     ENTERDONE       ;
>> f20e:  27 12
            JSR     ASCII2HEX       ; Convert ASCII byte to hex
>> f210:  bd f2 39
            LDX     #RECBUFF        ; Address of next byte in receive buffer
>> f213:  ce d0 55
            LDAB    RECBUFFLEN      ;
>> f216:  f6 d0 54
            ABX                     ;
>> f219:  3a
            STAA    0,X             ; Store data byte in receive buffer
>> f21a:  a7 00
            INCB                    ; Increment buffer length and store
>> f21c:  5c
            STAB    RECBUFFLEN      ;
>> f21d:  f7 d0 54
            BRA     ENTERLOOP       ; Continue with input stream
>> f220:  20 de
ENTERDONE   PULX                    ; Restore X
>> f222:  38
            PULB                    ; Restore B
>> f223:  33
            PULA                    ; Restore A
>> f224:  32
            RTS                     ; Done
>> f225:  39


*//////////////////////////////
*// Get input buffer address //
*//////////////////////////////
INBUFFADDR  LDX     #INBUFF
>> f226:  ce d0 04
            RTS
>> f229:  39


*//////////////////////////////
*// Get input buffer address //
*//////////////////////////////
INBUFFLENADDR   LDX     #INBUFFLEN
>> f22a:  ce d0 03
                RTS
>> f22d:  39


*//////////////////////////////////
*// Convert text to upper case   //
*// IN: A = character to convert //
*//////////////////////////////////
UPCASE      CMPA    #'a'            ; If ASCII digit isn't a character, skip
>> f22e:  81 61
            BLO     UPCASEDONE      ;
>> f230:  25 06
            CMPA    #'z'            ;
>> f232:  81 7a
            BHI     UPCASEDONE      ;
>> f234:  22 02
            SUBA    #$20            ; Convert lower case to upper case
>> f236:  80 20
UPCASEDONE  RTS                     ; Done
>> f238:  39


*//////////////////////////////////////////
*// Convert text to binary               //
*// IN:  LDX = input string (two digits) //
*// OUT: A   = converted byte value      //
*//////////////////////////////////////////
ASCII2HEX   PSHX                    ; Save X
>> f239:  3c
            LDAA    0,X             ;
>> f23a:  a6 00
            JSR     UPCASE          ; If character, convert to uppercase
>> f23c:  bd f2 2e
            CMPA    #$41            ; Is digit a character?
>> f23f:  81 41
            BLO     ISNUMBER        ; No - assume number
>> f241:  25 04
ISALPHA     SUBA    #$37            ; Convert character to hex value
>> f243:  80 37
            BRA     ASCII1          ;
>> f245:  20 02
ISNUMBER    SUBA    #$30            ; Convert number to hex value
>> f247:  80 30
ASCII1      ANDA    #$0F            ; Mask upper nibble bits
>> f249:  84 0f
            LSLA                    ; First digit is upper nibble - shift
>> f24b:  48
            LSLA                    ;
>> f24c:  48
            LSLA                    ;
>> f24d:  48
            LSLA                    ;
>> f24e:  48
            STAA    TEMP            ; Save number in temporary variable
>> f24f:  b7 d0 00
NEXTDIGIT   INX                     ; Move to second digit
>> f252:  08
            LDAA    0,X             ;
>> f253:  a6 00
            JSR     UPCASE          ; If character, convert to uppercase
>> f255:  bd f2 2e
            CMPA    #$41            ; Is digit a character?
>> f258:  81 41
            BLO     ISNUMBER2       ; No - assume number
>> f25a:  25 04
ISALPHA2    SUBA    #$37            ; Convert character to hex value
>> f25c:  80 37
            BRA     ASCII2          ;
>> f25e:  20 02
ISNUMBER2   SUBA    #$30            ; Convert number to hex value
>> f260:  80 30
ASCII2      ANDA    #$0F            ; Mask upper nibble bits
>> f262:  84 0f
            ORAA    TEMP            ; Or in upper nibble bits
>> f264:  ba d0 00
            PULX                    ; Restore X
>> f267:  38
            RTS                     ; Done
>> f268:  39


*//////////////////////////
*// Dump memory contents //
*//////////////////////////
DUMPMEM     PSHA                    ; Save A
>> f269:  36
            PSHB                    ; Save B
>> f26a:  37
            PSHX                    ; Save X
>> f26b:  3c
            LDX     #DUMPSTR        ; Load dump text
>> f26c:  ce f4 2e
            JSR     SERWRITESTR     ;
>> f26f:  bd f0 6a
            LDAA    #0              ; Zero counter
>> f272:  86 00
            STAA    TEMP
>> f274:  b7 d0 00
            LDX     #RECBUFF        ; Load receive buffer base
>> f277:  ce d0 55
DUMPMEM1    LDAA    #0
>> f27a:  86 00
DUMPLOOP    LDAB    0,X             ; Load byte from receive buffer
>> f27c:  e6 00
            JSR     SERPRTBYTE      ; Print byte in ascii to SCI
>> f27e:  bd f2 ad
            JSR     SERPRTSPACE     ; Print space
>> f281:  bd f2 c5
            INX                     ; Next byte
>> f284:  08
            INCA                    ; Increment byte count
>> f285:  4c
            CMPA    #$10
>> f286:  81 10
            BNE     DUMPLOOP        ;
>> f288:  26 f2
            JSR     SERPRTCR        ; Print CR to SCI
>> f28a:  bd f2 b8
            ADDA    TEMP
>> f28d:  bb d0 00
            STAA    TEMP
>> f290:  b7 d0 00
            CMPA    #$A0
>> f293:  81 a0
            BLO     DUMPMEM1
>> f295:  25 e3
DUMPDONE    PULX                    ; Restore X
>> f297:  38
            PULB                    ; Restore B
>> f298:  33
            PULA                    ; Restore A
>> f299:  32
            RTS                     ; Done
>> f29a:  39


*/////////////////////////////////////////////
*// Convert from binary to ASCII and output //
*// IN: B = byte to convert                 //
*/////////////////////////////////////////////
OUTLHLF     LSRB                    ; Shift data to right
>> f29b:  54
            LSRB
>> f29c:  54
            LSRB
>> f29d:  54
            LSRB
>> f29e:  54
OUTRHLF     ANDB    #$0F            ; Mask top half
>> f29f:  c4 0f
            ADDB    #$30            ; Convert to ascii
>> f2a1:  cb 30
            CMPB    #$39
>> f2a3:  c1 39
            BLE     OUTA            ; Jump if 0-9
>> f2a5:  2f 02
            ADDB    #$07            ; Convert to hex A-F
>> f2a7:  cb 07
OUTA        JSR     SERWRITECHR     ; Output character
>> f2a9:  bd f0 79
            RTS
>> f2ac:  39


*////////////////////////////////
*// Print byte to serial port  //
*// IN: B = byte to be printed //
*////////////////////////////////
SERPRTBYTE  PSHB
>> f2ad:  37
            PSHB
>> f2ae:  37
            JSR     OUTLHLF         ; Output left half
>> f2af:  bd f2 9b
            PULB                    ; Retrieve copy
>> f2b2:  33
            JSR     OUTRHLF         ; Output right half
>> f2b3:  bd f2 9f
            PULB
>> f2b6:  33
            RTS
>> f2b7:  39


*//////////////////////////////////////////
*// Print carriage return to serial port //
*//////////////////////////////////////////
SERPRTCR    PSHB
>> f2b8:  37
            LDAB    #CR
>> f2b9:  c6 0d
            JSR     SERWRITECHR
>> f2bb:  bd f0 79
            LDAB    #LF
>> f2be:  c6 0a
            JSR     SERWRITECHR
>> f2c0:  bd f0 79
            PULB
>> f2c3:  33
            RTS
>> f2c4:  39


*////////////////////////////////
*// Print space to serial port //
*////////////////////////////////
SERPRTSPACE PSHB
>> f2c5:  37
            LDAB    #SPACE
>> f2c6:  c6 20
            JSR     SERWRITECHR
>> f2c8:  bd f0 79
            PULB
>> f2cb:  33
            RTS
>> f2cc:  39


*///////////////////////////////////////////////////////
*// String compare routine                            //
*//                                                   //
*// IN: X = Pointer to string #1                      //
*// IN: Y = Pointer to string #2                      //
*// OUT: A = compare status (0 = match, 1 = NO match) //
*///////////////////////////////////////////////////////
STRCMP      PSHB                    ; Save B
>> f2cd:  37
            PSHX                    ; Save X
>> f2ce:  3c
            PSHY                    ; Save Y
>> f2cf:  18 3c
            LDAB    #0
>> f2d1:  c6 00
SCMPLOOP    LDAA    0,X             ; Load character from first string
>> f2d3:  a6 00
            CMPA    #0              ; NULL?
>> f2d5:  81 00
            BEQ     SCMPLPDONE      ;
>> f2d7:  27 0b
            CMPA    0,Y             ; Compare characters
>> f2d9:  18 a1 00
            BNE     SCMPLPDONE      ; No match
>> f2dc:  26 06
            INCB                    ;
>> f2de:  5c
            INX                     ; Next character
>> f2df:  08
            INY                     ; 
>> f2e0:  18 08
            BRA     SCMPLOOP        ; Check next character
>> f2e2:  20 ef
SCMPLPDONE  LDAA    0,X             ; If end of string - consider it a match
>> f2e4:  a6 00
            BEQ     SCMPMATCH       ;
>> f2e6:  27 09
            LDAA    0,Y             ;
>> f2e8:  18 a6 00
            BEQ     SCMPMATCH       ;
>> f2eb:  27 04
SCMPNOMATCH LDAA    #1              ; No match
>> f2ed:  86 01
            BRA     SCMPDONE        ;
>> f2ef:  20 06
SCMPMATCH   CMPB    #0              ;
>> f2f1:  c1 00
            BEQ     SCMPNOMATCH     ;
>> f2f3:  27 f8
            LDAA    #0              ;
>> f2f5:  86 00
SCMPDONE    PULY                    ; Restore Y
>> f2f7:  18 38
            PULX                    ; Restore X
>> f2f9:  38
            PULB                    ; Restore X
>> f2fa:  33
            RTS                     ; Done
>> f2fb:  39


*//////////////////////
*/ Clear LCD display //
*//////////////////////
LCD_CLEAR   PSHA                    ; Save A
>> f2fc:  36
            LDAA    #$1             ; Clear all display and return cursor home
>> f2fd:  86 01
            STAA    DISPC           ;
>> f2ff:  b7 c0 00
            JSR     LCD_WAIT        ;
>> f302:  bd f3 8c
            PULA                    ; Restore A
>> f305:  32
            RTS                     ; Done
>> f306:  39

*///////////////////////////////////////////////////////
*// Simple ms delay routine                           //
*//                                                   //
*// IN: A = Number of ms to delay (1-255)             //
*//                                                   //
*// NOTE: *** Assumes E=2MHz ***                      //
*///////////////////////////////////////////////////////
DELAY_MS    PSHX
>> f307:  3c
            LDX     #328
>> f308:  ce 01 48
            BRA     _DELAY_MS_1
>> f30b:  20 03
_DELAY_MS_0
            LDX     #332            ; 332*6 = 1992 cycles
>> f30d:  ce 01 4c
_DELAY_MS_1
            DEX
>> f310:  09
            BNE     _DELAY_MS_1
>> f311:  26 fd
            DECA
>> f313:  4a
            BNE     _DELAY_MS_0
>> f314:  26 f7
            PULX
>> f316:  38
            RTS
>> f317:  39

*///////////////////////////////////////////////////////
*// Initialize LCD display                            //
*//                                                   //
*// NOTE: The following sequence was taken from the   //
*//       Optrex LCD manual.                          //
*///////////////////////////////////////////////////////
LCD_INIT    PSHA                    ; Save A
>> f318:  36
            LDAA    #15             ; Wait 15ms to give Vcc time to stabilize
>> f319:  86 0f
            JSR     DELAY_MS        ;
>> f31b:  bd f3 07
            LDAA    #$38            ; Interface len=8, 2 lines, 5x7 font
>> f31e:  86 38
            STAA    DISPC           ;
>> f320:  b7 c0 00
            LDAA    #5              ; Wait 5ms
>> f323:  86 05
            JSR     DELAY_MS        ;
>> f325:  bd f3 07
            LDAA    #$38            ; REPEAT: Interface len=8, 2 lines, 5x7 font
>> f328:  86 38
            STAA    DISPC           ;
>> f32a:  b7 c0 00
            LDAA    #1              ; Wait 1ms
>> f32d:  86 01
            JSR     DELAY_MS        ;
>> f32f:  bd f3 07
            LDAA    #$38            ; REPEAT: Interface len=8, 2 lines, 5x7 font
>> f332:  86 38
            STAA    DISPC           ;
>> f334:  b7 c0 00
            JSR     LCD_WAIT        ; Busy Flag can now be checked
>> f337:  bd f3 8c
            LDAA    #$38            ; Interface len=8, 2 lines, 5x7 font
>> f33a:  86 38
            STAA    DISPC           ;
>> f33c:  b7 c0 00
            JSR     LCD_WAIT        ;
>> f33f:  bd f3 8c
            LDAA    #$C             ; Display on, cursor off, cursor blink off
>> f342:  86 0c
            STAA    DISPC           ;
>> f344:  b7 c0 00
            JSR     LCD_WAIT        ;
>> f347:  bd f3 8c
            LDAA    #$1             ; Clear all display and return cursor home
>> f34a:  86 01
            STAA    DISPC           ;
>> f34c:  b7 c0 00
            LDAA    #2              ; Wait 2ms
>> f34f:  86 02
            JSR     DELAY_MS        ;
>> f351:  bd f3 07
            JSR     LCD_WAIT        ;
>> f354:  bd f3 8c
            LDAA    #$6             ; Increment cursor and don't shift display
>> f357:  86 06
            STAA    DISPC           ;
>> f359:  b7 c0 00
            JSR     LCD_WAIT        ;
>> f35c:  bd f3 8c
            LDAA    #$80            ; Set DDRAM address to position 0.
>> f35f:  86 80
            STAA    DISPC           ;
>> f361:  b7 c0 00
            JSR     LCD_WAIT        ;
>> f364:  bd f3 8c
            PULA                    ; Restore A
>> f367:  32
            RTS                     ; Done
>> f368:  39


*//////////////////////
*// Write LCD string //
*//////////////////////
LCDWRITESTR PSHX                    ; Save X
>> f369:  3c
            PSHB                    ; Save B
>> f36a:  37
LCDSTRLOOP  LDAB    0,X             ; Get next character to be written
>> f36b:  e6 00
            BEQ     LCDSTRDONE      ; If NULL terminator, done
>> f36d:  27 06
            JSR     LCDWRITECHR     ; Write character to SCI
>> f36f:  bd f3 78
            INX                     ; Next character
>> f372:  08
            BRA     LCDSTRLOOP      ; Continue
>> f373:  20 f6
LCDSTRDONE  PULB                    ; Restore B
>> f375:  33
            PULX                    ; Restore X
>> f376:  38
            RTS                     ; Done
>> f377:  39


*/////////////////////////
*// Write LCD character //
*/////////////////////////
LCDWRITECHR PSHB                    ; Save B
>> f378:  37
            CMPB    #CR             ; Don't do CRs
>> f379:  c1 0d
            BEQ     LCDWRTDONE      ;
>> f37b:  27 0d
            CMPB    #LF             ; Don't do LFs
>> f37d:  c1 0a
            BEQ     LCDWRTDONE      ;
>> f37f:  27 09
            JSR     LCD_WAIT        ; Make sure LCD isn't doing anything
>> f381:  bd f3 8c
            STAB    DISPD           ; Write character to display
>> f384:  f7 c0 01
            JSR     LCD_WAIT        ;
>> f387:  bd f3 8c
LCDWRTDONE  PULB                    ; Restore B
>> f38a:  33
            RTS                     ; Done
>> f38b:  39


*///////////////////
*// LCD wait loop //
*///////////////////
LCD_WAIT    PSHA                    ; Save A
>> f38c:  36
LCDWTLOOP   LDAA    DISPC           ; Wait for LCD to swallow last request
>> f38d:  b6 c0 00
            ANDA    #$80            ;
>> f390:  84 80
            BNE     LCDWTLOOP       ; Not ready to take new data
>> f392:  26 f9
            PULA                    ; Restore A
>> f394:  32
            RTS                     ; Done
>> f395:  39


*/////////////////////////////////////////
*// SWI handler                         //
*// IN: A = command id to be executed   //
*/////////////////////////////////////////
SWIHDLR     PSHA                    ; Save A
>> f396:  36
            PSHB                    ; Save B
>> f397:  37
            PSHX                    ; Save X
>> f398:  3c
            PSHY                    ; Save Y
>> f399:  18 3c
            LDX     #BIOSCMDTBL     ; Load BIOS command table base
>> f39b:  ce f7 77
            PSHA                    ; Copy A -> B
>> f39e:  36
            PULB                    ;
>> f39f:  33
            LSLB                    ; Each offset is 2 bytes (multiply by 2)
>> f3a0:  58
            ABX                     ; Add B to X
>> f3a1:  3a
            LDY     0,X             ; Get routine address
>> f3a2:  1a ee 00
            STY     BIOSRTN         ; Store routine address
>> f3a5:  18 ff d0 01
            PULY                    ; Restore Y
>> f3a9:  18 38
            PULX                    ; Restore X
>> f3ab:  38
            PULB                    ; Restore B
>> f3ac:  33
            PULA                    ; Restore A
>> f3ad:  32
            JSR     BIOSRTN         ; Call BIOS routine
>> f3ae:  bd d0 01
            RTI                     ; Done
>> f3b1:  3b


*////////////////////
*//  Text strings  //
*////////////////////

*// Welcome string
WELCOMESTR  FCB CR
>> f3b2:  0d
            FCB LF
>> f3b3:  0a
            FCC "68MICRO Ver 0.3"
>> f3b4:  36 38 4d 49 43 52 4f 20 56 65 72 20 30 2e 33 
            FCB CR
>> f3c3:  0d
            FCB LF
>> f3c4:  0a
            FCB 0
>> f3c5:  00
*// Copyright string
COPYRGHTSTR FCC "Copyright (c) 2011, Jeff Glaum.  All rights reserved."
>> f3c6:  43 6f 70 79 72 69 67 68 74 20 28 63 29 20 32 30 31 31 2c 20 4a 65 66 66 20 47 6c 61 75 6d 2e 20 20 41 6c 6c 20 72 69 67 68 74 73 20 72 65 73 65 72 76 65 64 2e 
            FCB CR
>> f3fb:  0d
            FCB LF
>> f3fc:  0a
            FCB 0
>> f3fd:  00

SREC_S1HDR  FCC "S1"
>> f3fe:  53 31 
            FCB 0
>> f400:  00

SREC_S9HDR  FCC "S9"
>> f401:  53 39 
            FCB 0
>> f403:  00

*// User commands
COMMANDS    FCC "srec"
>> f404:  73 72 65 63 
            FCB 0
>> f408:  00
            FCC "dump"
>> f409:  64 75 6d 70 
            FCB 0
>> f40d:  00
            FCC "enter"
>> f40e:  65 6e 74 65 72 
            FCB 0    
>> f413:  00
            FCC "go"
>> f414:  67 6f 
            FCB 0    
>> f416:  00
            FCC "reset"
>> f417:  72 65 73 65 74 
            FCB 0    
>> f41c:  00
            FCC "?"
>> f41d:  3f 
            FCB 0    
>> f41e:  00
            FCC "help"
>> f41f:  68 65 6c 70 
            FCB 0    
>> f423:  00
            FCC "biosrtns"
>> f424:  62 69 6f 73 72 74 6e 73 
            FCB 0                   ; Must end in two NULLs
>> f42c:  00
            FCB 0
>> f42d:  00

DUMPSTR     FCB CR
>> f42e:  0d
            FCB LF
>> f42f:  0a
            FCC "Memory contents:"
>> f430:  4d 65 6d 6f 72 79 20 63 6f 6e 74 65 6e 74 73 3a 
            FCB CR
>> f440:  0d
            FCB LF
>> f441:  0a
            FCB 0
>> f442:  00

ENTERSTR    FCB CR
>> f443:  0d
            FCB LF
>> f444:  0a
            FCC "Enter each byte followed by return.  To finish press return twice."
>> f445:  45 6e 74 65 72 20 65 61 63 68 20 62 79 74 65 20 66 6f 6c 6c 6f 77 65 64 20 62 79 20 72 65 74 75 72 6e 2e 20 20 54 6f 20 66 69 6e 69 73 68 20 70 72 65 73 73 20 72 65 74 75 72 6e 20 74 77 69 63 65 2e 
            FCB CR
>> f487:  0d
            FCB LF
>> f488:  0a
            FCB 0
>> f489:  00

*// Resonse strings
UNKWNCMDSTR FCB CR
>> f48a:  0d
            FCB LF
>> f48b:  0a
            FCC "Invalid command"
>> f48c:  49 6e 76 61 6c 69 64 20 63 6f 6d 6d 61 6e 64 
            FCB CR
>> f49b:  0d
            FCB LF
>> f49c:  0a
            FCB 0
>> f49d:  00

*// Resonse string
PROMPTSTR   FCC "> "
>> f49e:  3e 20 
            FCB 0
>> f4a0:  00
         
*// Echo string
ECHOSTR     FCC "You typed: "
>> f4a1:  59 6f 75 20 74 79 70 65 64 3a 20 
            FCB 0
>> f4ac:  00
 
*// Ready to receive string
RECEIVESTR  FCB CR
>> f4ad:  0d
            FCB LF
>> f4ae:  0a
            FCC "Ready to receive S-Record data from host..."
>> f4af:  52 65 61 64 79 20 74 6f 20 72 65 63 65 69 76 65 20 53 2d 52 65 63 6f 72 64 20 64 61 74 61 20 66 72 6f 6d 20 68 6f 73 74 2e 2e 2e 
            FCB 0
>> f4da:  00

DOWNLOADSTR FCC "Downloading..."
>> f4db:  44 6f 77 6e 6c 6f 61 64 69 6e 67 2e 2e 2e 
            FCB 0
>> f4e9:  00

JUMPINGSTR  FCC "Jumping..."
>> f4ea:  4a 75 6d 70 69 6e 67 2e 2e 2e 
            FCB 0
>> f4f4:  00

SRECERRSTR  FCC "S-Record Error!"
>> f4f5:  53 2d 52 65 63 6f 72 64 20 45 72 72 6f 72 21 
            FCB 0
>> f504:  00

*// Help text
HELPTEXT    FCB CR
>> f505:  0d
            FCB LF
>> f506:  0a
            FCC "Commands:"
>> f507:  43 6f 6d 6d 61 6e 64 73 3a 
            FCB CR
>> f510:  0d
            FCB LF
>> f511:  0a
            FCC "srec     - Weezer receives & executes S-Record code""
>> f512:  73 72 65 63 20 20 20 20 20 2d 20 57 65 65 7a 65 72 20 72 65 63 65 69 76 65 73 20 26 20 65 78 65 63 75 74 65 73 20 53 2d 52 65 63 6f 72 64 20 63 6f 64 65 
            FCB CR
>> f545:  0d
            FCB LF
>> f546:  0a
            FCC "dump     - dumps contents of receive buffer"
>> f547:  64 75 6d 70 20 20 20 20 20 2d 20 64 75 6d 70 73 20 63 6f 6e 74 65 6e 74 73 20 6f 66 20 72 65 63 65 69 76 65 20 62 75 66 66 65 72 
            FCB CR
>> f572:  0d
            FCB LF
>> f573:  0a
            FCC "enter    - enters contents of receive buffer"
>> f574:  65 6e 74 65 72 20 20 20 20 2d 20 65 6e 74 65 72 73 20 63 6f 6e 74 65 6e 74 73 20 6f 66 20 72 65 63 65 69 76 65 20 62 75 66 66 65 72 
            FCB CR
>> f5a0:  0d
            FCB LF
>> f5a1:  0a
            FCC "go       - executes contents of receive buffer"
>> f5a2:  67 6f 20 20 20 20 20 20 20 2d 20 65 78 65 63 75 74 65 73 20 63 6f 6e 74 65 6e 74 73 20 6f 66 20 72 65 63 65 69 76 65 20 62 75 66 66 65 72 
            FCB CR
>> f5d0:  0d
            FCB LF
>> f5d1:  0a
            FCC "reset    - resets system"
>> f5d2:  72 65 73 65 74 20 20 20 20 2d 20 72 65 73 65 74 73 20 73 79 73 74 65 6d 
            FCB CR
>> f5ea:  0d
            FCB LF
>> f5eb:  0a
            FCC "?/help   - displays this text"
>> f5ec:  3f 2f 68 65 6c 70 20 20 20 2d 20 64 69 73 70 6c 61 79 73 20 74 68 69 73 20 74 65 78 74 
            FCB CR
>> f609:  0d
            FCB LF
>> f60a:  0a
            FCB 0
>> f60b:  00

BIOSRTNSTXT FCB CR
>> f60c:  0d
            FCB LF
>> f60d:  0a
            FCC "1: SERCLRINPUT"
>> f60e:  31 3a 20 53 45 52 43 4c 52 49 4e 50 55 54 
            FCB CR
>> f61c:  0d
            FCB LF
>> f61d:  0a
            FCC "2: CLRINSTRING"
>> f61e:  32 3a 20 43 4c 52 49 4e 53 54 52 49 4e 47 
            FCB CR
>> f62c:  0d
            FCB LF
>> f62d:  0a
            FCC "3: SERIAL_INIT"
>> f62e:  33 3a 20 53 45 52 49 41 4c 5f 49 4e 49 54 
            FCB CR
>> f63c:  0d
            FCB LF
>> f63d:  0a
            FCC "4: SERWRITESTR"
>> f63e:  34 3a 20 53 45 52 57 52 49 54 45 53 54 52 
            FCB CR
>> f64c:  0d
            FCB LF
>> f64d:  0a
            FCC "5: SERWRITECHR"
>> f64e:  35 3a 20 53 45 52 57 52 49 54 45 43 48 52 
            FCB CR
>> f65c:  0d
            FCB LF
>> f65d:  0a
            FCC "6: SERREADSTR"
>> f65e:  36 3a 20 53 45 52 52 45 41 44 53 54 52 
            FCB CR
>> f66b:  0d
            FCB LF
>> f66c:  0a
            FCC "7: SERREADCHR"
>> f66d:  37 3a 20 53 45 52 52 45 41 44 43 48 52 
            FCB CR
>> f67a:  0d
            FCB LF
>> f67b:  0a
            FCC "8: PARSECMDSTR"
>> f67c:  38 3a 20 50 41 52 53 45 43 4d 44 53 54 52 
            FCB CR
>> f68a:  0d
            FCB LF
>> f68b:  0a
            FCC "9: EXECUTECMD"
>> f68c:  39 3a 20 45 58 45 43 55 54 45 43 4d 44 
            FCB CR
>> f699:  0d
            FCB LF
>> f69a:  0a
            FCC "A: ENTERMEM"
>> f69b:  41 3a 20 45 4e 54 45 52 4d 45 4d 
            FCB CR
>> f6a6:  0d
            FCB LF
>> f6a7:  0a
            FCC "B: UPCASE"
>> f6a8:  42 3a 20 55 50 43 41 53 45 
            FCB CR
>> f6b1:  0d
            FCB LF
>> f6b2:  0a
            FCC "C: ASCII2HEX"
>> f6b3:  43 3a 20 41 53 43 49 49 32 48 45 58 
            FCB CR
>> f6bf:  0d
            FCB LF
>> f6c0:  0a
            FCC "D: DUMPMEM"
>> f6c1:  44 3a 20 44 55 4d 50 4d 45 4d 
            FCB CR
>> f6cb:  0d
            FCB LF
>> f6cc:  0a
            FCC "E: SERPRTBYTE"
>> f6cd:  45 3a 20 53 45 52 50 52 54 42 59 54 45 
            FCB CR
>> f6da:  0d
            FCB LF
>> f6db:  0a
            FCC "F: SERPRTCR"
>> f6dc:  46 3a 20 53 45 52 50 52 54 43 52 
            FCB CR
>> f6e7:  0d
            FCB LF
>> f6e8:  0a
            FCC "10: SERPRTSPACE"
>> f6e9:  31 30 3a 20 53 45 52 50 52 54 53 50 41 43 45 
            FCB CR
>> f6f8:  0d
            FCB LF
>> f6f9:  0a
            FCC "11: STRCMP"
>> f6fa:  31 31 3a 20 53 54 52 43 4d 50 
            FCB CR
>> f704:  0d
            FCB LF
>> f705:  0a
            FCC "12: LCD_INIT"
>> f706:  31 32 3a 20 4c 43 44 5f 49 4e 49 54 
            FCB CR
>> f712:  0d
            FCB LF
>> f713:  0a
            FCC "13: LCD_CLEAR"
>> f714:  31 33 3a 20 4c 43 44 5f 43 4c 45 41 52 
            FCB CR
>> f721:  0d
            FCB LF
>> f722:  0a
            FCC "14: LCDWRITESTR"
>> f723:  31 34 3a 20 4c 43 44 57 52 49 54 45 53 54 52 
            FCB CR
>> f732:  0d
            FCB LF
>> f733:  0a
            FCC "15: LCDWRITECHR"
>> f734:  31 35 3a 20 4c 43 44 57 52 49 54 45 43 48 52 
            FCB CR
>> f743:  0d
            FCB LF
>> f744:  0a
            FCC "16: LCD_WAIT"
>> f745:  31 36 3a 20 4c 43 44 5f 57 41 49 54 
            FCB CR
>> f751:  0d
            FCB LF
>> f752:  0a
            FCC "17: INBUFFADDR"
>> f753:  31 37 3a 20 49 4e 42 55 46 46 41 44 44 52 
            FCB CR
>> f761:  0d
            FCB LF
>> f762:  0a
            FCC "18: INBUFFLENADDR"
>> f763:  31 38 3a 20 49 4e 42 55 46 46 4c 45 4e 41 44 44 52 
            FCB CR
>> f774:  0d
            FCB LF
>> f775:  0a
            FCB 0
>> f776:  00

*// Command table
BIOSCMDTBL  EQU *
            FDB 0000            ; 0
>> f777:  0000
            FDB SERCLRINPUT     ; 1
>> f779:  f03e
            FDB CLRINSTRING     ; 2
>> f77b:  f04d
            FDB SERIAL_INIT     ; 3
>> f77d:  f058
            FDB SERWRITESTR     ; 4
>> f77f:  f06a
            FDB SERWRITECHR     ; 5
>> f781:  f079
            FDB SERREADSTR      ; 6
>> f783:  f086
            FDB SERREADCHR      ; 7
>> f785:  f0c0
            FDB PARSECMDSTR     ; 8
>> f787:  f0d2
            FDB EXECUTECMD      ; 9
>> f789:  f108
            FDB ENTERMEM        ; A
>> f78b:  f1f4
            FDB UPCASE          ; B
>> f78d:  f22e
            FDB ASCII2HEX       ; C
>> f78f:  f239
            FDB DUMPMEM         ; D
>> f791:  f269
            FDB SERPRTBYTE      ; E
>> f793:  f2ad
            FDB SERPRTCR        ; F
>> f795:  f2b8
            FDB SERPRTSPACE     ; 10
>> f797:  f2c5
            FDB STRCMP          ; 11
>> f799:  f2cd
            FDB LCD_INIT        ; 12
>> f79b:  f318
            FDB LCD_CLEAR       ; 13
>> f79d:  f2fc
            FDB LCDWRITESTR     ; 14
>> f79f:  f369
            FDB LCDWRITECHR     ; 15
>> f7a1:  f378
            FDB LCD_WAIT        ; 16
>> f7a3:  f38c
            FDB INBUFFADDR      ; 17
>> f7a5:  f226
            FDB INBUFFLENADDR   ; 18
>> f7a7:  f22a

*///////////////////////
*// Interrupt vectors //
*///////////////////////
            ORG $FFF6           ; SWI Vector
SWI_VECT    FDB SWIHDLR 
>> fff6:  f396

            ORG $FFFE           ; Reset vector
RESET_VECT  FDB WEEZER
>> fffe:  f000
