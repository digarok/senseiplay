P8_MLI_CALL       =     $bf00
P8_OPEN_BUFFER    =     $800                    ; @todo: allocate  ..  buffer for open file being accessed
P8_DATA_BUFFER    =     $D00                    ; @todo: allocate  ..  buffer for file read data
P8_DATA_BUFFER_SZ =     $1000                   ;
PT_DST_PTR        =     $00                     ; DP location used for copies
P8_ERR_BUF_IN_USE =     $56                     ; when the specified buffer is marked in-use by Prodos system table


********************************************************** OPEN ($C8)
P8_OPEN           =     $C8
P8_OPEN_PCNT      =     3
*       +-----------------------+
*     0 | Number of Parms (3)   |
*       +-----------------------+-----------------------+
*    +1 | Pointer to Pathname                           |
*       +-----------------------------------------------+
*    +3 | Pointer to Buffer                             |
*       +-----------------------+-----------------------+
*    +5 | Reference Number      |  <- result
*       +-----------------------+

********************************************************** READ ($CA)
P8_READ           =     $CA
P8_READ_PCNT      =     4
*       +-----------------------+
*     0 | Number of Parms (4)   |
*       +-----------------------+
*    +1 | Reference Number      |
*       +-----------------------------------------------+
*    +2 | Pointer to Data Buffer                        |
*       +-----------------------+-----------------------+
*    +4 | Requested Bytes                               |
*       +-----------------------+-----------------------+
*    +6 | Returned Bytes                                |  <- result
*       +-----------------------+-----------------------+


********************************************************** CLOSE ($CC)
P8_CLOSE          =     $CC
P8_CLOSE_PCNT     =     1
*       +-----------------------+
*     0 | Number of Parms (1)   |
*       +-----------------------+
*    +1 | Reference Number      |
*       +-----------------------+


**** MACROS ****

*PT_LoadFilenameToPtr 'ntpplayer';0     ; address in 0
* ]1 = pathname
* ]2 = DP pointer           <- points to destination
PT_LoadFilenameToPtr MAC
                  mx    %00
                  lda   ]2
                  sta   PT_DST_PTR
                  lda   ]2+2
                  sta   PT_DST_PTR+2
                  lda   #_pathname

                  jsr   PT_LoadFile
                  bra   _done
_pathname         str   ]1
_done
                  <<<

* a=#pathname
PT_LoadFile       mx    %00
                  sta   _PT_PARMTABLE+1
                                                ; ------------------- OPEN ---
                  sep   #$30
                  lda   #P8_OPEN                ; set up OPEN call
                  sta   PT_P8CALL_NUM
                  lda   #P8_OPEN_PCNT           ; build OPEN parm table
                  sta   _PT_PARMTABLE
                  rep   #$30
                  lda   #P8_OPEN_BUFFER
                  sta   _PT_PARMTABLE+3         ; done setting up OPEN
                  jsr   PT_P8CALL               ; OPEN file
                  sep   #$30
                  lda   _PT_PARMTABLE+5         ; get REFNUM
                  sta   _PT_REFNUM              ; store REFNUM for OPEN file

                                                ; ------------------- READ ---
                  sta   _PT_PARMTABLE+1         ; store REFNUM to READ file
                  lda   #P8_READ                ; set up READ call
                  sta   PT_P8CALL_NUM
                  lda   #P8_READ_PCNT           ; build READ parm table
                  sta   _PT_PARMTABLE
                  rep   #$30                    ; 16-bit
                  lda   #P8_DATA_BUFFER
                  sta   _PT_PARMTABLE+2
                  lda   #P8_DATA_BUFFER_SZ      ; always a full read, check returned bytes for EOF
                  sta   _PT_PARMTABLE+4         ; done setting up READ

:read_loop        jsr   PT_P8CALL               ; READ file
                  lda   _PT_PARMTABLE+6         ; to check returned number of bytes to see if EOF
                  beq   :eof_read_zero_bytes    ; this will also catch a buffer aligned file on final read
                  sta   :eof_check+1

:copy_to_dest                                   ; copy buffer to real destination
                  ldy   #0
:copy_word        lda   P8_DATA_BUFFER,Y
                  stal  [PT_DST_PTR],Y
                  iny
                  iny
:eof_check        cpy   #0                      ; <- eof_check (SMC)
                  bcc   :copy_word
                  cpy   #P8_DATA_BUFFER_SZ
                  bne   :eof_returned_bytes
                  lda   PT_DST_PTR
                  clc
                  adc   #P8_DATA_BUFFER_SZ
                  sta   PT_DST_PTR
                  bcc   :read_loop
                  inc   PT_DST_PTR+2            ; bank++
                  bra   :read_loop

:eof_returned_bytes

:eof_read_zero_bytes


                                                ; ------------------- CLOSE --
                  sep   #$30
                  lda   #P8_CLOSE_PCNT
                  sta   _PT_PARMTABLE
                  lda   #P8_CLOSE               ; set up CLOSE call
                  sta   PT_P8CALL_NUM
                  rep   #$30
                  jsr   PT_P8CALL
                  rts


PT_P8CALL         sec                           ; normally called in 16-bit mode!
                  xce
                  jsr   P8_MLI_CALL
                  dfb   P8_OPEN
PT_P8CALL_NUM     =     *-1                     ; SMC
                  da    _PT_PARMTABLE
                  bne   :error
                  clc
                  xce
                  rep   #$30                    ; return in 16 bit mode!
                  rts
                  mx    %11
:error
                  brl   PT_P8_ERROR


** Really simple error code output.  I should probably make a real debug error at some point :P
PT_P8_ERROR
                  sec
                  xce
                  ldx   #$f0
                  stx   $c022
                  jsr   $fdda
                  jsr   $fd8e
                  wai                           ; HALT

** Reusable parm table
_PT_REFNUM        dw    0                       ; reusable reference number for a single open file (only uses a byte but padded for 16-bit code)
_PT_PATHNAME_PTR  da    0                       ; reusable pointer to pathname of current file being worked with
_PT_PARMTABLE     ds    16                      ; this is a reusable P8 parameter table to be filled out dynamically












******************************************
* ToolCall Macros                        *
******************************************
Tool              MAC
                  LDX   #]1
                  JSL   $E10000
                  <<<
_TLStartUp        MAC
                  Tool  $201
                  <<<
_TLShutDown       MAC
                  Tool  $301
                  <<<
_NewHandle        MAC
                  Tool  $902
                  <<<
_MMStartUp        MAC
                  Tool  $202
                  <<<
_GetMasterId      MAC
                  Tool  $2003
                  <<<
_MTStartUp        MAC
                  Tool  $203
                  <<<

_TLTextMountVol   MAC
                  Tool  $1201
                  <<<
_UnPackBytes      MAC
                  Tool  $2703
                  <<<

LoadLongXY        MAC
                  LDX   #^]1
                  LDY   #]1
                  <<<

PushLongXY        MAC
                  PHX
                  PHY
                  <<<

PushLong          MAC
                  IF    #=]1
                  PushWord #^]1
                  ELSE
                  PushWord ]1+2
                  FIN
                  PushWord ]1
                  <<<

PushWord          MAC
                  IF    #=]1
                  PEA   ]1
                  ELSE
                  IF    MX/2
                  LDA   ]1+1
                  PHA
                  FIN
                  LDA   ]1
                  PHA
                  FIN
                  <<<
Tool              MAC
                  LDX   #]1
                  JSL   $E10000
                  <<<


****************************************
* AllocOneBank                         *
* This is a custom allocation function *
* that makes use of the fact that we   *
* request an entire locked bank and so *
* simply returns the bank in the       *
* accumulator. (basically dereference  *
* the Handle to get the pointer)       *
****************************************
AllocOneBank      mx    %00
                  PushLong #0
                  PushLong #$10000
                  PushWord UserId
                  PushWord #%11000000_00011100
                  PushLong #0
                  _NewHandle                    ; returns LONG Handle on stack
                  plx                           ; base address of the new handle
                  pla                           ; high address 00XX of the new handle (bank)
                  xba                           ; swap accumulator bytes to XX00
                  sta   :bank+2                 ; store as bank for next op (overwrite $XX00)
:bank             ldal  $000001,X               ; recover the bank address in A=XX/00
                  rts



* X/Y = length in bytes (24bit)
AllocContiguousPageAlign mx %00
                  PushLong #0                   ; result space
                  PushLongXY
                  PushWord UserId
                  PushWord #%11000000_00001100

                  PushLong #0
                  _NewHandle                    ; returns LONG Handle on stack
                  _Err
                  plx                           ; base address of the new handle
                  pla                           ; high address 00XX of the new handle (bank)
                  xba                           ; swap accumulator bytes to XX00
                  sta   :bank+2                 ; store as bank for next op (overwrite $XX00)
:bank             ldal  $000001,X               ; recover the bank address in A=XX/00
                  rts


