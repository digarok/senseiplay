P8_MLI_CALL       =     $bf00
P8_OPEN_BUFFER    =     $800                    ; @todo: allocate  ..  buffer for open file being accessed
P8_DATA_BUFFER    =     $D00                    ; @todo: allocate  ..  buffer for file read data
P8_DATA_BUFFER_SZ =     $1000                   ;
PT_DST_PTR        =     $00                     ; DP location used for copies
PT_TMP_PTR        =     $04                     ; Use for internal routines like string handling
PT_DIR_ENTRY_PTR  =     $08                     ; point to current entry when scanning a directory
P8_ERR_BUF_IN_USE =     #$56                    ; when the specified buffer is marked in-use by Prodos system table
P8_ERR_EOF        =     #$4C                    ; hit End Of File (or directory)
PT_PREFIX_BUFFER  =     $1D00                   ; 256 byte area

P8_CROUT          =     $FC62
P8_CROUT2         =     $FD8E
P8_COUT           =     $FDED
P8_PRHEX          =     $FDDA

P8_DIR_ENT_OFFSET_TYPE = $10
P8_DIR_ENT_OFFSET_LEN = $15                     ; $15-17


********************************************************** ONLINE ($C5)
P8_ONLINE         =     $C5
P8_ONLINE_PCNT    =     2
*       +-----------------------+
*     0 | Number of Parms (2)   |
*       +-----------------------+
*    +1 | Unit Number Code      |
*       +-----------------------------------------------+
*    +3 | Pointer to Data Buffer                        |
*       +-----------------------+-----------------------+


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


********************************************************** SET_PREFIX ($C8)
P8_SET_PREFIX     =     $C6
P8_SET_PREFIX_PCNT =    1
*       +-----------------------+
*     0 | Number of Parms (1)   |
*       +-----------------------+-----------------------+
*    +1 | Pointer to Pathname                           |
*       +-----------------------+-----------------------+


********************************************************** GET_PREFIX ($C7)
P8_GET_PREFIX     =     $C7
P8_GET_PREFIX_PCNT =    1
*       +-----------------------+
*     0 | Number of Parms (1)   |
*       +-----------------------+-----------------------+
*    +1 | Pointer to Data Buffer                        |  <- result
*       +-----------------------+-----------------------+



********************************************************** SET_MARK ($CE)
P8_SET_MARK       =     $CE
P8_SET_MARK_PCNT  =     2
*       +-----------------------+
*     0 | Number of Parms (2)   |
*       +-----------------------+
*    +1 | Reference Number      |
*       +-----------------------+-----------------------+-----------------------+
*    +2 | Mark Position (3-byte ptr inside file)                                |
*       +-----------------------+-----------------------+-----------------------+







*************************** MACROS **********************************

* A=ptr to str (with prefix length byte)
* This sets the high bit for each character in the string before printing
PT_PrintProdosStr MAC
                  jsr   _PT_PrintProdosStr
                  <<<

_PT_PrintProdosStr mx   %00
                  sta   PT_TMP_PTR
                  sep   #$30
                  lda   [PT_TMP_PTR]            ; length byte
                  tax
                  ldy   #1                      ; pre-increment index past length byte
:prloop           lda   [PT_TMP_PTR],y          ; SMC
                  ora   #%1000_0000             ; make printable char
                  jsr   P8_COUT
                  iny
                  dex
                  bpl   :prloop
                  rep   #$30
                  rts



* Returns buffer location in A
PT_GetPrefix      MAC
                  jsr   _PT_GetPrefix
                  <<<

_PT_GetPrefix     mx    %00
                  lda   #PT_PREFIX_BUFFER       ; set result buffer
                  sta   _PT_PARMTABLE+1
                  sep   #$30
                  lda   #P8_GET_PREFIX          ; set up GET_PREFIX call
                  sta   PT_P8CALL_NUM
                  lda   #P8_GET_PREFIX_PCNT     ; build GET_PREFIX parm table
                  sta   _PT_PARMTABLE
                  jsr   PT_P8CALL               ; returns in emulation 8-bit mode
                  clc
                  xce
                  rep   #$30
                  lda   #PT_PREFIX_BUFFER
                  rts



PT_ReadOnline     MAC
                  jsr   _PT_ReadOnline
                  <<<

_PT_ReadOnline    mx    %00
                  stz   _PT_ReadDirCount        ; zero result count
                  lda   #P8_DATA_BUFFER         ; set result buffer
                  sta   _PT_PARMTABLE+2
                  sep   #$30
                  stz   _PT_PARMTABLE+1         ; zero unit number will return all
                  lda   #P8_ONLINE              ; set up GET_PREFIX call
                  sta   PT_P8CALL_NUM
                  lda   #P8_ONLINE_PCNT         ; build GET_PREFIX parm table
                  sta   _PT_PARMTABLE
                  jsr   PT_P8CALL               ; returns in emulation 8-bit mode

                  clc
                  xce
                  rep   $30
                  lda   #P8_DATA_BUFFER
                  sta   PT_TMP_PTR
                  sep   $30

:read_online_entry ldy  #0
                  lda   (PT_TMP_PTR),y
                  and   #$0f
                  beq   :no_more_entries

                  ldx   #$0F                    ; 16 bytes
:copy_entry
                  lda   (PT_TMP_PTR),y
                  sta   [PT_DST_PTR],y          ; this doesn't make sense but it doesn't matter either
                  iny
                  dex
                  bpl   :copy_entry
:inc_ptrs         lda   PT_TMP_PTR
                  clc
                  adc   #$10
                  sta   PT_TMP_PTR
                  lda   PT_DST_PTR
                  clc
                  adc   #DirListEntrySize
                  sta   PT_DST_PTR
                  rep   $30
                  inc   _PT_ReadDirCount
                  sep   $30
                  bra   :read_online_entry

:no_more_entries  rep   $30
                  lda   _PT_ReadDirCount
                  rts



* Returns buffer location in A
PT_SetPrefix      MAC
                  jsr   _PT_SetPrefix
                  <<<

_PT_SetPrefix     mx    %00
                  lda   #PT_PREFIX_BUFFER       ; set result buffer
                  sta   _PT_PARMTABLE+1
                  sep   #$30
                  lda   #P8_SET_PREFIX          ; set up SET_PREFIX call
                  sta   PT_P8CALL_NUM
                  lda   #P8_GET_PREFIX_PCNT     ; build SET_PREFIX parm table
                  sta   _PT_PARMTABLE
                  jsr   PT_P8CALL               ; returns in emulation 8-bit mode
                  clc
                  xce
                  rep   #$30
                  lda   #PT_PREFIX_BUFFER
                  rts


* return prefix length in x
PT_RemovePrefix   MAC
                  jsr   _PT_RemovePrefix
                  <<<

_PT_RemovePrefix  sep   $30
                  ldx   PT_PREFIX_BUFFER
                  cpx   #1
                  bcs   :remove
:too_short        rts
:remove           dex                           ; remove trailing slash
                  lda   PT_PREFIX_BUFFER,x
                  cmp   #'/'
                  beq   :slash
:no_slash
                  cpx   #1
                  bne   :remove
                  rep   $30
                  rts
:slash
:store_new_len    stx   PT_PREFIX_BUFFER
                  rep   $30
                  rts



PT_AppendPrefix   MAC
                  rep   $30
                  lda   ]1
                  jsr   _PT_AppendPrefix
                  <<<

* a = ptr to prodos string
_PT_AppendPrefix  mx    %00
                  sta   PT_TMP_PTR
                  sep   $30
                  ldy   #0
                  lda   (PT_TMP_PTR),y          ; length of append string
                  inc                           ; +1 skip len byte
                  sta   :copy_len+1             ; smc - this many bytes to copy

                  lda   PT_PREFIX_BUFFER        ; get length Byte
                  inc                           ; +1 skip len byte
                  tax                           ; to index

                  ldy   #0
                  lda   (PT_TMP_PTR),y          ; length of append string

                  clc
                  adc   PT_PREFIX_BUFFER        ; add to prefix len
                  sta   PT_PREFIX_BUFFER        ; now length in memory is updated (orig still in x)

                  ldy   #1
:copy_char        lda   (PT_TMP_PTR),y
                  sta   PT_PREFIX_BUFFER,x
                  iny
                  inx
:copy_len         cpy   #0                      ; smc
                  bne   :copy_char
                  rep   $30
                  rts

* PT_LoadFilePtrToPtr 0;4     ; ptr to str filename in 0; dest address in 4
* ]1 = DP pointer to pathname
* ]2 = DP pointer           <- points to destination
PT_LoadFilePtrToPtr MAC
                  mx    %00

                  ldx   ]1                      ; load first to prevent clobbering

                  lda   ]2
                  sta   PT_DST_PTR
                  lda   ]2+2
                  sta   PT_DST_PTR+2

                  txa                           ;restore


                  jsr   _PT_LoadFile
                  bra   _done
_done
                  <<<


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

                  jsr   _PT_LoadFile
                  bra   _done
_pathname         str   ]1
_done
                  <<<

* a=#pathname
_PT_LoadFile      mx    %00
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
                  beq   :eof_read_zero_bytes    ; this will also catch a buffer aligned file on final read, I think...
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

:eof_returned_bytes                             ;\___ these should both be okay
:eof_read_zero_bytes                            ;/
                                                ; ------------------- CLOSE --
                  sep   #$30
                  lda   #P8_CLOSE_PCNT
                  sta   _PT_PARMTABLE
                  lda   #P8_CLOSE               ; set up CLOSE call
                  sta   PT_P8CALL_NUM
                  rep   #$30
                  jsr   PT_P8CALL
                  rts


**** PT PRODOS MLI "DISPATCHER" ****
PT_P8CALL         sec                           ; normally called in 16-bit mode!
                  xce
                  jsr   P8_MLI_CALL
                  dfb   P8_OPEN
PT_P8CALL_NUM     =     *-1                     ; SMC
                  da    _PT_PARMTABLE
                  bne   :error
:return           clc
                  xce
                  rep   #$30                    ; return in 16 bit mode!
                  rts
                  mx    %11
:error            cmp   #P8_ERR_EOF
                  bne   :fatal
                  ldx   _PT_EXPECT_EOF
                  bne   :return                 ; EOF is ok
:fatal            brl   PT_P8_ERROR


** Really simple error code output.  I should probably make a real debug error at some point :P
PT_P8_ERROR
                  sec
                  xce
                  ldx   #$f0
                  stx   $c022
                  jsr   P8_PRHEX
                  jsr   P8_CROUT2
                  wai                           ; HALT






PT_SetDirListPtr  MAC
                  lda   #]1
                  sta   PT_DST_PTR
                  lda   #^]1
                  sta   PT_DST_PTR+2
                  <<<


* A=ptr to prefixpath
PT_ReadDir        MAC
                  jsr   _PT_ReadDir
                  <<<

_PT_ReadDirCount  dw    0
_PT_ReadDir       mx    %00

*           sta   _PT_PARMTABLE+1        ; ptr to prefix could be passed in....

                                                ; ------------------- OPEN ---
:init             clc
                  xce
                  rep   $30

                  if    NO_SEED_PARENT          ; now we actually start with a parent entry ("..")
                  stz   _PT_ReadDirCount
                  else
                  lda   #1
                  sta   _PT_ReadDirCount
                  lda   #2                      ; str len
                  ldy   #0
                  sta   [PT_DST_PTR],y
                  lda   #'.'
                  iny
                  sta   [PT_DST_PTR],y
                  iny
                  sta   [PT_DST_PTR],y
                  lda   #$0f                    ; type
                  ldy   #$10
                  sta   [PT_DST_PTR],y

                  lda   PT_DST_PTR              ; advance pty
                  clc
                  adc   #DirListEntrySize       ; @todo this is defined outside of this file, redefine locally.
                  sta   PT_DST_PTR              ; doesn't cross banks (16bit) so your buffer shouldn't either
                  fin

                  sep   $30
                  lda   #1
                  sta   _PT_EXPECT_EOF

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
                                                ; @todo: dedupe above open code, identical to file open?

_PT_SetMarkFirstDirectoryBlock mx %11           ; YO I PROLLY DON'T NEED TO DO THIS SINCE I DON'T COME BACK HERE... ?
                  lda   #$00
                  sta   _PT_PARMTABLE+2         ; zero position for MLI_SET_MARK
                  sta   _PT_PARMTABLE+3
                  sta   _PT_PARMTABLE+4
                  lda   _PT_REFNUM
                  sta   _PT_PARMTABLE+1
                  lda   #P8_SET_MARK            ; set up SET_MARK call
                  sta   PT_P8CALL_NUM
                  lda   #P8_SET_MARK_PCNT
                  sta   _PT_PARMTABLE
                  rep   #$30
                  jsr   PT_P8CALL               ; SET_MARK in directory blocks
                  sep   #$30

_PT_ReadNextDirectoryBlock
                                                ;lda   _PT_REFNUM
                                                ;sta   _PT_PARMTABLE+1              ; refnum is already set

                  lda   #<P8_DATA_BUFFER
                  sta   _PT_PARMTABLE+2
                  lda   #>P8_DATA_BUFFER        ; ??? this was originally +1 ?
                  sta   _PT_PARMTABLE+3         ; store in (aDirData)
                  lda   #$00
                  sta   _PT_PARMTABLE+4
                  lda   #$02                    ; read $0200 bytes (= 2 sectors, = 1 ProDOS block)
                  sta   _PT_PARMTABLE+5

                  lda   #P8_READ                ; set up READ call
                  sta   PT_P8CALL_NUM
                  lda   #P8_READ_PCNT
                  sta   _PT_PARMTABLE
                  rep   #$30
                  jsr   PT_P8CALL               ; READ in directory blocks
                  and   #$00ff
                  cmp   #P8_ERR_EOF
                  bne   :continue

                  sep   #$30
                  lda   #P8_CLOSE_PCNT
                  sta   _PT_PARMTABLE
                  lda   #P8_CLOSE               ; set up CLOSE call
                  sta   PT_P8CALL_NUM
                  rep   #$30
                  jsr   PT_P8CALL
                  lda   _PT_ReadDirCount        ; return directory entry count
                  rts



** When you read a block you first must check the length and entries per block
** and store that to loop through the entries
:continue         sep   #$30
                  lda   P8_DATA_BUFFER+$23
                  beq :no_len      ; avoid zero bytes 
                  sta   _PT_DIR_ENTRY_LENGTH
:no_len                            ; avoid zero bytes  
                  lda   P8_DATA_BUFFER+$24
                  beq :no_ent
                  sta   _PT_DIR_ENTRIES_PER_BLOCK
:no_ent
                  lda   #<P8_DATA_BUFFER+4
                  sta   PT_DIR_ENTRY_PTR
                  lda   #>P8_DATA_BUFFER+4
                  sta   PT_DIR_ENTRY_PTR+1
                  lda   _PT_DIR_ENTRIES_PER_BLOCK
                  sta   _PT_DIR_ENTRIES_REMAINING ; now we're all set to scan!

* When you read in a directory or volume block you can skip the first
* entry because it's the header, referring to the directory


_PT_NextEntry     dec   _PT_DIR_ENTRIES_REMAINING

                  bne   :skip
                  beq   _PT_ReadNextDirectoryBlock
:skip
                  lda   PT_DIR_ENTRY_PTR
                  clc
                  adc   _PT_DIR_ENTRY_LENGTH
                  sta   PT_DIR_ENTRY_PTR
                  lda   PT_DIR_ENTRY_PTR+1
                  adc   #$00
                  sta   PT_DIR_ENTRY_PTR+1      ; we're now pointing at a dir entry

                  ldy   #$00
                  lda   (PT_DIR_ENTRY_PTR),y
                  and   #$F0
                  cmp   #$00                    ; skip inactive entries
                  beq   _PT_NextEntry
                  jsr   CloneEntryToPTDirList

                  if    DEBUG
                  jsr   DisplayFile
                  fin
***********************************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  bra   _PT_NextEntry



DisplayFile
                  lda   #">"
                  jsr   $FDED
                  ldy   #$0
                  lda   (PT_DIR_ENTRY_PTR),y    ; get storage type / filename length combination byte
                  and   #$0F                    ; trim storage type
                  sta   (PT_DIR_ENTRY_PTR),y    ; store actual length so we can use this as a length-prefixed string

                  iny                           ; y=1
                  tax                           ; x=len

:prname           lda   (PT_DIR_ENTRY_PTR),y
                  ora   #$80
                  jsr   $FDED
                  iny
                  dex
                  bne   :prname
                  jsr   P8_CROUT2
                  rts



CloneEntryToPTDirList mx %11


:copyNameLen      ldy   #$0
                  lda   (PT_DIR_ENTRY_PTR),y    ; get storage type / filename length combination byte
                  and   #$0F                    ; trim storage type
                  sta   [PT_DST_PTR],y          ; store actual length so we can use this as a length-prefixed string
:copyNameAndType  iny
                  cpy   #$11                    ; copy filename + additional byte for filetype at $10
                  beq   :copyLen
                  lda   (PT_DIR_ENTRY_PTR),y
                  sta   [PT_DST_PTR],y
                  bra   :copyNameAndType
:copyLen          ldy   #$15                    ;$15
                  lda   (PT_DIR_ENTRY_PTR),y
                  pha
                  iny                           ; $16
                  lda   (PT_DIR_ENTRY_PTR),y
                  pha
                  iny                           ; $17
                  lda   (PT_DIR_ENTRY_PTR),y
                  ldy   #$13
                  sta   [PT_DST_PTR],y
                  dey
                  pla
                  sta   [PT_DST_PTR],y
                  dey
                  pla
                  sta   [PT_DST_PTR],y

:advanceDstPtr    rep   $30
                  lda   PT_DST_PTR
                  clc
                  adc   #DirListEntrySize       ; @todo this is defined outside of this file, redefine locally.
                  sta   PT_DST_PTR              ; doesn't cross banks (16bit) so your buffer shouldn't either
                  inc   _PT_ReadDirCount

                  sep   $30

                                                ;inc   $c034
                  rts



** reusable parm table
_PT_REFNUM        dw    0                       ; reusable reference number for a single open file (only uses a byte but padded for 16-bit code)
_PT_PATHNAME_PTR  da    0                       ; reusable pointer to pathname of current file being worked with
_PT_PARMTABLE     ds    16                      ; this is a reusable P8 parameter table to be filled out dynamically

** used for scanning directory/volume entries
_PT_DIR_ENTRY_LENGTH db 0                       ; length in bytes of each entry in the directory
_PT_DIR_ENTRIES_PER_BLOCK db 0                  ; number of entries stored in each block of the directory file
_PT_DIR_ENTRIES_REMAINING db 0                  ; used to track remaining entries when scanning dir blocks

_PT_EXPECT_EOF    db    0                       ;










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











                  IF    0
** DIR ENTRIES LOOK LIKE THIS
*0d00.0d60

*00/0D00:00 00 03 00 FA 53 45 4E-....zSEN
*00/0D08:53 45 49 50 4C 41 59 00-SEIPLAY.
*00/0D10:00 00 00 00 00 00 B3 2A-......3*
*00/0D18:12 16 E0 FF B3 2A 12 16-..`.3*..
*00/0D20:05 00 C3 27 0D 06 00 06-..C'....
*                 |     `---------------------  entriesPerBlock
*                 `------------------------ entry length
*00/0D28:00 40 06 26 50 52 4F 44-.@.&PROD
*00/0D30:4F 53 00 00 00 00 00 00-OS......
*00/0D38:00 00 00 FF 07 00 22 00-......".
*00/0D40:E8 42 00 00 00 00 00 00-hB......
*00/0D48:80 21 00 00 00 00 00 00-.!......
*00/0D50:02 00 D3 53 52 43 00 00-..SSRC..
*00/0D58:00 00 00 00 00 00 00 00-........
*00/0D60:00-.

** ONLINE VOLUME ENTRIES LOOK LIKE THIS
*  I KEEP THE HIGH NIBBLE FOR NOTING THE VOLUME TYPE AND UNIT NUMBER
*0d00.0df0
*00/0D00:B3 52 41 4D 00 00 00 00 00 00 00 00 00 00 00 00-3RAM............
*00/0D10:74 47 53 4F 53 00 00 00 00 00 00 00 00 00 00 00-tGSOS...........
*00/0D20:5A 53 45 4E 53 45 49 50 4C 41 59 00 00 00 00 00-ZSENSEIPLAY.....
*00/0D30:D0 2F 00 00 00 00 00 00 00 00 00 00 00 00 00 00-P/..............
*00/0D40:60 27 00 00 00 00 00 00 00 00 00 00 00 00 00 00-`'..............
*00/0D50:E0 27 00 00 00 00 00 00 00 00 00 00 00 00 00 00-`'..............
*00/0D60:00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00-................
*...
*00/0DE0:00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00-................
*00/0DF0:00-.
*

                  FIN

