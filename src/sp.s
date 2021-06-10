********************************************************************************
*                                                                              *
*     _____                                _     ____     __                   *
*    / ___/  ___    ____    _____  ___    (_)   / __ \   / /  ____ _   __  __  *
*    \__ \  / _ \  / __ \  / ___/ / _ \  / /   / /_/ /  / /  / __ `/  / / / /  *
*   ___/ / /  __/ / / / / (__  ) /  __/ / /   / ____/  / /  / /_/ /  / /_/ /   *
*  /____/  \___/ /_/ /_/ /____/  \___/ /_/   /_/      /_/   \__,_/   \__, /    *
*                                                                   /____/     *
*                                                                              *
*  (c) 2021 Dagen Brock                                                        *
*  NinjaTrackerPlus (NTP) Engine by Jesse Blue of Ninjaforce                   *
********************************************************************************

* $D5	$0008	NTP	NinjaTrackerPlus sequence [Application Specific]
* from: https://github.com/a2infinitum/apple2-filetypes
* not at: https://prodos8.com/docs/techref/quick-reference-card/



VTAB              equ   $FC22                   ; Sets the cursor vertical position (from CV)
HOME              equ   $FC58                   ; Clears the window and puts the cursor in the upper left
                                                ;  corner of the window
COUT              equ   $FDED
KEY               equ   $C000
STROBE            equ   $C010

****
*   PrepareTools
*   LoadFile 'ntpplayer';0    ; bank aligned
*   - get ^^ filesize
*   - dynamically alloc ram   ; nice errors here plz
*   - load a file
*   init buffers et al
*   show program screen
*   do file handler
*     get current dir
*     set list offset index (to zero/first file)
*     set file selected index (to zero/first file)
*     draw list with highlights
*     handle keys
*   onplay
*     Load
*     - play
*     - update vus (if on?)
*     - update/draw note data (if on?)
*     - handle keys
*


*load mod
* take the filename -> get filesize
* allocate ram, contiguous  (also destroy any previous ram)





                  org   $2000                   ; start at $2000 (all ProDOS8 system files)
                  typ   $ff
                  mx    %11

                  sep   $30
                  jsr   Setup80Col
                  jsr   DrawMenuBackground

                                                ;GOXY  #5;#15
                                                ;PRINTSTR MouseString

                                                ;  jsr   P8CALL_GET_PREFIX
                                                ; lda   P8BUFF_PREFIXPATH
                                                ; jsr   P8CALL_ONLINE
                                                ; lda   P8BUFF_DRIVES_ONLINE
                                                ; jsr   DirTest

                  clc
                  xce
                  rep   #$30

                  jsr   PrepareTools
                  jsr   PrepareNTP
                  >>>   PT_GetPrefix            ; returns ptr in A ...
                                                ; >>>   PT_PrintProdosStr ; "/SENSEIPLAY/" is where we start
                  jsr   MenuRefreshDirList
                  brl   MenuLoop

MenuRefreshDirList mx   %00
                  stz   SL_selected             ; always zero
                  >>>   PT_SetDirListPtr,DirList ; tell PT where we want our directory list
                  >>>   PT_ReadDir              ; returns count in A
                  sta   DirListCount
                  rts

MenuRefreshDirListOnline mx %00
                  stz   SL_selected             ; always zero
                  >>>   PT_SetDirListPtr,DirList ; tell PT where we want our directory list
                  >>>   PT_ReadOnline
                  sta   DirListCount
                  rts


MenuHandlePrefixChange mx %11
                  ldy   #1
                  lda   (0),y                   ; get first char
                  cmp   #'.'                     ; is it "parent"?
                  bne   :normal_dir
:parent_dir       >>>   PT_RemovePrefix
                  mx    %00

                  lda   PT_PREFIX_BUFFER
                  and   #$00FF
                  cmp   #1                      ; if we're at "/" root then read online volumes
                  bne   :read_dir
:read_online      jsr   MenuRefreshDirListOnline
                  brl   MenuLoop
:normal_dir       sep   $30
                  lda   (0)                     ;\
                  and   #$0F                    ; > clear any volume data in high nibble
                  sta   (0)                     ;/
                  >>>   PT_AppendPrefix,0
                  >>>   PT_AppendPrefix,#PrefixSlashStr
:read_dir         >>>   PT_SetPrefix
                  jsr   MenuRefreshDirList
                  brl   MenuLoop
                  sep   $30
                  GOXY  #5;#22
                  clc
                  xce
                  rep   $30
                  lda   #PT_PREFIX_BUFFER
                  >>>   PT_PrintProdosStr



SelectorInit      mx    %00
:setup            SL_SETWINDOWPOS 25;5
                  SL_SETWINDOWSIZE 22;8
                  SL_SETRENDERFUNCTION DirectoryRenderItem
                  lda   DirListCount
                  sta   SL_itemscount
                  rts

* assume cursor xy is set for string printing
* a = item index   -  uses 0 and 2 on dp

DirectoryRenderItem mx  %00
                  ldx   #$80
                  stx   2                       ; don't change chars
                  stz   _sl_char_count          ; zero char counter
                  cmp   SL_itemscount           ; see if item exists
                  bcc   :exists
                  bra   :pad_out                ; otherwise just pad out the line


:exists           tax                           ; get item index

                  cpx   SL_selected
                  bne   :not_selected
                  stz   2                       ; turn on char inverter
:not_selected
                  jsr   SetPtr0toDirEntry       ; this calculates the pointer to our entry and stores it at $0
                  sep   #$30

                  lda   (0)                     ; volume "type" is denoted by drive info in high nibble of len byte
                  cmp   #$10
                  bcs   :volume_dir

                  ldy   #$10
                  lda   (0),y                   ; type
:t0               cmp   #$0F                    ; dir?
                  bne   :not_dir

                  ldy   #1
                  lda   (0),y                   ; get first char
                  cmp   #'.'                     ; is it "parent"?
                  bne   :normal_dir

:parent_dir       PRINTSTR IcoParentString
                  bra   :continue2
:volume_dir       PRINTSTR IcoVolString
                  bra   :continue2
:normal_dir       PRINTSTR IcoDirString
                  bra   :continue2
:not_dir
                  PRINTSTR IcoNoString
:continue2
                  lda   (0)                     ; get len byte
                  and   #$0F
                  tax                           ;  .. counter
                  ldy   #1                      ; start printing at byte 1
:pr_loop          lda   (0),y

                  and   #%01111111
                  sec                           ;\___  char inverter
                  sbc   2                       ;/
                  jsr   COUT
                  inc   _sl_char_count
                  iny
                  dex
                  bne   :pr_loop

:pad_out          sep   $30                     ; leave this, could be coming from above 16-bit area
                  lda   SL_windowsize_x

                  sec
                  sbc   _sl_char_count
                  tax


:pad_space        lda   #" "

                  jsr   COUT
                  dex
                  bne   :pad_space

                  rep   #$30
                  rts
_sl_char_count    dw    0                       ; used for width checking strings

******************************************************************  <<<<<<<<<<<<<<
MenuLoop          clc
                  xce
                  rep   #$30
                  jsr   SelectorInit
                                                ;               jsr   SL_DemoList1Run
                  jsr   SL_CalculateOffset
                  jsr   SL_DrawWindow
                  sep   #$30

                  lda   KEY
                  bpl   MenuLoop
                  sta   STROBE

                  lda   KEY
                  ldx   #0                      ; counter
:find_key         cmp   MenuActions,x
                  beq   :found_key
                  inx
                  cpx   #MenuActionsCount
                  bne   :find_key
                  bra   :not_down

:found_key        rep   $30
                  txa
                  and   #$00FF
                  asl
                  tay
                  lda   MenuRoutines,y
                  sta   :jump+1


:jump             jsr   $0000

                  bra   MenuLoop
:not_down         sep   #$30

:debug            jsr   Debug_Hex               ;
                  clc
                  xce

:no_key           rep   #$30
                  bra   MenuLoop
******************************************************************  <<<<<<<<<<<<<<

PlayerLoop        mx    %00
                  sep   $30
:key_loop         lda   KEY

                  bpl   :key_loop
                  sta   STROBE
                  clc
                  xce
                  rep   $30

                  jsr   _NTPstop
                  rts


* x = index to a directory entry
SetPtr0toDirEntry mx    %00
                  lda   #DirList                ; start w pointer at beginning of list
                  cpx   #0
                  beq   :continue               ; no need to add anything
                  clc
:calc_item_start  adc   #DirListEntrySize
                  dex
                  bne   :calc_item_start
:continue         sta   0                       ; address of string at zero
                  rts


MenuEnterSelected mx    %00
                  jsr   SL_GetSelected
                  tax
                  jsr   SetPtr0toDirEntry

                  sep   $30
:check_is_vol     lda   (0)                     ; volume "type" is denoted by drive info in high nibble of len byte
                  cmp   #$10
                  bcc   :check_is_dir
                  jsr   MenuHandlePrefixChange
:check_is_dir     ldy   #$10
                  lda   (0),y                   ; type

:t0               cmp   #$0F
                  bne   :not_dir
                  jsr   MenuHandlePrefixChange
                  rts

:not_dir          jsr   LoadNTP
                  jsr   StartMusic
                  jsr   PlayerLoop
                  rts

MenuActionsCount  =     7
MenuActions       db    #'w'
                  db    #'W'
                  db    #$0B                    ; up

                  db    #'s'
                  db    #'S'
                  db    #$0A                    ; dn

                  db    #$0D                    ; enter


MenuRoutines      da    SL_DecSelected
                  da    SL_DecSelected
                  da    SL_DecSelected

                  da    SL_IncSelected
                  da    SL_IncSelected
                  da    SL_IncSelected

                  da    MenuEnterSelected




* 8-bit with value in a plz
Debug_Hex         mx    %11
                  pha                           ; hex debug
                  GOXY  #75;#22                 ;
                  pla                           ;
                  jmp   $FDDA                   ; implied rts


*********************************************************
*                                               ; .... TEST CODE ....
*                 lda   #$0003                  ; bank 3
*                 sta   $02                     ; dp ptr hi
*                 stz   $00                     ; dp ptr lo
*                 PT_LoadFilenameToPtr 'ntp/engine.ntp';0
*                 jsr   StartMusic
*                 FUNHALT

LoadNTP           mx    %11
                  clc
                  xce
                  rep   $30
                  >>>   PT_GetPrefix            ; returns ptr in A ...
                                                ; >>>   PT_PrintProdosStr ; "/SENSEIPLAY/" is where we start
                  lda   #$0003                  ; bank 3
                  sta   $06                     ; dp ptr hi
                  stz   $04                     ; dp ptr lo
                  PT_LoadFilePtrToPtr 0;4
                  rts


StartMusic        mx    %00
                  ldy   #$0003
                  ldx   #0
                  jsr   _NTPprepare
                  bcc   ok
                  brk   $ee                     ;@todo?
ok                lda   #0
                  jsr   _NTPplay
                  rts


PrepareNTP        mx    %00
                  jsr   AllocOneBank
                  sta   BnkNTP

                  sta   _NTPprepare+2           ; FIX TABLE
                  sta   _NTPplay+2
                  sta   _NTPstop+2
                  sta   _NTPgetvuptr+2
                  sta   _NTPgete8ptr+2
                  sta   _NTPforcesongpos+2
                  sta   _NTPgetsongpos+2

                  sta   1                       ;\__ build 24 bit address like 00 00 B0 nn
                  stz   0                       ;/
                  PT_LoadFilenameToPtr 'ntpplayer';0 ; address in 0
                  rts                           ; we don't need bank addr any more



                  sep   #$30
                  sec
                  xce
:fo               inc   $c034
                  bra   :fo


                  jsr   P8Quit

******************************************
* Call this at the start of your program *
******************************************
                  mx    %00
PrepareTools      stz   MasterId                ; I haven't created a new user ID
                  _TLStartUp
                  pha
                  _MMStartUp
                  pla
                  bcc   MM_OK
* If the Memory Manager reported an error, we need to allocate our own memory first.
                  _MTStartUp
* First we need a user ID.
                  pha
                  pea   $1000
                  _GetMasterId                  ; Get me a new user ID (Application)
                  pla
                  sta   MasterId                ; Save it for later
* Now give us all of bank zero and bank one.
                  pha
                  pha                           ; Result space
                  pea   $0000
                  pea   $B800                   ; Block size
                  lda   MasterId
                  pha                           ; User ID
                  pea   $C002                   ; Attributes: locked, fixed, absolute
                  pea   $0000
                  pea   $0800                   ; Location (bank 0, $0800-$BFFF)
                  _NewHandle
                  plx
                  ply
                  _Err                          ; This shouldn't happen!
                  sty   Bnk0Hnd
                  stx   Bnk0Hnd+2               ; Save handle to bank 0 memory
                  pha
                  pha                           ; Result space
                  pea   $0000
                  pea   $B800                   ; Block size
                  lda   MasterId
                  pha                           ; User ID
                  pea   $C002                   ; Attributes: locked, fixed, absolute
                  pea   $0001
                  pea   $0800                   ; Location (bank 1, $0800-$BFFF)
                  _NewHandle
                  plx
                  ply
                  _Err                          ; This shouldn't happen!
                  sty   Bnk1Hnd
                  stx   Bnk1Hnd+2               ; Save handle to bank 0 memory
* We have the necessary memory protected.  Start up the memory manager again.
                  pha
                  _MMStartUp
                  pla
                  _Err                          ; This shouldn't happen!
MM_OK             sta   UserId                  ; Save the memory ID
                  rts

******************************************
* Basic Error Macro                      *
******************************************
_Err              mac
                  bcc   NoErr
                  do    ]0                      ; (DO if true)
                  jsr   PgmDeath                ;  this is conditionally compiled if
                  str   ]1                      ;  we pass in an error statement
                  else                          ; (ELSE)
                  jmp   PgmDeath0               ;  we just call the simpler error handler
                  fin                           ; (FIN)
NoErr             eom

****************************************
* Fatal Error Handler                  *
****************************************
PgmDeath          tax
                  pla
                  inc
                  phx
                  phk
                  pha
                  bra   ContDeath
PgmDeath0         pha
                  pea   $0000
                  pea   $0000
ContDeath         ldx   #$1503
                  jsl   $E10000


******************************************
* Standard ProDOS 8 Quit routine         *
******************************************
                  mx    %11
P8Quit            jsr   P8_MLI_CALL             ; first actual command, call ProDOS vector
                  dfb   $65                     ; with "quit" request ($65)
                  da    QuitParm
                  bcs   Error                   ; what's the point?  ;)
Error             brk   $00                     ; shouldn't ever  here!

QuitParm          dfb   4                       ; number of parameters
                  dfb   0                       ; standard quit type
                  da    $0000                   ; not needed when using standard quit
                  dfb   0                       ; not used
                  da    $0000                   ; not used


MasterId          ds    2
UserId            ds    2
BnkNTP            hex   0000                    ; used for NTP engine
Bnk0Hnd           hex   00000000
Bnk1Hnd           hex   00000000



**********************************************************   NINJATRACKERPLUS
** STUB FUNCTIONS
_NTPprepare       jsl   NTPprepare
                  rts
_NTPplay          jsl   NTPplay
                  rts
_NTPstop          jsl   NTPstop
                  rts
_NTPgetvuptr      jsl   NTPgetvuptr
                  rts
_NTPgete8ptr      jsl   NTPgete8ptr
                  rts
_NTPforcesongpos  jsl   NTPforcesongpos
                  rts
_NTPgetsongpos    jsl   NTPgetsongpos
                  rts

NinjaTrackerPlus  =     $0f0000
NTPprepare        =     NinjaTrackerPlus        ; IN: X=low, Y=high
NTPplay           =     NinjaTrackerPlus+3      ; IN: A=0 for loop
NTPstop           =     NinjaTrackerPlus+6
NTPgetvuptr       =     NinjaTrackerPlus+9      ; OUT: X:low, Y=high
NTPgete8ptr       =     NinjaTrackerPlus+12     ; tool does not use
NTPforcesongpos   =     NinjaTrackerPlus+15     ; tool does not use
NTPgetsongpos     =     NinjaTrackerPlus+18     ; tool does not use


FUNHALT           MAC
                  sec
                  xce
                  sep   #$30
:loop             lda   $c019

                  bpl   :skip
                  lda   #$f6
                  sta   $c022
:skip             lda   #$db
;                  jsr   $fdda

                  lda   $c022
                  clc
                  adc   #$10
                  sta   $c022
                  lda   #$FF
:delay            dec
                  bne   :delay
                  bra   :loop
                  <<<


                  put   p8tools
                  put   scrollist
                  dsk   sensei.system


GOXY_mixed        MAC

                  IF    mx/2-1                  ; LONGM

                  lda   #]2+{]1*$100}           ; multiply low byte by 256 and add high byte
                  sta   $24
                  php
                  sep   #$30

                  jsr   VTAB


                  plp
                  brk   #$f0

                  ELSE
                  ldx   ]1
                  ldy   ]2
                  stx   $24
                  sty   $25
                  jsr   VTAB
                  FIN
                  <<<

GOXY              MAC
                  ldx   ]1
                  ldy   ]2
                  stx   $24
                  sty   $25
                  jsr   VTAB
                  <<<

PRINTSTR          MAC
                  lda   #]1
                  ldy   #>]1
                  jsr   PrintString
                  <<<

PRINTSTR_mixed    MAC
                  IF    16_BIT                  ;  mx 00
                  sep   #$30
                  lda   #]1
                  ldy   #>]1
                  jsr   PrintString
                  rep   #$30

                  ELSE                          ; mx 11
                  lda   #]1
                  ldy   #>]1
                  jsr   PrintString
                  FIN
                  rts
                  <<<


PRINTXY           MAC

                  ldx   ]1
                  ldy   ]2
                  stx   $24
                  sty   $25
                  jsr   VTAB
                  lda   #]3
                  ldy   #>]3
                  jsr   PrintString
                  <<<

* PrintString (A=Low Byte,  Y=High Byte)
PrintString       mx    %11
                  sta   :loop+1
                  sty   :loop+2

                  ldy   #0
:loop             lda   $FFFF,y                 ; dummy bytes
                  beq   :done
                  jsr   $FDED                   ;COUT
                  iny
                  bra   :loop
:done             rts


BigNum            MAC
                  lda   #]2+{]1*$100}           ; multiply low byte by 256 and add high byte
                  <<<
* lda #MainMenuStrs
* ldy #>MainMenuStrs
* ldx #05 ; horiz pos
PrintStringsX     stx   _printstringsx_horiz

                  sta   $0
                  sty   $1
:loop             lda   _printstringsx_horiz
                  sta   $24
                  lda   $0                      ; slower, but allows API reuse
                  ldy   $1
                  jsr   PrintString             ; y is last val
                  iny
                  lda   ($0),y
                  beq   :done
                  tya                           ; not done so add strlen to source ptr
                  clc
                  adc   $0
                  sta   $0
                  bcc   :nocarry
                  inc   $1
:nocarry          bra   :loop

:done             rts

_printstringsx_horiz db 00

Setup80Col        mx    %11
                  lda   #$A0                    ;USE A BLANK SPACE TO
                  jsr   $C300                   ;TURN ON THE VIDEO FIRMWARE
                  rts

DrawMenuBackground mx   %11
                  jsr   HOME
                  lda   #TitleStrs
                  ldy   #>TitleStrs
                  ldx   #00                     ; horiz pos
                  jmp   PrintStringsX           ; implied rts

MyString          asc   "Welcome",00
MouseString       asc   $1B,'@ABCDEFGHIJKLMNOPQRSTUVWXYZXYXY[\]^_',$18,00
IcoDirString      asc   $1B,'XY',$18," ",$00
IcoParentString   asc   $1B,'KI',$18," ",$00
IcoVolString      asc   $1B,'Z^',$18," ",$00
IcoNoString       asc   "   ",$00
PrefixSlashStr    str   '/'

DirListCount      dw    0

TitleStrs
                  asc   " _____________________________________________________________________________",$8D,00
                  asc   $1B,'ZV_@ZVWVWVWV_',"SenseiPlay",'ZVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWV_'," // Infinitum ",'ZWVWVWVW_',$18,,$8D,00
                  asc   $1B,'ZLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL_',$18,$8D,00
                  asc   $1B,'Z',"                          Choose a track:                                    ",'_',$18,$8D,00
                  asc   $1B,'Z',"                       ____________________________                          ",'_',$18,$8D,00
                  asc   $1B,'Z',"                      ",'Z',"                            ",'_',"                         ",'_',$18,$8D,00
                  asc   $1B,'Z',"                      ",'Z',"                            ",'_',"                         ",'_',$18,$8D,00
                  asc   $1B,'Z',"                      ",'Z',"                            ",'_',"                         ",'_',$18,$8D,00
                  asc   $1B,'Z',"                      ",'Z',"                            ",'_',"                         ",'_',$18,$8D,00
                  asc   $1B,'Z',"                      ",'Z',"                            ",'_',"                         ",'_',$18,$8D,00
                  asc   $1B,'Z',"                      ",'Z',"                            ",'_',"                         ",'_',$18,$8D,00
                  asc   $1B,'Z',"                      ",'Z',"                            ",'_',"                         ",'_',$18,$8D,00
                  asc   $1B,'Z',"                      ",'Z',"                            ",'_',"                         ",'_',$18,$8D,00
                  asc   $1B,'Z',"                       ",'LLLLLLLLLLLLLLLLLLLLLLLLLLLL',"                          ",'_',$18,$8D,00
                  asc   $1B,'Z',"                                                                             ",'_',$18,$8D,00
                  asc   $1B,'Z',"                                                                             ",'_',$18,$8D,00
                  asc   $1B,'Z',"    _____                                _     ____     __                   ",'_',$18,$8D,00
                  asc   $1B,'Z',"   / ___/  ___    ____    _____  ___    (_)   / __ \   / /  ____ _   __  __  ",'_',$18,$8D,00
                  asc   $1B,'Z',"   \__ \  / _ \  / __ \  / ___/ / _ \  / /   / /_/ /  / /  / __ `/  / / / /  ",'_',$18,$8D,00
                  asc   $1B,'Z',"  ___/ / /  __/ / / / / (__  ) /  __/ / /   / ____/  / /  / /_/ /  / /_/ /   ",'_',$18,$8D,00
                  asc   $1B,'Z'," /____/  \___/ /_/ /_/ /____/  \___/ /_/   /_/      /_/   \__,_/   \__, /    ",'_',$18,$8D,00
                  asc   $1B,'Z',"                                                                  /____/     ",'_',$18,$8D,00
                  asc   $1B,'Z',"                                                                             ",'_',$18,$8D,00
                  asc   $1B," ",'LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL',$18," ",00
                  hex   00,00


                  ds    \
DirListMaxEntries =     256
DirListEntrySize  =     20                      ; 16 name + 1 type + 3 len
DirList           ds    #DirListEntrySize*DirListMaxEntries
