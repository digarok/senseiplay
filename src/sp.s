********************************************************************************
*                                                                              *
*     _____                                _     ____     __                   *
*    / ___/  ___    ____    _____  ___    (_)   / __ \   / /  ____ _   __  __  *
*    \__ \  / _ \  / __ \  / ___/ / _ \  / /   / /_/ /  / /  / __ `/  / / / /  *
*   ___/ / /  __/ / / / / (__  ) /  __/ / /   / ____/  / /  / /_/ /  / /_/ /   *
*  /____/  \___/ /_/ /_/ /____/  \___/ /_/   /_/      /_/   \__,_/   \__, /    *
*                                                                   /____/     *
*                                                                              *
*  (c) 2021-2022 Digarok                                                       *
*  NinjaTrackerPlus (NTP) Engine by Jesse Blue of Ninjaforce                   *
********************************************************************************

* $D5	$0008	NTP	NinjaTrackerPlus sequence [Application Specific]
* from: https://github.com/a2infinitum/apple2-filetypes
* not at: https://prodos8.com/docs/techref/quick-reference-card/


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


                  jsr   InitTextTools
                  jsr   Setup80Col
                  
                  jsr   TextColorSet
                  jsr   DrawMenuBackground
                  jsr   DrawNinjaAnimIn

                           

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
                  >>>   PT_PrintProdosStr       ; todo: is this needed?


SelectorInit      mx    %00
:setup            SL_SETWINDOWPOS 31;11
                  SL_SETWINDOWSIZE 32;8         
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
                  stz   _sl_show_size           ; turn off filesize printing
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
:not_dir          PRINTSTR IcoNoString
                  dec  _sl_show_size            ; turn on filesize printing
:continue2        lda #4                        ;\_ inc the character count since we printed...
                  sta _sl_char_count            ;/    this could be optimized out but I'm leaving it for flexibility.
                  lda   (0)                     ; get len byte
                  and   #$0F
                  tax                           ;  .. counter
                  ldy   #1                      ; start printing at byte 1
:pr_loop          lda   (0),y

                                                ;and                   #%01111111
                  clc                           ;\___  char inverter
                  adc   2                       ;/
                  cmp   #$40
                  bcc   :okVal
                  cmp   #$60
                  bcs   :okVal
                  sec
                  sbc   #$40                    ; deal with mousetext chars


:okVal            jsr   COOT8
                  inc   _sl_char_count
                  iny
                  dex
                  bne   :pr_loop  

:pad_out          sep   $30                     ; leave this, could be coming from above 16-bit area
                  lda   SL_windowsize_x
                  sec
                  sbc   _sl_char_count          ; win_x - printed chars = pad amount
                  ldx _sl_show_size             ; check if we are printing 10 char size string too
                  beq :no_size_pad
                  sbc   #10

:no_size_pad      tax                          

:pad_space        lda   #" "
                  jsr   COOT8
                  dex
                  bne   :pad_space

:size_test        lda _sl_show_size
                  beq :done
                
                  ldy #17 ; size offset (3-byte)
                  lda (0),Y
                  sta HEX24   ; set conversion bytes
                  iny
                  lda (0),Y
                  sta HEX24+1 
                  iny
                  lda (0),Y
                  sta HEX24+2

                  jsr HEX24TODEC8
                  jsr DEC8TOCHAR10R
                  ldx #0
:loop             lda CHAR10,x
                  jsr COOT8
                  inx
                  cpx #10
                  bne :loop
:done             rep $30
                  rts


                    ldy #19
                    lda (0),Y
                    sta HEX24+2 ; set for conv
            ;       jsr PrHex 
                    ldy #18
                    lda (0),Y
                    sta HEX24+1 ; set for conv
             ;      jsr PrHex 
                    ldy #17
                    lda (0),Y
                    sta HEX24+0 ; set for conv
           ;        jsr PrHex 

                   jsr HEX24TODEC8
                   jsr DEC8TOCHAR10R
              lda #" " 
              jsr COOT8
    

*    jsr HEX24TODEC8
*    jsr DEC8TOCHAR10R   
*    lda CHAR10


                  rep   #$30
                  rts
_sl_char_count    dw    0                       ; used for width checking strings
_sl_show_size     dw 0






******************************************************************  <<<<<<<<<<<<<<
MenuLoop          clc
                  xce
                  rep   #$30

                  jsr   SelectorInit            ; not expensive so just leave it here
                                                ;               jsr   SL_DemoList1Run
                  jsr   SL_CalculateOffset
                  jsr   SL_DrawWindow

:key_handling
                  sep   #$30

                  lda   KEY
                  bpl   MenuLoop
                  sta   STROBE

                  lda   KEY                     
                  ldx   #0                      ; index/counter
:find_key         cmp   MenuActions,x           ; go through all the keys we know about
                  beq   :found_key
                  inx
                  cpx   #MenuActionsCount
                  bne   :find_key
                  bra   :debug

:found_key        rep   $30
                  txa
                  and   #$00FF
                  asl
                  tay
                  lda   MenuRoutines,y
                  sta   :jump+1


:jump             jsr   $0000                   ; SMC
                  bra   MenuLoop

:debug            sep   #$30                    ;
                  pha                           ; hex debug
                  GOXY  #75;#22                 ;
                  pla                           ;
                  jsr   PrHex                   
              
                  bra   MenuLoop
******************************************************************  <<<<<<<<<<<<<<



PlayerLoop        mx    %00

:key_loop         jsr   ShowTrackPos
                  jsr   ShowVUs
                  sep   $30
                  lda   KEY
                  bpl   :key_loop
                  sta   STROBE

:cleanup          jsr   DrawMenuBackground
                  jsr   DrawNinjaAnimIn

                  clc
                  xce
                  rep   $30

                  jsr   _NTPstop


                  rts

ShowVUs           clc
                  xce
                  rep   $30
                  jsr   _NTPgetvuptr
                  stx   0
                  sty   2
                  lda   [0]                     ; number of tracks
                  tax                           ; counter
                  ldy   #2                      ; index of first track
:vu_loop          lda   [0],y
                  jsr   RenderVU
                  lda   [0],y
                  jsr   RenderVUBar
                  lda   [0],y
                  jsr   RenderVUVals
                  iny
                  iny
                  dex
                  bne   :vu_loop
                  rts


* y = (vu number*2)+2   a = value
RenderVUVals      mx    %00

                  phx
                  phy
                  sep   $30

                  tya
                  pha
                  asl                           ; now *4
                  clc
                  adc   VUBarX                  ; add offset
                  sta   8
                  GOXY  8;VUBarY+7
                  plx
                  dex
                  dex

                  lda   VUBarValues,x
                  jsr   PrHex

                  clc
                  xce
                  rep   $30

                  ply
                  plx
                  rts

* y = (vu number*2)+2   a = value
RenderVU          mx    %00

                  phx
                  phy
                  sep   $30
                  pha
                  tya
                  asl                           ; now *4
                  clc
                  adc   VUBarX                  ; add offset
                  sta   8
                  GOXY  8;VUBarY+6

                  pla
                  jsr   PrHex

                  clc
                  xce
                  rep   $30

                  ply
                  plx
                  rts


ShowTrackPos      clc
                  xce
                  rep   $30
                  jsr   _NTPgetsongpos
                  stx   0
                  sty   2
                  sep   $30


                                                ; bg area pre-drawn at start
                  GOXY  #36;#1                  ; cursor
                  ldy   #7                      ; pat
                  lda   [0],y
                  jsr   PrHex
                  GOXY  #46;#1                  ; cursor
                  ldy   #8                      ; pos
                  lda   [0],y
                  jsr   PrHex


                  GOXY  #22;#22                 ;;; debug show track pos
                  ldy   #2
                  ldal  [0],y
                  jsr   PrHex
                  ldy   #3
                  ldal  [0],y
                  jsr   PrHex

                  lda   #" "
                  jsr   COOT8
                  ldy   #4
                  ldal  [0],y
                  jsr   PrHex
                  ldy   #5
                  ldal  [0],y
                  jsr   PrHex

                  lda   #" "
                  jsr   COOT8
                  ldy   #6
                  ldal  [0],y
                  jsr   PrHex
                  ldy   #7
                  ldal  [0],y
                  jsr   PrHex

                  lda   #" "
                  jsr   COOT8
                  ldy   #8
                  ldal  [0],y
                  jsr   PrHex
                  ldy   #9
                  ldal  [0],y
                  jsr   PrHex

                  lda   #" "
                  jsr   COOT8
                  lda   #" "
                  jsr   COOT8
                  lda   #" "
                  jsr   COOT8
                  ldy   #10
                  ldal  [0],y
                  jsr   PrHex
                  ldy   #11
                  ldal  [0],y
                  jsr   PrHex

                  lda   #" "
                  jsr   COOT8
                  ldy   #12
                  ldal  [0],y
                  jsr   PrHex
                  ldy   #13
                  ldal  [0],y
                  jsr   PrHex
                  rts                           ; should return in 8-bit




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

:not_dir                                        ; Let's load our song if we can
                  jsr   DrawNinjaLoadEyes
                  jsr   SaveDP
                  jsr   LoadNTP
                  jsr   RestoreDP
                  jsr   StartMusic
                  bcs   :err
                  jsr   PlayerUi
                  jsr   PlayerLoop
:err
                  jsr   UnloadNTP
                  rts
PlayerUi          mx    %00                     ; @todo: this is a mess
                  sep   $30
                  GOXY  #30;#1                  ; title pat/pos (in title bar)
                  PRINTSTR PatPosString

                  GOXY  #0;VUBarY-4
                  lda   #NowPlayingStrs         ; now playing and clear file widget
                  ldy   #>NowPlayingStrs
                  ldx   #00                     ; horiz pos
                  jsr   PrintStringsX

                  clc
                  xce
                  rep   $30
                  jsr   SL_GetSelected          ; print name
                  tax
                  jsr   SetPtr0toDirEntry

                  sep   $30
                  GOXY  #30;VUBarY-2

                  clc
                  xce
                  rep   $30

                  lda   $0

                  >>>   PT_PrintProdosStr

                  jsr   _NTPgetvuptr

                  stx   0
                  sty   2

:set_vu_x_offset  lda   [0]                     ; number of tracks
                  sta   VUBarCount
                  cmp   #$4+1                   ;  set x offset based on how many tracks  (<=4), (<=8), (>8)
                  bcs   :over_4
:4_or_fewer       lda   #27
                  sta   VUBarX
                  bra   :vu_offset_complete
:over_4           cmp   #$8+1
                  bcs   :over_8
:5_to_8           lda   #20
                  sta   VUBarX
                  bra   :vu_offset_complete
:over_8
                  lda   #6
                  sta   VUBarX
:vu_offset_complete

:render_vu_boxes  jsr   RenderVUBoxes

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




_DPBAK            ds    256
SaveDP            clc
                  xce
                  rep   $30
                  ldx   #$FE
:copy             lda   $0,x
                  sta   _DPBAK,x
                  dex
                  dex
                  bpl   :copy
                  rts
RestoreDP         clc
                  xce
                  rep   $30
                  ldx   #$FE
:copy             lda   _DPBAK,x
                  sta   $0,x
                  dex
                  dex
                  bpl   :copy
                  rts

UnloadNTP         mx %00
                  clc                   ; I have trust issues..
                  xce
                  rep #$30      
                  ~DisposeHandle BnkMODHnd
                  _Err
                  rts

*********************************************************
LoadNTP           mx    %11
                  clc
                  xce
                  rep   $30
                                                ; >>>   PT_GetPrefix            ; returns ptr in A ...
                                                ; >>>   PT_PrintProdosStr ; "/SENSEIPLAY/" is where we start


* NEW STUFF      
                  ldy #19                      ; size offset (3-byte) from file entry
                  lda (0),Y
                  and #$00FF
                  tax                          ; 24 bit filesize to x/y
                  ldy #17
                  lda (0),Y
                  tay
                  jsr AllocContiguousPageAlign  ; allocate that much
                  sta BnkMODHnd+2               ; save the handle
                  stx BnkMODHnd

                  
                  stx   $08                     ; dereference ptr
                  sta   $0a                     
                  ldy #0
                  ldal [$8],Y
                  sta $04
                  sta BnkMODPtr                 ; save for easy access
                  ldy #2
                  ldal [$8],Y
                  sta $06
                  sta BnkMODPtr+2
                  PT_LoadFilePtrToPtr 0;4       ; and load file into allocated RAM
                  rts


StartMusic        mx    %00               
                  ldx BnkMODPtr
                  ldy BnkMODPtr+2
                  lda #0 ; no channel doubling
                  jsr   _NTPprepare
                  bcc   :ok
                  jsr   HoldUp
                  sec
                  rts

:ok               lda   #0
                  jsr   _NTPplay
                  sep $30
                  
                  clc
                  rts


HoldUp            mx    %00
                  sep   $30
                  lda   $c034
                  pha


                  ldy   #80                     ; delay

:wait_vbl_start   lda   $c019
                  bpl   :wait_vbl_start
:wait_vbl_end     inc   $c034
                  lda   $c019
                  bmi   :wait_vbl_end

                  lda   #1
                  sta   $c034
                  dey
                  bne   :wait_vbl_start
                  pla
                  sta   $c034
                  rts

                  mx %11
WaitVBL           
:wait_vbl_start   lda   $c019
                  bpl   :wait_vbl_start
:wait_vbl_end     lda   $c019
                  bmi   :wait_vbl_end
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
                  _MTStartUp                    ; If the Memory Manager reported an error, we need to allocate our own memory first.
                  pha                           ; First we need a user ID.
                  pea   $1000
                  _GetMasterId                  ; Get me a new user ID (Application)
                  pla
                  sta   MasterId                ; Save it for later
                  pha                           ; Now give us all of bank zero and bank one.
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
BnkMOD            hex   0000                    ; used for MOD loading... there may be multiple banks but it must be contiguous starting at this one.
BnkMODHnd         hex   00000000
BnkMODPtr         adrl  00000000
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
;                  jsr   PrHex

                  lda   $c022
                  clc
                  adc   #$10
                  sta   $c022
                  lda   #$FF
:delay            dec
                  bne   :delay
                  bra   :loop
                  <<<





GOXY              MAC
                  ldx   ]1
                  ldy   ]2
                  stx   text_h
                  sty   text_v
                  <<<

PRINTSTR          MAC
                  lda   #]1
                  ldy   #>]1
                  jsr   PrintString
                  <<<



* PrintString (A=Low Byte,  Y=High Byte)
PrintString       mx    %11
                  sta   :loop+1
                  sty   :loop+2

                  clc
                  xce

                  ldy   #0
:loop             lda   $FFFF,y                 ; dummy bytes
                  beq   :done
                  jsr   COOT8
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
                  sta   text_h
                  lda   $0                      ; slower, but allows API reuse
                  ldy   $1
                  jsr   PrintString             ; y is last val
                  inc   text_v                  ; update cursor pos
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
_printstringsy_clip db 00
* lda #MainMenuStrs
* ldy #>MainMenuStrs
* ldx #05 ; horiz pos
SetYClip          sta _printstringsy_clip
                  rts
PrintStringsXYClip     stx   _printstringsx_horiz
                  sta   $0
                  sty   $1
                  stz   $12                    ; y clip
:loop             
                  lda   $12 
                  cmp   _printstringsy_clip
                  beq   :done
                  lda   _printstringsx_horiz
                  sta   text_h
                  lda   $0                      ; slower, but allows API reuse
                  ldy   $1
                  jsr   PrintString             ; y is last val
                  inc   text_v                  ; update cursor pos
                  inc   $12                      ; update y clip
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

Setup80Col        mx    %11
                  lda   #$A0                    ;USE A BLANK SPACE TO
                  jsr   $C300                   ;TURN ON THE VIDEO FIRMWARE
                  rts

DrawMenuBackground mx   %11

                  jsr   text_clear              ; clear screen
                  stz   text_h                  ; set top left corner (HOME)
                  stz   text_v

                  lda   #TitleStrs
                  ldy   #>TitleStrs
                  ldx   #00                     ; horiz pos
                  jmp   PrintStringsX           ; implied rts

DrawNinjaLoadEyes mx    %11
                  GOXY  #15;#15
                  PRINTSTR NinjaAppleEyesClose
                  rts

DrawNinjaAnimIn   mx %11
                  lda   #23             ; start position Y
                  sta   _dnai_y
                  stz   _dnai_y_clip    ; 0 lines
                  lda   #15             ; ninja height
                  sta   _dnai_y_clip_max

:loop             jsr   WaitVBL

                  ldx   #3
                  ldy   _dnai_y
                  lda   _dnai_y_clip
                  jsr   DrawNinjaXYClip
                
                  jsr   WaitVBL
                  dec   _dnai_y         ; next pass start higher
                  inc   _dnai_y_clip
                  lda   _dnai_y_clip
                  cmp   _dnai_y_clip_max
                  beq   :done
                  bra :loop
:done             rts 
_dnai_y          db 0   ; y start
_dnai_y_clip     db 0   ; current value
_dnai_y_clip_max db 0   ; done value


                  
TextColorSet      mx %11
                  lda $c034
                  sta _bak_bordercolor
                  lda $c022
                  sta _bak_textcolor
                  lda #0
                  sta $c034
                  lda #$C0 ; grn black
                  sta $c022
                  rts

TextColorRestore  mx %11          
                  lda _bak_bordercolor
                  sta $c022
                  lda _bak_textcolor
                  sta $c034
                  rts
                  
_bak_bordercolor ds 1
_bak_textcolor ds 1




PrefixSlashStr    str   '/'
TestStr           str   'Hello all'

DirListCount      dw    0


MouseString       asc   '@ABCDEFGHIJKLMNOPQRSTUVWXYZXYXY[\]^_',00
PatPosString      asc   '_',"Pat:    ",'C'," Pos:   ",'Z',00
IcoDirString      asc   'XY'," ",$00
IcoParentString   asc   'KI'," ",$00
IcoVolString      asc   'Z^'," ",$00
IcoNoString       asc   "   ",$00
TitleStrs
                  asc   " _____________________________________________________________________________",00
                  asc   'ZV_@ZVWVWVWV_',"SenseiPlay",'ZVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWV_'," // Infinitum ",'ZWVWVWVW_',00
                  asc   'ZLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL_',00
                  asc   'Z',"    _____                                _     ____     __                   ",'_',00
                  asc   'Z',"   / ___/  ___    ____    _____  ___    (_)   / __ \   / /  ____ _   __  __  ",'_',00
                  asc   'Z',"   \__ \  / _ \  / __ \  / ___/ / _ \  / /   / /_/ /  / /  / __ `/  / / / /  ",'_',00
                  asc   'Z',"  ___/ / /  __/ / / / / (__  ) /  __/ / /   / ____/  / /  / /_/ /  / /_/ /   ",'_',00
                  asc   'Z'," /____/  \___/ /_/ /_/ /____/  \___/ /_/   /_/      /_/   \__,_/   \__, /    ",'_',00
                  asc   'Z',"                                                                  /____/     ",'_',00
                  asc   'Z',"                              Choose a track:                                ",'_',00
                  asc   'Z',"                             _________________________________               ",'_',00
                  asc   'Z',"                            ",'Z',"                                 ",'_',"              ",'_',00
                  asc   'Z',"                            ",'Z',"                                 ",'_',"              ",'_',00
                  asc   'Z',"                            ",'Z',"                                 ",'_',"              ",'_',00
                  asc   'Z',"                            ",'Z',"                                 ",'_',"              ",'_',00
                  asc   'Z',"                            ",'Z',"                                 ",'_',"              ",'_',00
                  asc   'Z',"                            ",'Z',"                                 ",'_',"              ",'_',00
                  asc   'Z',"                            ",'Z',"                                 ",'_',"              ",'_',00
                  asc   'Z',"                            ",'Z',"                                 ",'_',"              ",'_',00
                  asc   'Z',"                             ",'LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL',"               ",'_',00
                  asc   'Z',"                                                                             ",'_',00
                  asc   'Z',"                                                                             ",'_',00

                  asc   'Z',"                                                                             ",'_',00
                  asc   " ",'LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL'," ",00        
                  hex   00,00

NinjaStrs         asc   "                        ",00
                  asc   "           ______       ",00
                  asc   "         .'      `.     ",00
                  asc   "   /.   /          \    ",00
                  asc   "   `.`.:            :   ",00
                  asc   "   _.:'|   ,--------|   ",00
                  asc   "   `-./|  | ",'[',"     ",'['," :   ",00
                  asc   "       :  \_.---^-._|   ",00
                  asc   "    __  \           ;   ",00
                  asc   " .",'SL',"\_\  :         /    ",00
                  asc   "     \_\ :  ` . _.`     ",00
                  asc   "      \ (        :      ",00
                  asc   " __.---``         `--._ ",00
                  asc   "/                      \",00
                  hex   00,00,00,00,00,00   ; not needed
NinjaAppleEyesClose asc '@',"     ",'@',00
                 mx %11
; x = x, y=y, clip = ylines to draw before stopping
DrawNinjaXYClip   jsr SetYClip ; with A value
                  sty text_v                                  ; works
                  lda #NinjaStrs
                  ldy #>NinjaStrs
                  jsr PrintStringsXYClip
                  rts


NowPlayingStrs    asc   'Z',"                          Now Playing:                                       ",'_',00
                  asc   'Z',"                       ____________________________                          ",'_',00
                  asc   'Z',"                      ",'Z',"                            ",'_',"                         ",'_',00
                  asc   'Z',"                       ",'LLLLLLLLLLLLLLLLLLLLLLLLLLLL',"                          ",'_',00
                  asc   'Z',"                                                                             ",'_',00
                  asc   'Z',"                                                                             ",'_',00
                  asc   'Z',"                                                                             ",'_',00
                  asc   'Z',"                                                                             ",'_',00
                  asc   'Z',"                                                                             ",'_',00
                  asc   'Z',"                                                                             ",'_',00
                  asc   'Z',"                                                                             ",'_',00
                  asc   'Z',"                                                                             ",'_',00
                  asc   'Z',"                                                                             ",'_',00
                  asc   'Z',"                                                                             ",'_',00
                  hex   00,00


                  ds    \
DirListMaxEntries =     256
DirListEntrySize  =     20                      ; 16 name + 1 type + 3 len
DirList           ds    #DirListEntrySize*DirListMaxEntries
WaitKeyColor      mx %11
                  inc $c034
WaitKey           mx    %11
:nope             lda   KEY
                  bpl   :nope
                  sta   STROBE
                  rts


                  put   p8tools
                  put   texttools
                  put   scrollist
                  put   vubars
                  dsk   sensei.system
