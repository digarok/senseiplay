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

* $D5 $0008 NTP NinjaTrackerPlus sequence [Application Specific]
* from: https://github.com/a2infinitum/apple2-filetypes
* not at: https://prodos8.com/docs/techref/quick-reference-card/



                    org   $2000                 ; start at $2000 (all ProDOS8 system files)
                    typ   $ff
                    mx    %11
                    jsr   SaveTextColors
                    jsr   InitTextTools
                    jsr   SetGSText
                    jsr   Setup80Col

                    jsr   DrawSplash
                                                ;jsr   WaitKey

                    jsr   TextColorInit
                    jsr   TextColorSet

                    jsr   DrawMenuBackground
                    jsr   DrawNinjaAnimIn

                    clc
                    xce
                    rep   #$30

                    jsr   PrepareTools
                    jsr   PrepareNTP
                    >>>   PT_GetPrefix          ; returns ptr in A ...
                                                ; >>>   PT_PrintProdosStr ; "/SENSEIPLAY/" is where we start
                    jsr   MenuRefreshDirList
                    brl   MenuLoop


MenuRefreshDirList  mx    %00
                    stz   SL_selected           ; always zero
                    >>>   PT_SetDirListPtr,DirList ; tell PT where we want our directory list
                    >>>   PT_ReadDir            ; returns count in A
                    sta   DirListCount
                    rts


MenuRefreshDirListOnline mx %00
                    stz   SL_selected           ; always zero
                    >>>   PT_SetDirListPtr,DirList ; tell PT where we want our directory list
                    >>>   PT_ReadOnline
                    sta   DirListCount
                    rts


MenuHandlePrefixChange mx %11
                    ldy   #1
                    lda   (0),y                 ; get first char
                    cmp   #'.'                   ; is it "parent"?
                    bne   :normal_dir
:parent_dir         >>>   PT_RemovePrefix
                    mx    %00

                    lda   PT_PREFIX_BUFFER
                    and   #$00FF
                    cmp   #1                    ; if we're at "/" root then read online volumes
                    bne   :read_dir
:read_online        jsr   MenuRefreshDirListOnline
                    brl   MenuLoop
:normal_dir         sep   $30
                    lda   (0)                   ;\
                    and   #$0F                  ; > clear any volume data in high nibble
                    sta   (0)                   ;/
                    >>>   PT_AppendPrefix,0
                    >>>   PT_AppendPrefix,#PrefixSlashStr
:read_dir           >>>   PT_SetPrefix
                    jsr   MenuRefreshDirList
                    brl   MenuLoop
                    sep   $30
                    GOXY  #5;#22
                    clc
                    xce
                    rep   $30
                    lda   #PT_PREFIX_BUFFER
                    >>>   PT_PrintProdosStr     ; todo: is this needed?


SelectorInit        mx    %00
:setup              SL_SETWINDOWPOS 31;11
                    SL_SETWINDOWSIZE 32;10
                    SL_SETRENDERFUNCTION DirectoryRenderItem
                    lda   DirListCount
                    sta   SL_itemscount
                    rts


* assume cursor xy is set for string printing
* a = item index   -  uses 0 and 2 on dp
DirectoryRenderItem mx    %00
                    ldx   #$80
                    stx   2                     ; don't change chars
                    stz   _sl_char_count        ; zero char counter
                    stz   _sl_show_size         ; turn off filesize printing
                    cmp   SL_itemscount         ; see if item exists
                    bcc   :exists
                    bra   :pad_out              ; otherwise just pad out the line


:exists             tax                         ; get item index

                    cpx   SL_selected
                    bne   :not_selected
                    stz   2                     ; turn on char inverter

:not_selected
                    jsr   SetPtr0toDirEntry     ; this calculates the pointer to our entry and stores it at $0
                    sep   #$30

                    lda   (0)                   ; volume "type" is denoted by drive info in high nibble of len byte
                    cmp   #$10
                    bcs   :volume_dir

                    ldy   #$10
                    lda   (0),y                 ; type
:t0                 cmp   #$0F                  ; dir?
                    bne   :not_dir

                    ldy   #1
                    lda   (0),y                 ; get first char
                    cmp   #'.'                   ; is it "parent"?
                    bne   :normal_dir

:parent_dir         PRINTSTR IcoParentString
                    bra   :continue2
:volume_dir         PRINTSTR IcoVolString
                    bra   :continue2
:normal_dir         PRINTSTR IcoDirString
                    bra   :continue2
:not_dir            PRINTSTR IcoNoString
                    dec   _sl_show_size         ; turn on filesize printing
:continue2          lda   #4                    ;\_ inc the character count since we printed...
                    sta   _sl_char_count        ;/    this could be optimized out but I'm leaving it for flexibility.
                    lda   (0)                   ; get len byte
                    and   #$0F
                    tax                         ;  .. counter
                    ldy   #1                    ; start printing at byte 1
:pr_loop            lda   (0),y

                                                ;and                   #%01111111
                    clc                         ;\___  char inverter
                    adc   2                     ;/
                    cmp   #$40
                    bcc   :okVal
                    cmp   #$60
                    bcs   :okVal
                    sec
                    sbc   #$40                  ; deal with mousetext chars


:okVal              jsr   COOT8
                    inc   _sl_char_count
                    iny
                    dex
                    bne   :pr_loop

:pad_out            sep   $30                   ; leave this, could be coming from above 16-bit area
                    lda   SL_windowsize_x
                    sec
                    sbc   _sl_char_count        ; win_x - printed chars = pad amount
                    ldx   _sl_show_size         ; check if we are printing 10 char size string too
                    beq   :no_size_pad
                    sbc   #10

:no_size_pad        tax

:pad_space          lda   #" "
                    jsr   COOT8
                    dex
                    bne   :pad_space

:size_test          lda   _sl_show_size
                    beq   :done

                    ldy   #17                   ; size offset (3-byte)
                    lda   (0),Y
                    sta   HEX24                 ; set conversion bytes
                    iny
                    lda   (0),Y
                    sta   HEX24+1
                    iny
                    lda   (0),Y
                    sta   HEX24+2

                    jsr   HEX24TODEC8
                    jsr   DEC8TOCHAR10R
                    ldx   #0
:loop               lda   CHAR10,x
                    jsr   COOT8
                    inx
                    cpx   #10
                    bne   :loop
:done               rep   $30
                    rts


                    ldy   #19
                    lda   (0),Y
                    sta   HEX24+2               ; set for conv
                                                ;       jsr PrHex
                    ldy   #18
                    lda   (0),Y
                    sta   HEX24+1               ; set for conv
                                                ;      jsr PrHex
                    ldy   #17
                    lda   (0),Y
                    sta   HEX24+0               ; set for conv
                                                ;        jsr PrHex

                    jsr   HEX24TODEC8
                    jsr   DEC8TOCHAR10R
                    lda   #" "
                    jsr   COOT8

                    rep   #$30
                    rts
_sl_char_count      dw    0                     ; used for width checking strings
_sl_show_size       dw    0






******************************************************************  <<<<<<<<<<<<<<
MenuLoop            clc
                    xce
                    rep   #$30

                    jsr   SelectorInit          ; not expensive so just leave it here
                                                ;               jsr   SL_DemoList1Run
                    jsr   SL_CalculateOffset
                    jsr   SL_DrawWindow

:key_handling
                    sep   #$30
                    jsr   TextColorSet          ; I just call this regularly during the menu in case they go into CPanel

                    lda   KEY
                    bpl   MenuLoop
                    sta   STROBE

                    lda   KEY
                    ldx   #0                    ; index/counter
:find_key           cmp   MenuActions,x         ; go through all the keys we know about
                    beq   :found_key
                    inx
                    cpx   #MenuActionsCount
                    bne   :find_key
                    bra   MenuLoop
                                                ;bra   :debug

:found_key          rep   $30
                    txa
                    and   #$00FF
                    asl
                    tay
                    lda   MenuRoutines,y
                    sta   :jump+1

:jump               jsr   $0000                 ; SMC
                    bra   MenuLoop

* :debug              sep   #$30                  ;
*                     pha                         ; hex debug
*                     GOXY  #75;#22               ;
*                     pla                         ;
*                     jsr   PrHex

*                     bra   MenuLoop
******************************************************************  <<<<<<<<<<<<<<



PlayerLoop          mx    %00

:key_loop           jsr   ShowTrackPos
                    jsr   ShowVUs
                    sep   $30
                    lda   KEY
                    bpl   :key_loop
                    sta   STROBE

:cleanup            jsr   DrawMenuBackground
                    jsr   DrawNinjaAnimIn

                    clc
                    xce
                    rep   $30

                    jsr   _NTPstop


                    rts

ShowVUs             clc
                    xce
                    rep   $30
                    jsr   _NTPgetvuptr
                    stx   0
                    sty   2
                    lda   [0]                   ; number of tracks
                    tax                         ; counter
                    ldy   #2                    ; index of first track
:vu_loop
                    lda   [0],y
                    jsr   RenderVUBar

                    jsr   RenderVUVals
                    iny
                    iny
                    dex
                    bne   :vu_loop
                    rts



ShowTrackPos        clc
                    xce
                    rep   $30
                    jsr   _NTPgetsongpos
                    stx   0
                    sty   2
                    sep   $30


                                                ; bg area pre-drawn at start
                    GOXY  #36;#1                ; cursor
                    ldy   #4                    ; pat
                    lda   [0],y
                    jsr   PrHex
                    GOXY  #46;#1                ; cursor
                    ldy   #8                    ; pos
                    lda   [0],y
                    jsr   PrHex


                    rts                         ;; <- remove for additional debugging
                    GOXY  #22;#22               ;;; debug show track pos
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
                    rts                         ; should return in 8-bit




* x = index to a directory entry
SetPtr0toDirEntry   mx    %00
                    lda   #DirList              ; start w pointer at beginning of list
                    cpx   #0
                    beq   :continue             ; no need to add anything
                    clc
:calc_item_start    adc   #DirListEntrySize
                    dex
                    bne   :calc_item_start
:continue           sta   0                     ; address of string at zero
                    rts

MenuEnterSelected   mx    %00
                    jsr   SL_GetSelected
                    tax
                    jsr   SetPtr0toDirEntry

                    sep   $30
:check_is_vol       lda   (0)                   ; volume "type" is denoted by drive info in high nibble of len byte
                    cmp   #$10
                    bcc   :check_is_dir
                    jsr   MenuHandlePrefixChange
:check_is_dir       ldy   #$10
                    lda   (0),y                 ; type

:t0                 cmp   #$0F
                    bne   :not_dir
                    jsr   MenuHandlePrefixChange
                    rts

:not_dir                                        ; Let's load our song if we can
                    jsr   DrawNinjaLoadEyes
                    jsr   SaveDP
                    jsr   LoadNTP
                    jsr   RestoreDP
                    jsr   StartMusic            ; seems to return mx=11
                    bcs   :err
                    clc
                    xce
                    rep   #$30
                    jsr   ParseSongInfo
                    jsr   PlayerUi
                    jsr   PlayerLoop
:err
                    jsr   UnloadNTP
                    rts



NTPFileVerStr       asc   "NTP File Version: "
NTPFileVerByte      asc   " ",00
NTPFileVer          db    0                     ; actual value in case we need to branch on this
NTPNumTracks        db    0
NTPNumInst          db    0
NTPNumPatt          db    0
NTPLenPatt          db    0

ParseSongInfo       mx    %00

                    jsr   SetModZPPtr           ;NEW

                    sep   $30                   ; NEW
                    ldy   #4
                    ldal  [ModZPPtr],Y
                    sta   NTPFileVer
                    clc
                    adc   #"0"
                    sta   NTPFileVerByte
                    iny
                    ldal  [ModZPPtr],Y
                    sta   NTPNumTracks
                    ldal  [ModZPPtr],Y
                    sta   NTPNumInst
                    ldal  [ModZPPtr],Y
                    sta   NTPNumPatt
                    ldal  [ModZPPtr],Y
                    sta   NTPLenPatt

                    rts
PlayerUi            mx    %00                   ; @todo: this is a mess
                    sep   #$30

                    jsr   DrawMenuBackgroundBox
                    PRINTSTRSXY #3;#3;SenseiLogoStrs
                    PRINTSTRXY #30;#1;PatPosString ; title pat/pos (in title bar)
                    PRINTSTRSXY #30;VUBarY-4;NowPlayingStrs
                    PRINTSTRXY #58;#22;NTPFileVerStr ; file version

                    clc
                    xce
                    rep   #$30




                    jsr   SL_GetSelected        ; print name
                    tax
                    jsr   SetPtr0toDirEntry

                    sep   $30
                    GOXY  #32;VUBarY-2
                    rep   $30

                    lda   $0
                    >>>   PT_PrintProdosStr
                    jsr   _NTPgetvuptr

                    stx   0
                    sty   2

:set_vu_x_offset    lda   [0]                   ; number of tracks
                    sta   VUBarCount
                    tax
                    jsr   SetVUBarWidth         ; x is preserved
                    jsr   SetVUBarOffset
                    jsr   RenderVUBoxes
                    rts


MenuActions         db    #'w'
                    db    #'W'
                    db    #$0B                  ; up

                    db    #'s'
                    db    #'S'
                    db    #$0A                  ; dn

                    db    #$0D                  ; enter

                    db    #'q'                   ; quit
                    db    #'Q'

                    db    #'c'                   ; color
                    db    #'C'
                    db    #'b'                   ; background
                    db    #'B'

                    db    #'?'
MenuActionsCount    =     *-MenuActions

MenuRoutines        da    SL_DecSelected
                    da    SL_DecSelected
                    da    SL_DecSelected

                    da    SL_IncSelected
                    da    SL_IncSelected
                    da    SL_IncSelected

                    da    MenuEnterSelected

                    da    QuitRoutine
                    da    QuitRoutine
                                                ;da    QuitRoutine

                    da    DoColor
                    da    DoColor
                    da    DoBGColor
                    da    DoBGColor


                    da    DoHelp

_DPBAK              ds    256
SaveDP              clc
                    xce
                    rep   $30
                    ldx   #$FE
:copy               lda   $0,x
                    sta   _DPBAK,x
                    dex
                    dex
                    bpl   :copy
                    rts
RestoreDP           clc
                    xce
                    rep   $30
                    ldx   #$FE
:copy               lda   _DPBAK,x
                    sta   $0,x
                    dex
                    dex
                    bpl   :copy
                    rts

UnloadNTP           mx    %00
                    clc                         ; I have trust issues..
                    xce
                    rep   #$30
                    ~DisposeHandle BnkMODHnd
                    _Err
                    rts

*********************************************************
LoadNTP             mx    %11

                    *   sep   $30
                    *   lda $0
                    *   ldy $1
                    *   pha
                    *   phy
                    * ;  jsr DrawNinjaBubble
                    * ;  jsr DrawNinjaInPlace
                    *   GOXY #36;#13
                    *   lda #LoadingFileString
                    *   ldy #>LoadingFileString
                    * ;  ldx #36 ; horiz pos
                    * ;  jsr PrintStringsX
                    *   lda 0                         ; filename
                    *   >>> PT_PrintProdosStr
                    *   ply
                    *   pla
                    *   sta $0
                    *   sty $1


                    clc
                    xce
                    rep   $30
                                                ; >>>   PT_GetPrefix            ; returns ptr in A ...
                                                ; >>>   PT_PrintProdosStr ; "/SENSEIPLAY/" is where we start


* NEW STUFF
                    ldy   #19                   ; size offset (3-byte) from file entry
                    lda   (0),Y
                    and   #$00FF
                    tax                         ; 24 bit filesize to x/y
                    ldy   #17
                    lda   (0),Y
                    tay
                    jsr   AllocContiguousPageAlign ; allocate that much
                    sta   BnkMODHnd+2           ; save the handle
                    stx   BnkMODHnd


                    stx   $08                   ; dereference ptr
                    sta   $0a
                    ldy   #0
                    ldal  [$8],Y
                    sta   BnkMODPtr             ; save for easy access
                    ldy   #2
                    ldal  [$8],Y
                    sta   BnkMODPtr+2
                    jsr   SetModZPPtr           ; slower but consistent
                    PT_LoadFilePtrToPtr 0;ModZPPtr ; and load file into allocated RAM
                    rts

ModZPPtr            =     $14                   ; long ptr to mod data
SetModZPPtr         mx    %00
                    lda   BnkMODPtr
                    sta   ModZPPtr
                    lda   BnkMODPtr+2
                    sta   ModZPPtr+2
                    rts



StartMusic          mx    %00
                    ldx   BnkMODPtr
                    ldy   BnkMODPtr+2
                    lda   #0                    ; no channel doubling
                    jsr   _NTPprepare
                    bcc   :ok
                    jsr   HoldUp
                    sec
                    rts

:ok                 lda   #0
                    jsr   _NTPplay
                    sep   $30

                    clc
                    rts

HoldUp              mx    %00
                    sep   $30
                    jsr   DrawOnlyNinjaBackground
                    jsr   DrawNinjaBubble
                    PRINTSTRXY #36;#13;CantPlayFileString
                    PRINTSTRXY #3;#14;NinjaAngryBrows

                    jsr   BorderCops            ; <- anim delay

                    jsr   TextColorSet
:cleanup            jsr   DrawMenuBackground
                    jsr   DrawNinjaInPlace
                    rts

ColorSeq            db    $00
                    db    01
                    db    08
                    db    08
                    db    09
                    db    08
                    db    08
                    db    01
                    db    00
ColorSeqLen         =     *-ColorSeq
ColorSeqStart       db    0
ColorSeqCurIndex    db    0
ColorNext           db    0
ColorUpdateDelay    db    0
; vbl start ; border black ; wait for 0
; line/2 change? get next color; store
; start @ c034=$60 end $e0

BorderCops          mx    %00
                    clc
                    xce
                    sep   #$30
                    lda   #$88                  ;min bar
                    sta   4
                    lda   #$D8                  ;max bar
                    sta   5

                    stz   ColorUpdateDelay
                    stz   ColorSeqStart
                    stz   ColorSeqCurIndex
                    stz   2                     ; 2-3= 16 bit loop counter
                    stz   3
:loop                                           ;stz ColorSeqStart                     ; del
                                                ;stz ColorSeqCurIndex                  ; del
                    INCROLL ColorUpdateDelay;#$80
                    beq   :incstart
                    lda   ColorSeqStart
                    bra   :noinc
:incstart           INCROLL ColorSeqStart;#ColorSeqLen-1 ;\_ bump starting color
:noinc              sta   ColorSeqCurIndex      ;/

                    lda   $c02e
                    cmp   4
                    bcc   :under
                    cmp   5
                    bcs   :over
:lineloop           sta   1
                    jsr   GetNextColor
                    tay

:lineswait          lda   $c02e
                    cmp   1
                    beq   :lineswait
                    sty   $c034
                    cmp   5
                    bcc   :lineloop
:under
:over               stz   $c034


                    lda   KEY
                    bpl   :nokey
                    sta   STROBE
:done16             sep   #$30
                    rts
:nokey
                    rep   #$30
                    INCROLL 2;#$2F00            ; check delay
                    beq   :done16
                    sep   #$30
                    bra   :loop


GetNextColor        mx    %11
                    INCROLL ColorSeqCurIndex;#ColorSeqLen-1 ; cmp to N-1 because index starts at 0
                    tax
                    lda   ColorSeq,X
                    rts

INCROLL             MAC
                    lda   ]1
                    cmp   ]2
                    beq   _roll
_noroll             inc
                    bra   _store
_roll               lda   #0
_store              sta   ]1
                    <<<

                    mx    %11
WaitVBL
:wait_vbl_start     lda   $c019
                    bpl   :wait_vbl_start
:wait_vbl_end       lda   $c019
                    bmi   :wait_vbl_end
                    rts

PrepareNTP          mx    %00
                    jsr   AllocOneBank
                    sta   BnkNTP

                    sta   _NTPprepare+2         ; FIX TABLE
                    sta   _NTPplay+2
                    sta   _NTPstop+2
                    sta   _NTPgetvuptr+2
                    sta   _NTPgete8ptr+2
                    sta   _NTPforcesongpos+2
                    sta   _NTPgetsongpos+2

                    sta   1                     ;\__ build 24 bit address like 00 00 B0 nn
                    stz   0                     ;/
                    PT_LoadFilenameToPtr 'ntpplayer';0 ; address in 0
                    rts                         ; we don't need bank addr any more



                    sep   #$30
                    sec
                    xce
:fo                 inc   $c034
                    bra   :fo


                    jsr   P8Quit

******************************************
* Call this at the start of your program *
******************************************
                    mx    %00
PrepareTools        stz   MasterId              ; I haven't created a new user ID
                    _TLStartUp
                    pha
                    _MMStartUp
                    pla
                    bcc   MM_OK
                    _MTStartUp                  ; If the Memory Manager reported an error, we need to allocate our own memory first.
                    pha                         ; First we need a user ID.
                    pea   $1000
                    _GetMasterId                ; Get me a new user ID (Application)
                    pla
                    sta   MasterId              ; Save it for later
                    pha                         ; Now give us all of bank zero and bank one.
                    pha                         ; Result space
                    pea   $0000
                    pea   $B800                 ; Block size
                    lda   MasterId
                    pha                         ; User ID
                    pea   $C002                 ; Attributes: locked, fixed, absolute
                    pea   $0000
                    pea   $0800                 ; Location (bank 0, $0800-$BFFF)
                    _NewHandle
                    plx
                    ply
                    _Err                        ; This shouldn't happen!
                    sty   Bnk0Hnd
                    stx   Bnk0Hnd+2             ; Save handle to bank 0 memory
                    pha
                    pha                         ; Result space
                    pea   $0000
                    pea   $B800                 ; Block size
                    lda   MasterId
                    pha                         ; User ID
                    pea   $C002                 ; Attributes: locked, fixed, absolute
                    pea   $0001
                    pea   $0800                 ; Location (bank 1, $0800-$BFFF)
                    _NewHandle
                    plx
                    ply
                    _Err                        ; This shouldn't happen!
                    sty   Bnk1Hnd
                    stx   Bnk1Hnd+2             ; Save handle to bank 0 memory
* We have the necessary memory protected.  Start up the memory manager again.
                    pha
                    _MMStartUp
                    pla
                    _Err                        ; This shouldn't happen!
MM_OK               sta   UserId                ; Save the memory ID
                    rts

******************************************
* Basic Error Macro                      *
******************************************
_Err                mac
                    bcc   NoErr
                    do    ]0                    ; (DO if true)
                    jsr   PgmDeath              ;  this is conditionally compiled if
                    str   ]1                    ;  we pass in an error statement
                    else                        ; (ELSE)
                    jmp   PgmDeath0             ;  we just call the simpler error handler
                    fin                         ; (FIN)
NoErr               eom

****************************************
* Fatal Error Handler                  *
****************************************
PgmDeath            tax
                    pla
                    inc
                    phx
                    phk
                    pha
                    bra   ContDeath
PgmDeath0           pha
                    pea   $0000
                    pea   $0000
ContDeath           ldx   #$1503
                    jsl   $E10000

QuitRoutine         mx    %00
                    sec
                    xce
                    sep   #$30
                    bra   P8Quit

******************************************
* Standard ProDOS 8 Quit routine         *
******************************************
                    mx    %11
P8Quit              jsr   P8_MLI_CALL           ; first actual command, call ProDOS vector
                    dfb   $65                   ; with "quit" request ($65)
                    da    QuitParm
                    bcs   Error                 ; what's the point?  ;)
Error               brk   $00                   ; shouldn't ever  here!

QuitParm            dfb   4                     ; number of parameters
                    dfb   0                     ; standard quit type
                    da    $0000                 ; not needed when using standard quit
                    dfb   0                     ; not used
                    da    $0000                 ; not used


MasterId            ds    2
UserId              ds    2
BnkNTP              hex   0000                  ; used for NTP engine
BnkMOD              hex   0000                  ; used for MOD loading... there may be multiple banks but it must be contiguous starting at this one.
BnkMODHnd           hex   00000000
BnkMODPtr           adrl  00000000
Bnk0Hnd             hex   00000000
Bnk1Hnd             hex   00000000



**********************************************************   NINJATRACKERPLUS
** STUB FUNCTIONS
_NTPprepare         jsl   NTPprepare
                    rts
_NTPplay            jsl   NTPplay
                    rts
_NTPstop            jsl   NTPstop
                    rts
_NTPgetvuptr        jsl   NTPgetvuptr
                    rts
_NTPgete8ptr        jsl   NTPgete8ptr
                    rts
_NTPforcesongpos    jsl   NTPforcesongpos
                    rts
_NTPgetsongpos      jsl   NTPgetsongpos
                    rts

NinjaTrackerPlus    =     $0f0000
NTPprepare          =     NinjaTrackerPlus      ; IN: X=low, Y=high
NTPplay             =     NinjaTrackerPlus+3    ; IN: A=0 for loop
NTPstop             =     NinjaTrackerPlus+6
NTPgetvuptr         =     NinjaTrackerPlus+9    ; OUT: X:low, Y=high
NTPgete8ptr         =     NinjaTrackerPlus+12   ; tool does not use
NTPforcesongpos     =     NinjaTrackerPlus+15   ; tool does not use
NTPgetsongpos       =     NinjaTrackerPlus+18   ; tool does not use


DoColor             mx    %00
                    sep   #$30
                    lda   _cur_textcolor
                    clc
                    adc   #$10
                    and   #$F0
:store              ora   _cur_bordercolor      ; pick up bg color
                    sta   _cur_textcolor

                    rep   #$30
                    rts

DoBGColor           mx    %00
                    sep   #$30
                    lda   _cur_bordercolor
                    inc
                    cmp   #$10
                    bne   :store
                    lda   #0
:store              sta   _cur_bordercolor
                    lda   _cur_textcolor
                    and   #$F0
                    ora   _cur_bordercolor      ; pick up bg color
                    sta   _cur_textcolor

                    rep   #$30
                    rts


DoHelp              mx    %00                   ; comes from MenuAction
                    sep   #$30
                    jsr   DrawOnlyNinjaBackground
                    jsr   DrawNinjaBubble
                    PRINTSTRSXY #33;#10;HelpStr0
                    jsr   WaitKey
                    jsr   DrawNinjaBubble
                    PRINTSTRSXY #36;#10;ThankStr0
                    jsr   WaitKey

:cleanup            jsr   DrawMenuBackground
                    jmp   DrawNinjaInPlace

DrawOnlyNinjaBackground mx %11
                    jsr   DrawMenuBackgroundBox
                    PRINTSTRSXY #3;#3;SenseiLogoStrs
                    jmp   DrawNinjaInPlace

DrawMenuBackground  mx    %11
                    jsr   DrawMenuBackgroundBox
                    PRINTSTRSXY #3;#3;SenseiLogoStrs
                    PRINTSTRXY #38;#9;ChooseTrackStr
                    PRINTSTRSXY #29;#10;TrackBoxStrs
                    PRINTSTRXY #68;#21;HelpStr
                    rts

DrawMenuBackgroundBox mx  %11
;                  jsr   text_clear            ; clear screen  <- don't need because we are wall-to-wall :P
                    stz   text_h                ; set top left corner (HOME)
                    stz   text_v
                    lda   #MenuTopStrs
                    ldy   #>MenuTopStrs
                    ldx   #00                   ; horiz pos
                    jsr   PrintStringsX         ; implied rts
                    PRINTSTRXLUP #0;#3;#20;MenuMidStr
                    PRINTSTRXY #0;#23;MenuBotStr
                    rts

DrawSplash          mx    %11
                    jsr   text_clear            ; clear screen
                    stz   $c034
                    stz   $c022
                    PRINTSTRSXY #8;#1;LogoStrs
                    ldx   #$10
                    jsr   FadeDelay
                    PRINTSTRSXY #7;#2;LogoStrs
                    lda   #$50                  ; G
                    jsr   SetColorDelay2
                    PRINTSTRSXY #7;#3;LogoStrs
                    lda   #$10                  ; R
                    jsr   SetColorDelay2
                    PRINTSTRSXY #6;#4;LogoStrs
                    lda   #$90                  ; O
                    jsr   SetColorDelay2
                    PRINTSTRSXY #6;#5;LogoStrs
                    lda   #$D0                  ; Y
                    jsr   SetColorDelay2
                    PRINTSTRSXY #5;#6;LogoStrs
                    lda   #$F0
                    jsr   SetColorDelay2
                    ldx   #$80
                    jsr   FadeDelay
                    jsr   WipeSplash
                    rts
WipeSplash          mx    %11
                    PRINTSTRSXY #4;#7;LogoStrs
                    lda   #$F0
                    jsr   SetColorDelay2
                    PRINTSTRSXY #3;#8;LogoStrs
                    lda   #$D0                  ; Y
                    jsr   SetColorDelay2
                    PRINTSTRSXY #3;#9;LogoStrs
                    lda   #$D0                  ; Y
                    jsr   SetColorDelay2
                    PRINTSTRSXY #2;#10;LogoStrs
                    lda   #$90                  ; Y
                    jsr   SetColorDelay2
                    PRINTSTRSXY #2;#11;LogoStrs
                    lda   #$90                  ; Y
                    jsr   SetColorDelay2
                    PRINTSTRSXY #1;#12;LogoStrs
                    lda   #$10                  ; Y
                    jsr   SetColorDelay2
                    PRINTSTRSXY #1;#13;LogoStrs
                    lda   #$10                  ; Y
                    jsr   SetColorDelay2
                    PRINTSTRSXY #0;#14;LogoStrs
                    lda   #$50                  ; Y
                    jsr   SetColorDelay2
                    PRINTSTRSXY #0;#15;LogoStrs
                    lda   #$50                  ; Y
                    jsr   SetColorDelay2
                    stz   $c022

                    rts

* a=color
SetColorDelay       sta   $c022
                    ldx   #$04
                    jmp   FadeDelay
SetColorDelay2      sta   $c022
                    ldx   #$02
                    jmp   FadeDelay
* x=delay
FadeDelay           mx    %11
:loop               jsr   WaitVBL
                    dex
                    bne   :loop
                    rts

DrawNinjaBubble     mx    %11
                    PRINTSTRSXY #27;#8;NinjaBubble
                    rts

DrawNinjaInPlace    mx    %11
                    ldx   #3
                    ldy   _dnai_y
                    lda   _dnai_y_clip
                    jmp   DrawNinjaXYClip

DrawNinjaLoadEyes   mx    %11
                    PRINTSTRXY #15;#15;NinjaAppleEyesClose
                    rts

DrawNinjaAnimIn     mx    %11
                    lda   #23                   ; start position Y
                    sta   _dnai_y
                    stz   _dnai_y_clip          ; 0 lines
                    lda   #15                   ; ninja height
                    sta   _dnai_y_clip_max

:loop               jsr   WaitVBL

                    ldx   #3
                    ldy   _dnai_y
                    lda   _dnai_y_clip
                    jsr   DrawNinjaXYClip

                    jsr   WaitVBL
                    inc   _dnai_y_clip
                    lda   _dnai_y_clip
                    cmp   _dnai_y_clip_max
                    beq   :done
                    dec   _dnai_y               ; next pass start higher
                    bra   :loop
:done               rts
_dnai_y             db    0                     ; y start
_dnai_y_clip        db    0                     ; current value
_dnai_y_clip_max    db    0                     ; done value


SaveTextColors      mx    %11
                    lda   $c034
                    sta   _bak_bordercolor
                    lda   $c022
                    sta   _bak_textcolor
                    rts

TextColorInit       mx    %11
                    lda   #0
                    sta   _cur_bordercolor
                    lda   #$C0                  ; grn black
                    sta   _cur_textcolor
                    rts

TextColorSet        mx    %11
                    lda   _cur_bordercolor
                    sta   $c034
                    lda   _cur_textcolor
                    sta   $c022
                    rts


TextColorRestore    mx    %11
                    lda   _bak_bordercolor
                    sta   $c022
                    lda   _bak_textcolor
                    sta   $c034
                    rts

_bak_bordercolor    ds    1
_bak_textcolor      ds    1
_cur_bordercolor    ds    1
_cur_textcolor      ds    1




PrefixSlashStr      str   '/'

DirListCount        dw    0

MouseString         asc   '@ABCDEFGHIJKLMNOPQRSTUVWXYZXYXY[\]^_',00
PatPosString        asc   '_',"Pat:    ",'C'," Pos:   ",'Z',00
IcoDirString        asc   'XY'," ",$00
IcoParentString     asc   'KI'," ",$00
IcoVolString        asc   'Z^'," ",$00
IcoNoString         asc   "   ",$00
CantPlayFileString  asc   "Can't play this file!@#!",$00,$00,$00
LoadingFileString   asc   "Loading ",$00,$00,$00

HelpStr0            asc   'J'," ",'K'," Use arrows to navigate.",00
HelpStr1            asc   "  ",'M'," Press Return to play song",'I',00
                    asc   "          or enter directory.",00
                    asc   " ",00
HelpStr2            asc   " 'C' = Change 'C'olor ",00
HelpStr3            asc   " 'B' = Change 'B'ackground",00
HelpStr4            asc   " 'Q' = 'Q'uit",00
                    hex   00,00

ThankStr0           asc   'U'," Written by DiGaRoK ",'H',00
                    asc   " ",00
                    asc   "  ",'@A@',"  Thanks!!! ",'@A@',00
                    asc   "  ",'[',"   Jesse Blue   ",'[',00
                    asc   "  ",'[',"    J.Craft     ",'[',00
                    asc   "  ",'[',"    DWSJason    ",'[',00
                    asc   "  ",'[',"     FatDog     ",'[',00
                    asc   "  ",'['," Antoine Vignau ",'[',00
                    hex   00


LogoStrs            asc   "                                                 ",00
                    asc   "            _              _             ____                            ",00
                    asc   "   ____    (_)  ____      (_)  ____ _   / __/  ____    _____  _____  ___  ",00
                    asc   "  / __ \  / /  / __ \    / /  / __ `/  / /_   / __ \  / ___/ / ___/ / _ \ ",00
                    asc   " / / / / / /  / / / /   / /  / /_/ /  / __/  / /_/ / / /    / /__  /  __/ ",00
                    asc   "/_/ /_/ /_/  /_/ /_/ __/ /   \__,_/  /_/     \____/ /_/     \___/  \___/ ",00
                    asc   "                    /___/",00,00


MenuTopStrs         asc   " ______________________________________________________________________________",00
                    asc   'ZV_@'," v0.1.2",'ZVWVWVWVWVWVWVWVWVWVWVWV_',"Ninjaforce",'ZVWVWVWVWVWVWVWVWV_',"][ infinitum",'ZW_',00
                    asc   'ZLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL_',00
                    hex   00
MenuMidStr          asc   'Z',"                                                                              ",'_',00
MenuBotStr          asc   " ",'LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL'," ",00
SenseiLogoStrs      asc   "   _____                                _     ____     __ ",00
                    asc   "  / ___/  ___    ____    _____  ___    (_)   / __ \   / /  ____ _   __  __ ",00
                    asc   "  \__ \  / _ \  / __ \  / ___/ / _ \  / /   / /_/ /  / /  / __ `/  / / / / ",00
                    asc   " ___/ / /  __/ / / / / (__  ) /  __/ / /   / ____/  / /  / /_/ /  / /_/ / ",00
                    asc   "/____/  \___/ /_/ /_/ /____/  \___/ /_/   /_/      /_/   \__,_/   \__, / ",00
                    asc   "                                                                 /____/  ",00
                    hex   00

ChooseTrackStr      asc   "Choose a track:",00
TrackBoxStrs        asc   " _________________________________",00
                    asc   'Z',"                                 ",'_',00
                    asc   'Z',"                                 ",'_',00
                    asc   'Z',"                                 ",'_',00
                    asc   'Z',"                                 ",'_',00
                    asc   'Z',"                                 ",'_',00
                    asc   'Z',"                                 ",'_',00
                    asc   'Z',"                                 ",'_',00
                    asc   'Z',"                                 ",'_',00
                    asc   'Z',"                                 ",'_',00
                    asc   'Z',"                                 ",'_',00
                    asc   " ",,'LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL',00
                    hex   00
HelpStr             asc   "? - help",00

NinjaBubble         asc   "    _________________________________",00
                    asc   "   ",'Z',"                                 ",'_',00
                    asc   "   ",'Z',"                                 ",'_',00
                    asc   "   ",'Z',"                                 ",'_',00
                    asc   "   ",'Z',"                                 ",'_',00
                    asc   "   ",'Z',"                                 ",'_',00
                    asc   "   ",'Z',"                                 ",'_',00
                    asc   "   /                                 ",'_',00
                    asc   "  /                                  ",'_',00
                    asc   " /                                   ",'_',00
                    asc   "/                                    ",'_',00
                    asc   'LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL',00
                    hex   00
NinjaStrs           asc   "                        ",00
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
                    hex   00
NinjaAppleEyesClose asc   '@',"     ",'@',00
NinjaAngryBrows     asc   "   _.:'|   ,-\---/--|   ",00,00,00
                    mx    %11
; x = x, y=y, clip = ylines to draw before stopping
DrawNinjaXYClip     jsr   SetYClip              ; with A value
                    sty   text_v                ; works
                    lda   #NinjaStrs
                    ldy   #>NinjaStrs
                    jmp   PrintStringsXYClip    ; implied rts


; x = x, y=y, clip = ylines to draw before stopping
DrawLogoXYClip      jsr   SetYClip              ; with A value
                    sty   text_v                ; works
                    lda   #LogoStrs
                    ldy   #>LogoStrs
                    jmp   PrintStringsXYClip    ; implied rts


NowPlayingStrs      asc   "   Now Playing:",00
                    asc   " _________________",00
                    asc   'Z',"                 ",'_',00
                    asc   " ",'LLLLLLLLLLLLLLLLL',00
                    hex   00,00


                    ds    \
DirListMaxEntries   =     256
DirListEntrySize    =     20                    ; 16 name + 1 type + 3 len
DirList             ds    #DirListEntrySize*DirListMaxEntries
WaitKeyColor        mx    %11
                    inc   $c034
WaitKey             mx    %11
:nope               lda   KEY
                    bpl   :nope
                    sta   STROBE
                    rts
KEY                 equ   $C000
STROBE              equ   $C010



                    put   p8tools
                    put   texttools
                    put   scrollist
                    put   vubars
                    dsk   sensei.system
