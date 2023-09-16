********************************************************************************
*                                                                              *
*     _____                                _     ____     __                   *
*    / ___/  ___    ____    _____  ___    (_)   / __ \   / /  ____ _   __  __  *
*    \__ \  / _ \  / __ \  / ___/ / _ \  / /   / /_/ /  / /  / __ `/  / / / /  *
*   ___/ / /  __/ / / / / (__  ) /  __/ / /   / ____/  / /  / /_/ /  / /_/ /   *
*  /____/  \___/ /_/ /_/ /____/  \___/ /_/   /_/      /_/   \__,_/   \__, /    *
*                                                                   /____/     *
*                                                                              *
*  (c) 2021-2023 Digarok / Ninjaforce                                          *
*  NinjaTrackerPlus (NTP) Engine by Jesse Blue of Ninjaforce                   *
********************************************************************************
*                                                                              *
* This program will allow you to try to play any file, but the technical       *
* specification for the NTP filetype is as follows:                            *
*   TYPE: $D5 AUX: $0008 - NTP NinjaTrackerPlus sequence                       *
*   from: https://github.com/a2infinitum/apple2-filetypes                      *
* not at: https://prodos8.com/docs/techref/quick-reference-card/               *
********************************************************************************

                org   $2000                 ; start at $2000 (all ProDOS8 system files)
                typ   $ff
                mx    %11
                jsr   SaveTextColors
                jsr   TextToolsInit
                jsr   SetGSText
                jsr   Setup80Col
                jsr   DrawSplash
                jsr   ResetTextColors
                jsr   DrawMenuBackground
                jsr   DrawNinjaAnimIn

                clc
                xce
                rep   #$30

                jsr   PrepareTools
                jsr   PrepareNTP            ; loads NTPPlayer and patches vectors
                jsr   PrepareGfx            ; load dgr
                >>>   PT_GetPrefix          ; returns ptr in A ...
                                            ; >>>   PT_PrintProdosStr ; "/SENSEIPLAY/" is where we start
                jsr   MenuRefreshDirList
                jsr   DrawMenuLiveElements
                brl   MenuLoop


MenuRefreshDirList mx %00
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
:parent_dir     >>>   PT_RemovePrefix
                mx    %00

                lda   PT_PREFIX_BUFFER
                and   #$00FF
                cmp   #1                    ; if we're at "/" root then read online volumes
                bne   :read_dir
:read_online    jsr   MenuRefreshDirListOnline
                jsr   ShowPrefix
                brl   MenuLoop
:normal_dir     sep   $30
                lda   (0)                   ;\
                and   #$0F                  ; > clear any volume data in high nibble
                sta   (0)                   ;/
                >>>   PT_AppendPrefix,0
                >>>   PT_AppendPrefix,#PrefixSlashStr
:read_dir       >>>   PT_SetPrefix
                jsr   MenuRefreshDirList
                jsr   ShowPrefix
                brl   MenuLoop

PFXLEN          =     #58
ShowPrefix      sep   $30
                GOXY  #20;#2
                PRINTCHARS #" ";#PFXLEN
                GOXY  #20;#2
                rep   $30
                lda   #PT_PREFIX_BUFFER

                >>>   PT_PrintProdosStrLClipses,#PFXLEN
                rts


DebugBrk        mx    %00
                ldal  $00C061               ; this loads both "buttons"/keys
                bpl   :no_option            ; but we only check option
:option         brk   $DB

:no_option      rts

SelectorInit    mx    %00
:setup          SL_SETWINDOWPOS 37;5
                SL_SETWINDOWSIZE 33;15
                SL_SETRENDERFUNCTION DirectoryRenderItem
                lda   DirListCount
                sta   SL_itemscount
                rts


* assume cursor xy is set for string printing
* a = item index   -  uses 0 and 2 on dp
DirectoryRenderItem mx %00
                ldx   #$80
                stx   2                     ; don't change chars
                stz   _sl_char_count        ; zero char counter
                stz   _sl_show_size         ; turn off filesize printing
                cmp   SL_itemscount         ; see if item exists
                bcc   :exists
                bra   :pad_out              ; otherwise just pad out the line


:exists         tax                         ; get item index

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
:t0             cmp   #$0F                  ; dir?
                bne   :not_dir

                ldy   #1
                lda   (0),y                 ; get first char
                cmp   #'.'                   ; is it "parent"?
                bne   :normal_dir

:parent_dir     PRINTSTR IcoParentString
                bra   :continue2
:volume_dir     PRINTSTR IcoVolString
                bra   :continue2
:normal_dir     PRINTSTR IcoDirString
                bra   :continue2
:not_dir        PRINTSTR IcoNoString
                dec   _sl_show_size         ; turn on filesize printing
:continue2      lda   #4                    ;\_ inc the character count since we printed...
                sta   _sl_char_count        ;/    this could be optimized out but I'm leaving it for flexibility.
                lda   (0)                   ; get len byte
                and   #$0F
                tax                         ;  .. counter
                ldy   #1                    ; start printing at byte 1
:pr_loop        lda   (0),y

                                            ;and                   #%01111111
                clc                         ;\___  char inverter
                adc   2                     ;/
                cmp   #$40
                bcc   :okVal
                cmp   #$60
                bcs   :okVal
                sec
                sbc   #$40                  ; deal with mousetext chars


:okVal          jsr   COUT80
                inc   _sl_char_count
                iny
                dex
                bne   :pr_loop

:pad_out        sep   $30                   ; leave this, could be coming from above 16-bit area
                lda   SL_windowsize_x
                sec
                sbc   _sl_char_count        ; win_x - printed chars = pad amount
                ldx   _sl_show_size         ; check if we are printing 10 char size string too
                beq   :no_size_pad
                sbc   #10

:no_size_pad    tax

:pad_space      lda   #" "
                jsr   COUT80
                dex
                bne   :pad_space

:size_test      lda   _sl_show_size
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
:loop           lda   CHAR10,x
                jsr   COUT80
                inx
                cpx   #10
                bne   :loop
:done           rep   $30
                rts


                ldy   #19
                lda   (0),Y
                sta   HEX24+2               ; set for conv

                ldy   #18
                lda   (0),Y
                sta   HEX24+1               ; set for conv

                ldy   #17
                lda   (0),Y
                sta   HEX24+0               ; set for conv


                jsr   HEX24TODEC8
                jsr   DEC8TOCHAR10R
                lda   #" "
                jsr   COUT80

                rep   #$30
                rts
_sl_char_count  dw    0                     ; used for width checking strings
_sl_show_size   dw    0


KEY_ESC         =     $9B                   ;$1B
KEY_LEFT        =     $88                   ;$08
KEY_RIGHT       =     $95                   ;$15
KEY_TAB         =     $89                   ;$09
KEY_ENTER       =     $8D                   ;$0D
KEY_UP          =     $8B                   ;$0B
KEY_DOWN        =     $8A                   ;$0A


****************************************
* Main Menu Loop                       *
****************************************
MenuLoop        rep   #$30

                jsr   SelectorInit          ; not expensive so just leave it here
                jsr   SL_CalculateOffset
                jsr   SL_DrawWindow         ; overdraw
                jsr   DrawNinjaBlinks
:key_handling
                sep   #$30
                jsr   WaitVBL               ; this is to control timing of the blinking, sorry too lazy for interrupts or clock
                jsr   ResetTextColors       ; Called regularly during menu in case they go into CPanel
                lda   KEY
                bpl   MenuLoop

                sta   STROBE
:to_upper       cmp   #"a"
                bcc   :cont
                cmp   #"z"+1
                bcs   :cont
                sec
                sbc   #$20                  ; "a"=$e1 so -$20 is $c1, or "A"
:cont
                ldx   #0                    ; index/counter
:find_action    cmp   MenuActions,x         ; go through all the keys we know about
                beq   :found_action
                inx
                cpx   #MenuActionsCount
                bne   :find_action
*                jsr  PrHex   ; debug
                bra   MenuLoop

:found_action   rep   $30
                txa
                and   #$00FF
                asl
                tay
                lda   MenuRoutines,y
                sta   :jump+1

:jump           jsr   $0000                 ; SMC
                bra   MenuLoop


****************************************
* Music Player Loop                    *
****************************************
PlayerLoop      mx    %00
                inc   SP_ISPLAYING          ; set "true"
:key_loop       rep   $30
                lda   SP_ISPLAYING
                bne   :cont
                lda   SPOPT_PLAY_MULTI
                bne   :next_track
                brl   __cleanup
:next_track     jsr   PlayerNext
:cont           jsr   ShowTrackPos          ; updates SP_ISPLAYING
                lda   SPOPT_GR_PLAYER
                beq   :text_mode
:gr_mode        jsr   ShowGRVUs
                bra   :check_keys
:text_mode      jsr   ShowVUs
:check_keys     jsr   CheckKeyPlayerActions ; returns action location in A or 0 for no action found
                beq   :key_loop
                sta   :jump+1

:jump           jsr   $0000                 ; SMC
                bra   :key_loop


__cleanup       stz   SPOPT_PLAY_MULTI
                sep   $30                   ; so cleanup and exit
                sta   TXTSET                ; text mode
                jsr   TextClear
                jsr   DrawMenuBackground
                jsr   DrawNinjaAnimIn
                jsr   DrawMenuLiveElements
                rep   $30
                jsr   _NTPstop
                jsr   UnloadNTP
                rts


PlayerNext      mx    %00
                pla                         ; clean up return address  because we're not going bacck
                lda   SPOPT_PLAY_MULTI
                beq   PlayerExit
                stz   SP_ISPLAYING
                jsr   _NTPstop
                jsr   UnloadNTP
                jsr   SL_GetSelected        ; set ptr again just in case (bug reported?)
                sta   $10
                jsr   SL_IncSelected
                jsr   SL_GetSelected
                cmp   $10
                beq   :enddir
                jmp   MenuEnterSelected     ; we're jumping back to the main menu
:enddir
                bra   __cleanup             ; or


PlayerExit      mx    %00
                stz   SP_ISPLAYING
                pla                         ; pull return because
                bra   __cleanup             ; will RTS from there


CheckKeyPlayerActions mx %00
                sep   $30
                lda   KEY
                bmi   :key_pressed
                rep   $30                   ; not needed but consistent?
                lda   #0
                rts
:key_pressed    mx    %11
                sta   STROBE
:to_upper       cmp   #"a"                   ; todo refactor
                bcc   :cont
                cmp   #"z"+1
                bcs   :cont
                sec
                sbc   #$20                  ; "a"=$e1 so -$20 is $c1, or "A"
:cont
                ldx   #0                    ; index/counter
:find_action    cmp   PlayerActions,x       ; go through all the keys we know about
                beq   :found_action
                inx
                cpx   #PlayerActionsCount
                bne   :find_action
                rep   $30
                lda   #0                    ; not an action - return
                rts
:found_action   rep   $30
                txa
                and   #$00FF
                asl
                tay
                lda   PlayerRoutines,y
                rts


PlayerToggleGRPlayer mx %11
                rep   $30
                jsr   ToggleGRPlayer
                sep   $30
                jmp   SetPlayerUI

PlayerToggleInfo mx   %11
                rep   $30
                jsr   ToggleInfo
                sep   $30
                jmp   SetPlayerUI

PlayerBack      mx    %11
                rep   $30
                jsr   _NTPgetsongpos
                stx   0
                sty   2
                lda   [0]
                ldy   #8
                lda   [0],Y
                dec
                jsr   _NTPforcesongpos
                rts

PlayerForward   mx    %11
                rep   $30
                jsr   _NTPgetsongpos
                stx   0
                sty   2
                lda   [0]
                ldy   #8
                lda   [0],Y
                inc
                jsr   _NTPforcesongpos
                rts

* PlayerFullInfo  mx %11
*                 sta TXTSET
*                 jsr TextClear
*                 jsr WaitKey
*                 jsr SetModZPPtrToFirstInstrument
*                 jsr SetPlayerUI
*                 rts

* SetModZPPtrToFirstInstrument   mx    %11
*                 rep   $30
*                 brk $ff
*                 jsr   SetModZPPtr

*                 ldy #9
*                 ldal  [ModZPPtr],y
*                 and #$00ff
*                 pha
*                 clc
*                 tya
*                 adc 1,s
*                 ply ; toss
*                 inc ; add +1 for len byte
*                 tay
*                 lda   NTPNumTracks  ; BYTE!
*                 and #$00ff
* :skip_track_data iny
*                 iny
*                 iny  ; +3 bytes
*                 dec
*                 bne :skip_track_data
*                 tya
*                 clc
*                 adc #14 ; skip all the cool instrument info :P
*                 tay
*                 sep $20



*                 ldal  [ModZPPtr],y
*                 sta   NTPLenPatt
*                 rts

ShowGRVUs       mx    %00
                jsr   _NTPgetvuptr
                stx   0
                sty   2
                lda   [0]                   ; number of tracks
                tax                         ; counter
                ldy   #2                    ; index of first track
:vu_loop
                lda   [0],y
                jsr   RenderGRVUBar
                iny
                iny
                dex
                bne   :vu_loop
                rts

ShowVUs         mx    %00
                jsr   _NTPgetvuptr
                stx   0
                sty   2
                lda   [0]                   ; number of tracks
                tax                         ; counter
                ldy   #2                    ; index of first track
:vu_loop
                lda   [0],y
                jsr   RenderVUBar
                * jsr   RenderVUVals
                iny
                iny
                dex
                bne   :vu_loop
                rts


ShowTrackPos    mx    %00
                jsr   _NTPgetsongpos
                stx   0
                sty   2
                lda   [0]
                sta   SP_ISPLAYING
                sep   $30
                lda   SPOPT_INFO
                beq   :check_text
:plus4          GOXY  #8;#22
                ldy   #8                    ; pos
                lda   [0],y
                jsr   PRBYTEDECPADH
                GOXY  #8;#23
                ldy   #4                    ; pat
                lda   [0],y
                jsr   PRBYTEDECPADH
:check_text     lda   SPOPT_GR_PLAYER
                bne   :exit
                GOXY  #35;#1                ; cursor
                ldy   #8                    ; pos
                lda   [0],y
                jsr   PRBYTEDECPADH

                GOXY  #45;#1                ; cursor
                ldy   #4                    ; pat
                lda   [0],y
                jsr   PRBYTEDECPADH

:exit           rep   $30
                rts


* x = index to a directory entry
SetPtr0toDirEntry mx  %00
                lda   #DirList              ; start w pointer at beginning of list
                cpx   #0
                beq   :continue             ; no need to add anything
                clc
:calc_item_start adc  #DirListEntrySize
                dex
                bne   :calc_item_start
:continue       sta   0                     ; address of string at zero
                rts

* sets path to /
VolsSelected    mx    %00
                jsr   PT_PrefixSlash
                >>>   PT_SetPrefix
                jsr   MenuRefreshDirListOnline
                jsr   ShowPrefix
                brl   MenuLoop

PlayDirSelected mx    %00
                ldal  $00C060               ; this loads both "buttons"/keys
                bmi   :no_option            ; but we only check option
                brl   MenuLoop
:no_option
                lda   #1
                sta   SPOPT_PLAY_MULTI
                jsr   SL_GetSelected
                tax
                jsr   SetPtr0toDirEntry
                sep   $30
:check_is_vol   lda   (0)                   ; volume "type" is denoted by drive info in high nibble of len byte
                cmp   #$10
                bcc   :check_is_dir
                rts                         ; nope - we don't handle entering vols on multi-play
:check_is_dir   ldy   #$10
                lda   (0),y                 ; type

:t0             cmp   #$0F
                bne   :is_file
                rts                         ; nope - we don't handle entering dirs on multi-play
:is_file                                    ; Let's load our song if we can
                jsr   DrawNinjaEyesApple
                jsr   SaveDP
                jsr   LoadNTP
                bcc   :play
:load_err       jsr   RestoreDP
                rts
:play           jsr   RestoreDP
                jsr   StartMusic            ; seems to return mx=11
                bcs   :err
                rep   #$30
                jsr   ParseSongInfo
                jsr   SetPlayerUI
                jsr   PlayerLoop
:err
                jsr   UnloadNTP
                rts





MenuEnterSelected mx  %00
                jsr   SL_GetSelected
                tax
                jsr   SetPtr0toDirEntry

                sep   $30
:check_is_vol   lda   (0)                   ; volume "type" is denoted by drive info in high nibble of len byte
                cmp   #$10
                bcc   :check_is_dir
                jsr   MenuHandlePrefixChange
:check_is_dir   ldy   #$10
                lda   (0),y                 ; type

:t0             cmp   #$0F
                bne   :is_file
                jsr   MenuHandlePrefixChange
                rts

* LOAD Initiated                            ; <<<------------- !!!!
:is_file                                    ; Let's load our song if we can
                lda   SPOPT_PLAY_MULTI      ; this also gets called from play-multi
                beq   :play_single
                lda   SPOPT_GR_PLAYER
                bne   :skip_text
                GOXY  #0;#15                ; one line abox separator
                PRINTCHARS #" ";#80          ; just clear the whole line :P
:skip_text
                sta   MIXSET
                jsr   TextClearPlus4
                PRINTSTRXY #25;#22;#LoadingStrs

                rep   $30
                jsr   SL_GetSelected        ; set ptr again just in case (bug reported?)
                tax
                jsr   SetPtr0toDirEntry

                lda   $0
                >>>   PT_PrintProdosStr     ; <<<--- filename
                sep   $30

                bra   :skip

:play_single    jsr   DrawNinjaEyesApple
:skip           jsr   SaveDP
                jsr   LoadNTP
                bcc   :play
:load_err       jsr   RestoreDP
                rts
:play           jsr   RestoreDP
                jsr   StartMusic            ; seems to return mx=11
                bcs   :err
                rep   #$30
                jsr   ParseSongInfo
                jsr   SetPlayerUI
                jsr   PlayerLoop
:err
                jsr   UnloadNTP
                rts

SetPlayerUI     mx    %00
                lda   SPOPT_GR_PLAYER
                beq   :text_ui
:dgr_ui         sep   $30
                jsr   DL_SetDLRMode
                jsr   PlayerGui             ;returns with mx %00
                sep   $30
                bra   :cont
:text_ui        sta   TXTSET
                jsr   PlayerTui
:cont           lda   SPOPT_INFO
                beq   :title
:draw_info      sta   MIXSET
                jsr   TextClearPlus4
                jsr   ShowPlus4SongInfo
                PRINTSTRSXY #3;#22;PatPosBStrings ; title pat/pos (in bottom)

:title          jsr   DrawTitle             ; it will figure out if it has anything to render
                rts


PlayerTui       mx    %00
                sep   #$30
                jsr   TextClear

                PRINTSTRSXY #3;#3;SenseiLogoStrs
                PRINTSTRSXY #25;#0;PatPosStrings ; title pat/pos (in title bar)
                PRINTSTRSXY #0;#16;RanDumbSeparator ; grid lines

                rep   $30
                jsr   SetupVUBars
                jsr   RenderVUBoxes
                rts

SetupVUBars     mx    %00
                jsr   _NTPgetvuptr
                stx   0
                sty   2
:set_vu_x_offset lda  [0]                   ; number of tracks
                sta   VUBarCount
                tax
                jsr   SetVUBarWidth         ; x is preserved
                jsr   SetVUBarOffset
                rts

ParseSongInfo   mx    %00
                jsr   SetModZPPtr

                sep   $30
                ldy   #4
                ldal  [ModZPPtr],y
                sta   NTPFileVer
                iny
                ldal  [ModZPPtr],y
                sta   NTPNumTracks
                iny
                ldal  [ModZPPtr],y
                sta   NTPNumInst
                iny
                ldal  [ModZPPtr],y
                sta   NTPNumPatt
                iny
                ldal  [ModZPPtr],y
                sta   NTPLenPatt
                rts

NTPFileVer      db    0                     ; actual value in case we need to branch on this
NTPNumTracks    db    0
NTPNumInst      db    0
NTPNumPatt      db    0
NTPLenPatt      db    0


ShowPlus4SongInfo mx  %11
                PRINTSTRSXY INFO_OFFSET_X;INFO_OFFSET_Y+2;FileInfoDetStrs

                GOXY  #INFO_OFFSET_X+10;#INFO_OFFSET_Y+2
                lda   NTPNumTracks
                jsr   PRBYTEDEC

                GOXY  #INFO_OFFSET_X+22;#INFO_OFFSET_Y+2
                lda   NTPNumInst
                jsr   PRBYTEDEC

                GOXY  #INFO_OFFSET_X+10;#INFO_OFFSET_Y+3
                lda   NTPNumPatt
                jsr   PRBYTEDEC

                GOXY  #INFO_OFFSET_X+22;#INFO_OFFSET_Y+3
                lda   NTPLenPatt
                jsr   PRBYTEDEC


                PRINTSTRXY #38;#23;NTPVerStr
                GOXY  #42;#23
                lda   NTPFileVer
                jsr   PRBYTEDEC

                GOXY  #30;#22
                rep   $30
                jsr   SL_GetSelected        ; set ptr again just in case (bug reported?)
                tax
                jsr   SetPtr0toDirEntry
                sep   $30
                lda   ($0)                  ; get len byte
                jsr   GoXCenter
                rep   $30
                lda   $0
                >>>   PT_PrintProdosStr     ; <<<--- filename
                sep   $30
                rts


DrawTitle       mx    %11
                lda   NTPFileVer
                cmp   #$2                   ; version 2 or above??
                bcs   :v2up
                rts
:v2up
                ldy   #9                    ; in v2
                ldal  [ModZPPtr],Y          ; title len byte
                beq   :done_title
                pha
                inc
                inc                         ; +2 for quotes
                jsr   GoXCenter
                lda   SPOPT_GR_PLAYER
                beq   :no_gr
                lda   SPOPT_INFO
                bne   :gr_mix
:gr             pla
                rts
:gr_mix         lda   #21
                bra   :set_y
:no_gr          lda   #15                   ; y=15 (above lines)
:set_y          sta   text_v
                jsr   PrQuote

                plx                         ; count in x
                ldy   #10
:print_title    cpx   #0
                beq   :q
                lda   [ModZPPtr],y
                ora   #%1000_0000
                jsr   COUT80
                iny
                dex
                bra   :print_title
:q              jsr   PrQuote
:done_title     rts



**  A=C1 Z=DA  a=E1  z=FA
MenuActions     db    #KEY_UP               ; up
                db    #KEY_DOWN             ; dn
                db    #KEY_TAB              ; tab
                db    #KEY_ENTER            ; enter
                db    #"P"                   ; play dir/multi
                db    #"Q"                   ; quit

                db    #"D"                   ; double
                db    #"L"                   ; loop
                db    #"G"                   ; gr
                db    #"I"                   ; info


                db    #"?"
MenuActionsCount =    *-MenuActions

MenuRoutines    da    SL_DecSelected
                da    SL_IncSelected
                da    VolsSelected
                da    MenuEnterSelected
                da    PlayDirSelected

                da    QuitRoutine

                da    ToggleDoubler
                da    ToggleLooping
                da    ToggleGRPlayer
                da    ToggleInfo

                da    DoHelp
PlayerActions   db    #KEY_RIGHT
                db    #KEY_LEFT
*                db    #"F"                   ; full info
                db    #"G"                   ; gr
                db    #"I"                   ; info
                db    #"N"                   ; next
                db    #" "                   ; next
                db    #KEY_ENTER            ; next
                db    #KEY_ESC

PlayerActionsCount =  *-PlayerActions
PlayerRoutines
                da    PlayerForward
                da    PlayerBack
*                da    PlayerFullInfo
                da    PlayerToggleGRPlayer
                da    PlayerToggleInfo
                da    PlayerNext
                da    PlayerNext
                da    PlayerNext
                da    PlayerExit


** SAVE/RESTORE DIRECT PAGE / ZP
SaveDP          rep   $30
                ldx   #$FE
:copy           lda   $0,x
                sta   _DPBAK,x
                dex
                dex
                bpl   :copy
                rts


RestoreDP       rep   $30
                ldx   #$FE
:copy           lda   _DPBAK,x
                sta   $0,x
                dex
                dex
                bpl   :copy
                rts


UnloadNTP       mx    %00
                clc                         ; I have trust issues..
                xce
                rep   #$30
                ~DisposeHandle BnkMODHnd
                _Err
                rts

*********************************************************
LoadNTP         mx    %11
                rep   $30
                                            ; >>>   PT_GetPrefix            ; returns ptr in A ...
                                            ; >>>   PT_PrintProdosStr ; "/SENSEIPLAY/" is where we start
                ldy   #19                   ; size offset (3-byte) from file entry
                lda   (0),Y
                and   #$00FF
                tax                         ; 24 bit filesize to x/y
                ldy   #17
                lda   (0),Y
                tay
                jsr   AllocContiguousPageAlign ; allocate that much
                cpx   #$FFFF
                bne   :noerr
                jsr   NinjaErrCantAlloc
                sec
                rts

:noerr          sta   BnkMODHnd+2           ; save the handle
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
                clc
                rts

SetModZPPtr     mx    %00
                lda   BnkMODPtr
                sta   ModZPPtr
                lda   BnkMODPtr+2
                sta   ModZPPtr+2
                rts
ModZPPtr        =     $14                   ;{DP} long ptr to mod data


StartMusic      mx    %00
                ldx   BnkMODPtr
                ldy   BnkMODPtr+2
                lda   SPOPT_DOUBLE_CHANS    ; configurable (0=off/1=on)
                jsr   _NTPprepare
                bcc   :ok
                jsr   NinjaErrCantPlay
                sec
                rts

:ok             lda   SPOPT_LOOP_SONG
                eor   #%1                   ; it's reversed to NTPplay
                jsr   _NTPplay
                sep   $30
                clc
                rts


NinjaErrCantAlloc mx  %00
                xba                         ; change order for printing
                pha
                sep   $30
                jsr   DrawOnlyNinjaBackground
                jsr   DrawNinjaBubble
                PRINTSTRXY #36;#8;CantAllocMemString
                PRINTSTRXY #3;#12;NinjaAngryBrows
                GOXY  #46;#17
                pla
                jsr   PrHex
                pla
                jsr   PrHex
                jsr   BorderCops            ; <- anim delay
                bra   NinjaErrCleanupReturn


NinjaErrCantPlay mx   %00
                sep   $30
                pha                         ; just save error code as 8-bit ;)
                jsr   DrawOnlyNinjaBackground
                jsr   DrawNinjaBubble
                PRINTSTRXY #35;#8;CantPlayFileString
                PRINTSTRXY #3;#12;NinjaAngryBrows
                GOXY  #36;#11
                pla                         ; err code
                dec                         ; natural number to zero-index
                asl
                tax
                ldy   NTPPrep_Errs+1,x
                lda   NTPPrep_Errs,x
                ldx   #35                   ; x position
                jsr   PrintStringsX
                jsr   BorderCops            ; <- anim delay
*                 bra   NinjaErrCleanupReturn

NinjaErrCleanupReturn
                jsr   ResetTextColors
                jsr   DrawMenuBackground
                jsr   DrawMenuLiveElements
                jmp   DrawNinjaInPlace


ColorSeq        db    $00
                db    01
                db    08
                db    08
                db    09
                db    08
                db    08
                db    01
                db    00
ColorSeqLen     =     *-ColorSeq
ColorSeqStart   db    0
ColorSeqCurIndex db   0
ColorNext       db    0
ColorUpdateDelay db   0
; vbl start ; border black ; wait for 0
; line/2 change? get next color; store
; start @ c034=$60 end $e0

BorderCops      mx    %00
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
:loop
                INCROLL ColorUpdateDelay;#$80
                beq   :incstart
                lda   ColorSeqStart
                bra   :noinc
:incstart       INCROLL ColorSeqStart;#ColorSeqLen-1 ;\_ bump starting color
:noinc          sta   ColorSeqCurIndex      ;/

                lda   $c02e
                cmp   4
                bcc   :under
                cmp   5
                bcs   :over
:lineloop       sta   1
                jsr   GetNextColor
                tay

:lineswait      lda   $c02e
                cmp   1
                beq   :lineswait
                sty   $c034
                cmp   5
                bcc   :lineloop
:under
:over           stz   $c034

                lda   KEY
                bpl   :nokey
                sta   STROBE
:done16         sep   #$30
                rts
:nokey
                rep   #$30
                INCROLL 2;#$5000            ; check delay
                beq   :done16
                sep   #$30
                bra   :loop


GetNextColor    mx    %11
                INCROLL ColorSeqCurIndex;#ColorSeqLen-1 ; cmp to N-1 because index starts at 0
                tax
                lda   ColorSeq,X
                rts

INCROLL         MAC
                lda   ]1
                cmp   ]2
                beq   _roll
_noroll         inc
                bra   _store
_roll           lda   #0
_store          sta   ]1
                <<<


PrepareNTP      mx    %00
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

PrepareGfx      mx    %00                   ; expects you to still have the 24-bit pointer from PrepareNTP set!
                lda   #$D000
                sta   $0
                PT_LoadFilenameToPtr 'dgr.radio.dgr';0
                rts



******************************************
* Call this at the start of your program *
******************************************
                mx    %00
PrepareTools    stz   MasterId              ; I haven't created a new user ID
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
MM_OK           sta   UserId                ; Save the memory ID
                rts

******************************************
* Basic Error Macro                      *
******************************************
_Err            mac
                bcc   NoErr
                do    ]0                    ; (DO if true)
                jsr   PgmDeath              ;  this is conditionally compiled if
                str   ]1                    ;  we pass in an error statement
                else                        ; (ELSE)
                jmp   PgmDeath0             ;  we just call the simpler error handler
                fin                         ; (FIN)
NoErr           eom

****************************************
* Fatal Error Handler                  *
****************************************
PgmDeath        tax
                pla
                inc
                phx
                phk
                pha
                bra   ContDeath
PgmDeath0       pha
                pea   $0000
                pea   $0000
ContDeath       ldx   #$1503
                jsl   $E10000

QuitRoutine     mx    %00
                sep   #$30
                brl   PT_Quit


MasterId        ds    2
UserId          ds    2
BnkNTP          hex   0000                  ; used for NTP engine
BnkMOD          hex   0000                  ; used for MOD loading... there may be multiple banks but it must be contiguous starting at this one.
BnkMODHnd       hex   00000000
BnkMODPtr       adrl  00000000
Bnk0Hnd         hex   00000000
Bnk1Hnd         hex   00000000



******************************************
* NINJATRACKERPLUS Stub Functions        *
******************************************
_NTPprepare     jsl   NTPprepare
                rts
_NTPplay        jsl   NTPplay
                rts
_NTPstop        jsl   NTPstop
                rts
_NTPgetvuptr    jsl   NTPgetvuptr
                rts
_NTPgete8ptr    jsl   NTPgete8ptr
                rts
_NTPforcesongpos jsl  NTPforcesongpos
                rts
_NTPgetsongpos  jsl   NTPgetsongpos
                rts

NinjaTrackerPlus =    $0f0000
NTPprepare      =     NinjaTrackerPlus      ; IN: X=low, Y=high
NTPplay         =     NinjaTrackerPlus+3    ; IN: A=0 for loop
NTPstop         =     NinjaTrackerPlus+6
NTPgetvuptr     =     NinjaTrackerPlus+9    ; OUT: X:low, Y=high
NTPgete8ptr     =     NinjaTrackerPlus+12   ; tool does not use
NTPforcesongpos =     NinjaTrackerPlus+15   ; tool does not use
NTPgetsongpos   =     NinjaTrackerPlus+18   ; tool does not use



******************************************
* SenseiPlay Options                     *
******************************************

** ONLY SET PRIOR
SPOPT_DOUBLE_CHANS dw 0                     ; default is off (word because we load a 16 bit value)
SPOPT_LOOP_SONG dw    0
SPOPT_PLAY_MULTI dw   0                     ; play through dir, starting with current selection
** ONLY SET DURING
SP_ISPLAYING    dw    0                     ; used to detect when we need to stop a song looping
** SET ANY TIME
SPOPT_GR_PLAYER dw    0                     ; 0 = off/text   1 = on    2? 3?
SPOPT_INFO      dw    0                     ; 0 = off/no info/full graphics   1 = show mix mode info at bottom
SPOPT_FADE_SONG dw    0                     ; try to fade it out after it is "stopped"

DrawCheckboxes  mx    %00
                STATUSBOX SPOPT_DOUBLE_CHANS;#1;#22
                STATUSBOX SPOPT_LOOP_SONG;#22;#22
                STATUSBOX SPOPT_GR_PLAYER;#36;#22
                STATUSBOX SPOPT_INFO;#51;#22
                rts

ToggleDoubler   mx    %00
                TOGGLE SPOPT_DOUBLE_CHANS
                jmp   DrawCheckboxes

ToggleLooping   mx    %00
                TOGGLE SPOPT_LOOP_SONG
                jmp   DrawCheckboxes


ToggleGRPlayer  mx    %00
                TOGGLE SPOPT_GR_PLAYER
                jmp   DrawCheckboxes

ToggleInfo      mx    %00
                TOGGLE SPOPT_INFO
                jmp   DrawCheckboxes


** Toggle between 0 and 1... m can be short or long, but pass location in long X
** Macro call: `TOGGLE SomeOption`
TOGGLE          MAC
                ldx   #]1
                jsr   Toggle
                EOM
                mx    %00
Toggle          lda   $0000,x
                eor   #%1
                sta   $0000,x
                rts

** Helper to draw little checkboxes
** Macro call:  `STATUSBOX  MyBoolean;#XLOC;#YLOC`
STATUSBOX       MAC
                sep   #$30
                GOXY  ]2;]3
                lda   ]1
                jsr   _STATUSBOX
                EOM
_STATUSBOX      mx    %11
                bne   :not_zero
                PRINTSTR OpenBoxStr
                bra   :exit
:not_zero       PRINTSTR CloseBoxStr
:exit           rep   #$30
                rts


DoHelp          mx    %00                   ; comes from MenuAction
                sep   #$30
                jsr   DrawOnlyNinjaBackground
                jsr   DrawNinjaBubble
                PRINTSTRSXY #33;#6;HelpStrs0
                PRINTSTRSXY #35;#12;HelpStrs1
                jsr   WaitKey
                jsr   DrawNinjaBubble
                PRINTSTRSXY #33;#6;HelpStrs2
                jsr   WaitKey
                jsr   DrawNinjaBubble
                PRINTSTRSXY #40;#6;ThankStrs0
                PRINTSTRSXY #44;#10;ThankStrs1
                jsr   Digawait              ; special wait
                jsr   DrawNinjaBubble
                PRINTSTRSXY #34;#6;MoreTracksStrs
                jsr   WaitKey

:cleanup        jsr   DrawMenuBackground
                jsr   DrawMenuLiveElements
                jmp   DrawNinjaInPlace

Digawait        mx    %11
:loop           PRINTSTRXY #53;#6;Dig1
                ldx   #$10                  ; DELAY
                jsr   FadeDelay
                bcs   :done
                PRINTSTRXY #53;#6;Dig2
                ldx   #$10                  ; DELAY
                jsr   FadeDelay
                bcs   :done
                PRINTSTRXY #53;#6;Dig3
                ldx   #$10                  ; DELAY
                jsr   FadeDelay
                bcs   :done
                bra   :loop

:done           rts


** This just copies our DGR image
* 0=src0 8=src1   4=dest0
PlayerGui       mx    %11
                rep   $30
                lda   BnkNTP                ; build a bunch of 24-bit (long) ptrs
                sta   1
                sta   9
                lda   #$D000                ; loads here in ntp bank
                sta   0
                lda   #$D3C0                ; aux (or main, can't remember)
                sta   8

                stz   4
                stz   6

                stz   $c
                lda   #1
                sta   $e

                stz   $10                   ; line counter
:linelp
                lda   $10
                asl                         ; *2
                tax
                lda   LineTable,x
                sta   $4
                sta   $C
                ldy   #0
:copypx         ldal  [$0],y
                stal  [$4],y
                ldal  [$8],y
                stal  [$c],y
                iny
                iny
                cpy   #40                   ; one line o' pixels
                bne   :copypx

:updatesrcptr   lda   $0
                clc
                adc   #40
                sta   $0
                lda   $8
                clc
                adc   #40
                sta   $8

                inc   $10                   ; next line
                lda   $10
                cmp   #24
                bne   :linelp

                jsr   SetupVUBars
                rts

WaitVBL         mx    %11
:wait_vbl_start lda   $c019
                bpl   :wait_vbl_start
:wait_vbl_end   lda   $c019
                bmi   :wait_vbl_end
                rts

DL_SetDLRMode   mx    %11
                lda   LORES                 ;set lores
                lda   SETAN3                ;enables DLR
                sta   SET80VID

                sta   C80STOREON            ; enable aux/page1,2 mapping
                sta   MIXCLR                ;make sure graphics-only mode
                rts

DrawOnlyNinjaBackground mx %11
                jsr   DrawMenuBackgroundBox
                jmp   DrawNinjaInPlace

DrawMenuBackground mx %11
                jsr   DrawMenuBackgroundBox
                PRINTSTRSXY #35;#4;TrackBoxStrs
                PRINTSTRXY #0;#22;HelpFooterStr0
                PRINTSTRXY #0;#23;HelpFooterStr1
                rts

DrawMenuLiveElements mx %11
                jsr   DrawCheckboxes
                jsr   ShowPrefix
                sep   $30
                rts


DrawMenuBackgroundBox mx %11
                PRINTSTRSXY #0;#0;MenuBoxStrs
                rts

******************************************
* Intro Splash Animation                 *
******************************************
DrawSplash      mx    %11
                jsr   TextClear             ; clear screen
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

                ldx   #$40                  ; DELAY
                jsr   FadeDelay
                bcc   WipeSplash            ; clear? then keep animating
                rts                         ; otherwise, they hit key - skip


WipeSplash      mx    %11
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
                jsr   TextClear
                rts

* a=color
SetColorDelay2  sta   $c022
                ldx   #$02
                jmp   FadeDelay
* x=delay
FadeDelay       mx    %11
:loop           jsr   WaitVBL
                lda   KEY
                bpl   :nope
                sta   STROBE
                sec
                rts
:nope           dex
                bne   :loop
                clc
                rts

DrawNinjaBubble mx    %11
                PRINTSTRSXY #27;#4;NinjaBubble
                rts

DrawNinjaInPlace mx   %11
                ldx   #3
                ldy   _dnai_y
                lda   _dnai_y_clip
                jmp   DrawNinjaXYClip

DrawNinjaEyesApple mx %11
                PRINTSTRXY #15;#13;NinjaEyesApple
                rts

DrawNinjaEyesOpen mx  %11
                PRINTSTRXY #15;#13;NinjaEyesOpen
                rts

DrawNinjaEyesClosed mx %11
                PRINTSTRXY #15;#13;NinjaEyesClose
                rts

* "random" blink, and a chance for a second blink, but not too often
* ninjas don't get dry eyes, they just take microsleeps before slaying enemies
DrawNinjaBlinks mx    %00
                lda   _blink_delay          ; already 0 skip
                beq   :chk_blink
                dec   _blink_delay
                bne   :chk_blink            ; didn't hit 0 skip
:unblink        sep   $30                   ; just hit 0 so open dem eyes
                jsr   DrawNinjaEyesOpen
                rep   $30

                jsr   RandLFSR16            ; roll dice for a second blink
                cmp   #$0700
                bcs   :chk_blink
                lda   #$04                  ; second blink activated!
                sta   _blink_counter

:chk_blink      lda   _blink_counter
                beq   :got_blink
                dec   _blink_counter
                rts
:got_blink      sep   $30
                jsr   DrawNinjaEyesClosed
                rep   $30
                lda   #$0002
                sta   _blink_delay
                jsr   RandLFSR16
                and   #$07FF                ; blink frequency
                sta   _blink_counter
                rts
_blink_counter  dw    $0000
_blink_delay    dw    $0000
_blink_rep      dw    $0

DrawNinjaAnimIn mx    %11
                lda   #21                   ; start position Y
                sta   _dnai_y
                stz   _dnai_y_clip          ; 0 lines
                lda   #15                   ; ninja height
                sta   _dnai_y_clip_max

:loop           jsr   WaitVBL

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
:done           rts
_dnai_y         =     $20                   ;{DP} y start
_dnai_y_clip    =     $21                   ;{DP} current value
_dnai_y_clip_max =    $22                   ;{DP} done value

                mx    %11
; x = x, y=y, clip = ylines to draw before stopping
DrawNinjaXYClip jsr   SetYClip              ; with A value
                sty   text_v                ; works
                lda   #NinjaStrs
                ldy   #>NinjaStrs
                jmp   PrintStringsXYClip    ; implied rts


; x = x, y=y, clip = ylines to draw before stopping
DrawLogoXYClip  jsr   SetYClip              ; with A value
                sty   text_v                ; works
                lda   #LogoStrs
                ldy   #>LogoStrs
                jmp   PrintStringsXYClip    ; implied rts


* "Random" 16-bit number via Linear Feedback Shift Register
RandLFSR16      mx    %00
                lda   _rl16seed
                lsr
                bcc   :noeor
                eor   #$B400                ; <- courtesy JBrooksBSI
:noeor          sta   _rl16seed
                rts
_rl16seed       dw    0001


WaitKeyColor    mx    %11
                inc   $c034
WaitKey         mx    %11
:nope           lda   KEY
                bpl   :nope
                sta   STROBE
                rts

SaveTextColors  mx    %11
                lda   $c034
                sta   _bak_bordercolor
                lda   $c022
                sta   _bak_textcolor
                rts

ResetTextColors mx    %11
                lda   _bak_bordercolor
                sta   $c034
                lda   _bak_textcolor
                sta   $c022
                rts
_bak_bordercolor ds   1
_bak_textcolor  ds    1


PrefixSlashStr  str   '/'
DirListCount    dw    0


* MouseString     asc   '',00,'@ABCDEFGHIJKLMNOPQRSTUVWXYZXYXY[\]^_',00
PatPosBStrings  asc   "Pos:",00
                asc   "Pat:",00,00
PatPosStrings   asc   " ___________________________",00
PatPosString    asc   'ZVWVW_',"Pos:    ",'C'," Pat:   ",'ZVWVW_',00
PatPosStringEnd asc   " ",'LLLLLLLLLLLLLLLLLLLLLLLLLLL',00,00
IcoDirString    asc   'XY'," ",$00
IcoParentString asc   'KI'," ",$00
IcoVolString    asc   'Z^'," ",$00
IcoNoString     asc   "   ",$00
CantPlayFileString asc "Can't play this file!!",$00,
CantAllocMemString asc "Can't allocate memory!!",$00
NTPPrep_Errs    da    NTPPrep_ErrMsg1,NTPPrep_ErrMsg2,NTPPrep_ErrMsg3
NTPPrep_ErrMsg1 asc   "NTPErr1: Not an NTP file",00
                asc   "         Couldn't find header",00,00
NTPPrep_ErrMsg2 asc   "NTPErr2: File version not supported",00,00
NTPPrep_ErrMsg3 asc   "NTPErr3: Disable channel doubling",'I',00
                asc   "         Max 7 channels for effect.",00,00

* HelpFooterStr0  asc   'J'," ",'K'," to navigate.  ",'M'," Return to play song",'I',00
HelpFooterStr0  asc   'Z\_',"Double Channels   ",'Z\_',"Loop NTP   ",'Z\_',"GR Player   ",'Z\_',"Info    ",'A',"-Play Multi ",00
HelpFooterStr1  asc   "   ",'L',"                    ",'L',"             ",'L',"              ",'L',"         ",'L',00
CloseBoxStr     asc   '^'," ",00
OpenBoxStr      asc   '\_',00

HelpStrs0       asc   "MENU KEYS:",00
                asc   " ",00
                asc   "Arrows ",'J'," ",'K',"  = navigate",00
                asc   "Return   ",'M',"  = play song / change dir.",00
                asc   "Tab    ",'U_',"   = go to prodos volumes",00,00
HelpStrs1
                asc   "'L'  =  Allow songs to loop",00
                asc   "'D'  =  Stereo Double Channels",00
                asc   "'G'  =  Show 'Graphical' Player",00
                asc   "'I'  =  Show Song Information",00
                asc   "'Q'  = 'Q'uit",00
                hex   00,00

HelpStrs2       asc   "PLAYER KEYS:",00
                asc   " ",00
HelpStrs3
                asc   " 'G'  =  Show 'Graphical' Player",00
                asc   " 'I'  =  Show Song Information",00
                asc   " 'N'  =  Next Song (in Multi mode)",00
                asc   " spc  =  Next Song (in Multi mode)",00
                asc   " esc  =  Exit Player",00
                hex   00,00



Dig1            asc   "DiGaRoK",00
Dig2            asc   "D1G@R0K",00
Dig3            asc   "D|GaR",'A',"K",00
ThankStrs0      asc   'U'," Written by DiGaRoK ",'H',00
                asc   " ",00
                asc   " ",'@A@',"   Thanks!!!  ",'@A@',00,00
ThankStrs1      asc   "  Jesse Blue",00
                asc   "    FatDog",00
                asc   "   J.Craft",00
                asc   "   DWSJason",00
                asc   "Antoine Vignau",00
                asc   "Olivier Zardini",00
                asc   "  JBrooksBSI",00
                hex   00

MoreTracksStrs  asc   "Get the latest version of SenseiPlay",00
                asc   "     (and the source code!):",00
                asc   " ",00
                asc   " ",00
                asc   "  github.com/digarok/senseiplay",00
                asc   " ",00
                asc   " ",00
                asc   " ",00
                asc   "Get NinjaTracker at www.ninjaforce.com",00
                asc   " ",00
                asc   "THANK YOU FOR USING THE APPLE IIGS!",00
                hex   00

_DPBAK          =     *                     ; after intro use logo space as a buffer
_VARS           =     *+#256
LogoStrs        asc   "                                                 ",00
                asc   "            _              _             ____                            ",00
                asc   "   ____    (_)  ____      (_)  ____ _   / __/  ____    _____  _____  ___  ",00
                asc   "  / __ \  / /  / __ \    / /  / __ `/  / /_   / __ \  / ___/ / ___/ / _ \ ",00
                asc   " / / / / / /  / / / /   / /  / /_/ /  / __/  / /_/ / / /    / /__  /  __/ ",00
                asc   "/_/ /_/ /_/  /_/ /_/ __/ /   \__,_/  /_/     \____/ /_/     \___/  \___/ ",00
                asc   "                    /___/",00,00

LoadingStrs     asc   " Loading ...  ",00
MenuBoxStrs     asc   " ______________________________________________________________________________",00
                asc   'ZV_',"SenseiPlay v2.0",'ZVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVWVW_',00
                asc   'ZV_',"NinjaForce 2023",'Z_',"                                                          ",'Z_',00
                asc   " ",'LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL'," ",00
                hex   FF,11                 ; repeat 20x
                * asc   'Z',"                                                                              ",'_',00
                asc   "                                                                                ",00
                asc   " ",'LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL'," ",00,00

SenseiLogoStrs  asc   "   _____                                _     ____     __ ",00
                asc   "  / ___/  ___    ____    _____  ___    (_)   / __ \   / /  ____ _   __  __ ",00
                asc   "  \__ \  / _ \  / __ \  / ___/ / _ \  / /   / /_/ /  / /  / __ `/  / / / / ",00
                asc   " ___/ / /  __/ / / / / (__  ) /  __/ / /   / ____/  / /  / /_/ /  / /_/ / ",00
                asc   "/____/  \___/ /_/ /_/ /____/  \___/ /_/   /_/      /_/   \__,_/   \__, / ",00
                asc   "                                                                 /____/  ",00
                hex   00

RanDumbSeparator asc  'GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG',00
                asc   'SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS',00
                asc   '\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\',00
                asc   "_____________________________________________________________________________________",00
                asc   " ",00
                asc   'SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS',00
                asc   " ",00
                asc   "_____________________________________________________________________________________",00
                hex   00

INFO_OFFSET_X   =     #55
INFO_OFFSET_Y   =     #20
FileInfoDetStrs asc   "Tracks:       Inst:",00
                asc   "Patterns:     Length:",00
                hex   00
NTPVerStr       asc   "NTPv",00


TrackBoxStrs    asc   " __________________________________",00
                hex   FF,0F                 ; repeat 10x
                asc   'Z',"                                  ",'_',00
                asc   " ",,'LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL',00
                hex   00

NinjaBubble     asc   "   ____________________________________________",00
                hex   FF,0A                 ; repeat 9x
                asc   "  ",'Z',"                                            ",'_',00
                asc   "  /                                            ",'_',00
                asc   " /                                             ",'_',00
                asc   "/                                              ",'_',00
                asc   'LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL',00
                hex   00

NinjaEyesApple  asc   '@',"     ",'@',00
NinjaEyesClose  asc   "-     -",00
NinjaEyesOpen   asc   '[',"     ",'[',00
NinjaAngryBrows asc   "   _.:'|   ,-\---/--|   ",00
NinjaStrs       asc   "                        ",00
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

**************************************************
* Apple Standard Memory Locations
**************************************************
KEY             equ   $C000
C80STOREOFF     equ   $C000
C80STOREON      equ   $C001
RAMWRTMAIN      equ   $C004
RAMWRTAUX       equ   $C005
SET80VID        equ   $C00D                 ;enable 80-column display mode (WR-only)
STROBE          equ   $C010
RDVBLBAR        equ   $C019                 ;not VBL (VBL signal low
VBL             equ   $C02E
SPEAKER         equ   $C030
LORES           equ   $C050
TXTSET          equ   $C051
MIXCLR          equ   $C052
MIXSET          equ   $C053
TXTPAGE1        equ   $C054
TXTPAGE2        equ   $C055
SETAN3          equ   $C05E                 ;Set annunciator-3 output to 0
CLRLORES        equ   $F832

                put   texttools
                put   scrollist
                put   vubars
                put   gstools
                put   p8lib                 ; put me last
                dsk   sensei.system
