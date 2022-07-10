VUBarDecay          db    #$5                   ; how much time between stepdown.. can change if needed
VUBarDecayValue     db    #$1                   ; how much to decay between stepdown.. can change if needed

VUBarValues         ds    #VUBarMaxTracks*2
VUBarDecays         ds    #VUBarMaxTracks*2
VUBarX              dw    #$0004
VUBarY              =     #13
VUBarCount          dw    #$04
VUBarMaxTracks      =     #32                   ; I'm not sure of max tracks so doing 32 to be safe
VUBarWidth          db    #4                    ; 2,4,6?

VUOnString4         asc   'VW',$00
VUOffString4        asc   'II',$00               ; G I

VUBoxTopString4     asc   " __ ",00
VUBoxMidString4     asc   'Z',"  ",'_',00
VUBoxBotString4     asc   " ",'LL'," ",00


VUOnString6         asc   'VWVW',$00
VUOffString6        asc   "_",'II',"_",$00         ; G I

VUBoxTopString6     asc   " ____   ",00
VUBoxMidString6     asc   'Z'," xx ",'_',"  ",00
VUBoxBotString6     asc   " ",'LLLL',"   ",00


VUOnString2         asc   'VW',$00
VUOffString2        asc   '_I',$00               ; G I

VUBoxTopString2     asc   "__",00
VUBoxMidString2     asc   '_'," ",00
VUBoxBotString2     asc   'LL',00
VUBoxRightLip       asc   '_',00
VUBoxLeftLip        asc   'Z',00

* a = number of tracks
SetVUBarWidth       mx    %00
                    sep   #$30
                    cmp   #16
                    bcc   :under16
:over16             lda   #2
                    bra   :out
:under16            cmp   #5
                    bcc   :under5
                    lda   #4
                    bra   :out
:under5             lda   #6
:out                sta   VUBarWidth
                    rep   #$30
                    rts

* x = number of tracks
SetVUBarOffset      mx    %00
                    sep   #$30
                    cpx   #4                    ; IS IT 4?!!!
                    bne   :not4
                    lda   #25
                    sta   VUBarX
                    rts
:not4               cpx   #16
                    bcc   :under16
:over16
                    lda   #38                   ; midpoint, eh?
:to_tha_left16      dex
                    beq   :vu_offset_complete
                    dec
                    cmp   #3
                    bcc   :vu_offset_complete   ; cant go left-er
                    bra   :to_tha_left16

:under16            lda   #38                   ; midpoint, eh?
:to_tha_left        dex
                    beq   :vu_offset_complete
                    sec
                    sbc   #2
                    cmp   #3
                    bcc   :vu_offset_complete   ; cant go left-er
                    bra   :to_tha_left

:vu_offset_complete sta   VUBarX
                    rts



* a = count
RenderVUBoxes       mx    %00
                    sep   $30
                                                ; STZ   VUBarX                ; TEMP DELETE!
:prep_offset        lda   VUBarX

                    sta   $FE                   ; ptr for goxy


:tops               GOXY  $FE;#VUBarY           ; DO TOPS
                    ldx   VUBarCount
:prn_tops           lda   VUBarWidth
                    cmp   #2
                    bne   :chk4
                    PRINTSTR VUBoxTopString2
                    bra   :chklp
:chk4               cmp   #4
                    bne   :chk6
                    PRINTSTR VUBoxTopString4
:chk6               cmp   #6
                    bne   :chklp                ; THIS SHOULDN'T HAPPEN
                    PRINTSTR VUBoxTopString6

:chklp              dex
                    bne   :prn_tops
                                                ; DO MIDS
]VU_Y_LINES         =     #VUBarY+1             ; <--- LUP START
                    LUP   4
                    GOXY  $FE;#]VU_Y_LINES
                    ldx   VUBarCount
:prn_mids           lda   VUBarWidth
                    cmp   #2
                    bne   :chk4_b
                    PRINTSTR VUBoxMidString2
                    bra   :chklp_b
:chk4_b             cmp   #4
                    bne   :chk6_b
                    PRINTSTR VUBoxMidString4
                    bra   :chklp_b
:chk6_b             cmp   #6
                    bne   :chklp_b              ; THIS SHOULDN'T HAPPEN
                    PRINTSTR VUBoxMidString6

:chklp_b            dex
                    bne   :prn_mids
]VU_Y_LINES         =     ]VU_Y_LINES+1
                    --^                         ; <--- LUP END

:bots               GOXY  $FE;#VUBarY+5         ; DO BOTTOMS
                    ldx   VUBarCount
:prn_bots           lda   VUBarWidth
                    cmp   #2
                    bne   :chk4c
                    PRINTSTR VUBoxBotString2
                    bra   :chklpc
:chk4c              cmp   #4
                    bne   :chk6c
                    PRINTSTR VUBoxBotString4
                    bra   :chklpc
:chk6c              cmp   #6
                    bne   :chklpc               ; THIS SHOULDN'T HAPPEN
                    PRINTSTR VUBoxBotString6

:chklpc             dex
                    bne   :prn_bots

                    lda   VUBarWidth
                    cmp   #2
                    beq   :rightCap
                    rts

:rightCap
                    lda   text_h
                    sta   $fe
                    lda   VUBarX
                    dec
                    sta   $fc
]VU_Y_LINES         =     #VUBarY+1             ; <--- LUP START
                    LUP   4
                    GOXY  $FE;#]VU_Y_LINES
                    PRINTSTR VUBoxRightLip
                    GOXY  $FC;#]VU_Y_LINES
                    PRINTSTR VUBoxLeftLip
]VU_Y_LINES         =     ]VU_Y_LINES+1
                    --^
                    rts


* y = (vu number*2)+2   a = value
RenderVUBar         mx    %00

                    phy
                    phx


                    sep   $30
                    mx    %11
                    pha
                    lda   VUBarWidth
                    cmp   #2
                    bne   :chk4
                    tya
                    dec
                    dec
                    bra   :next
:chk4               cmp   #4
                    bne   :chk6
                    tya                         ; calc x pos within VUs
                    dec
                    dec
                    asl                         ; *4
                    inc
                    bra   :next
:chk6               tya
                    dec
                    dec
                    asl                         ; *8
                    asl
                    inc
:next               clc
                    adc   VUBarX                ; add global x offset

                    sta   $FE                   ; ptr for goxy?
                    pla
                    tyx
                    dex
                    dex


:calculate          cmp   #0
                    beq   :decay_check
                    sta   VUBarValues,x
:reset_decay        lda   VUBarDecay            ;reset decay
                    sta   VUBarDecays,x
                    bra   :draw
:decay_check        dec   VUBarDecays,x         ; dec delay
                    bpl   :draw
                                                ;dec   VUBarValues,x           ; dec value
                    lda   VUBarValues,x
                    sec
                    sbc   VUBarDecayValue
                    bvc   :not_under
                    lda   #0
:not_under          sta   VUBarValues,x
                    bpl   :reset_decay
                    stz   VUBarValues,x         ; don't underflow
                    bra   :reset_decay          ; and reset decay

:draw               lda   VUBarValues,x         ; a = 00-ff   y = offset

                    sta   _VBUVAL
                                                ; stx   _VBUOFF


:bar0               GOXY  $FE;#VUBarY+4


                    lda   _VBUVAL
                    cmp   #$10
                    bcs   :over10
:under10            jsr   VUOffPrint
                    bra   :bar1
:over10             jsr   VUOnPrint


:bar1
                    GOXY  $FE;#VUBarY+3
                    lda   _VBUVAL
                    cmp   #$60
                    bcs   :over60
:under60            jsr   VUOffPrint
                    bra   :bar2
:over60             jsr   VUOnPrint


:bar2               GOXY  $FE;#VUBarY+2
                    lda   _VBUVAL
                    cmp   #$A0
                    bcs   :overA0
:underA0            jsr   VUOffPrint
                    bra   :bar3
:overA0             jsr   VUOnPrint


:bar3               GOXY  $FE;#VUBarY+1
                    lda   _VBUVAL
                    cmp   #$C0
                    bcs   :overC0
:underC0            jsr   VUOffPrint
                    bra   :bar_done
:overC0             jsr   VUOnPrint

:bar_done
                    clc
                    xce
                    rep   $30
                    plx
                    ply
                    rts

_VBUVAL             db    0
_VBUOFF             db    0

VUOnPrint           mx    %11
                    lda   VUBarWidth
                    cmp   #2
                    bne   :chk4
                    PRINTSTR VUOnString2
                    rts
:chk4               cmp   #4
                    bne   :chk6
                    PRINTSTR VUOnString4
                    rts
:chk6               cmp   #6
                    bne   :chkskip
                    PRINTSTR VUOnString6
:chkskip            rts

VUOffPrint          mx    %11
                    lda   VUBarWidth
                    cmp   #2
                    bne   :chk4
                    PRINTSTR VUOffString2
                    rts
:chk4               cmp   #4
                    bne   :chk6
                    PRINTSTR VUOffString4
                    rts
:chk6               cmp   #6
                    bne   :chkskip
                    PRINTSTR VUOffString6
:chkskip            rts





_vu6_offs           db    #$20,#$30,#$40,#$50
* y = (vu number*2)+2
RenderVUVals        mx    %00
                    phx                         ; need to return x unmodified
                    phy                         ; need to return y unmodified
                    sep   $30
                    tya
                    dec
                    dec
                    pha


                    ldy   VUBarWidth
                    cpy   #4
                    beq   :vu4
                    cpy   #2
                    beq   :vu2
                    bra   :vu6
:vu2                clc
                    adc   VUBarX
                    bra   :print



:vu6                lsr
                    tax
                    lda   #2
                    clc
                    adc   VUBarX
:add                clc
                    adc   #8
                    dex
                    bne   :add
                    bra   :print

:vu4                asl                         ; now *4
                    clc
                    adc   VUBarX                ; add offset
                    INC

:print              sta   8
                    GOXY  8;VUBarY+6
                    plx
                    lda   VUBarValues,x
                    jsr   PrHex

:vu_done            clc
                    xce
                    rep   $30
                    ply
                    plx
                    rts

* y = (vu number*2)+2   a = value
RenderVU            mx    %00

                    phx
                    phy
                    sep   $30
                    pha
                    tya
                    lda   VUBarWidth
                    cmp   #2
                    beq   :noshift
                    asl                         ; now *4
:noshift            clc
                    adc   VUBarX                ; add offset
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
