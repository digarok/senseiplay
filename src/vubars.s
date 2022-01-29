VUBarDecay        db    #$4                     ; how much time between stepdown.. can change if needed
VUBarDecayValue   db    #$1                     ; how much to decay between stepdown.. can change if needed

VUBarValues       ds    #VUBarMaxTracks*2
VUBarDecays       ds    #VUBarMaxTracks*2
VUBarX            dw    #$0004
VUBarY            =     #13
VUBarSpacing      db    #$00
VUBarCount        dw    #$04
VUBarMaxTracks    =     #32                     ; I'm not sure of max tracks so doing 32 to be safe



VUOnString       asc   'VW',$00
VUOffString        asc   'II',$00  ; G I 

VUBoxTopString    asc   " __ ",00
VUBoxMidString    asc   'Z',"  ",'_',00
VUBoxBotString    asc   " ",'LL'," ",00

* a = count
RenderVUBoxes     mx    %00
                  sep   $30
:prep_offset      lda   VUBarX
                  inc                           ; offset +3 ... seems better to offset the boxes than the bars but this is hard to explain so I won't
                  inc
                  inc
                  sta   $FE                     ; ptr for goxy


:tops             GOXY  $FE;#VUBarY
                  ldx   VUBarCount
:prn_tops         PRINTSTR VUBoxTopString
                  dex
                  bne   :prn_tops

]VU_Y_LINES       =     #VUBarY+1               ; <--- LUP START
                  LUP   4
                  GOXY  $FE;#]VU_Y_LINES
                  ldx   VUBarCount
:prn_mids         PRINTSTR VUBoxMidString
                  dex
                  bne   :prn_mids
]VU_Y_LINES       =     ]VU_Y_LINES             +1
                  --^                           ; <--- LUP END

:bots             GOXY  $FE;#VUBarY+5
                  ldx   VUBarCount
:prn_bots         PRINTSTR VUBoxBotString
                  dex
                  bne   :prn_bots


                  rts


* y = (vu number*2)+2   a = value
RenderVUBar       mx    %00
                  phy
                  phx


                  sep   $30
                  mx    %11
                  pha
                  tya
                  asl                           ; *4
                  clc
                  adc   VUBarX

                  sta   $FE                     ; ptr for goxy?
                  pla
                  tyx
                  dex
                  dex


:calculate        cmp   #0
                  beq   :decay_check
                  sta   VUBarValues,x
:reset_decay      lda   VUBarDecay              ;reset decay
                  sta   VUBarDecays,x
                  bra   :draw
:decay_check      dec   VUBarDecays,x           ; dec delay
                  bpl   :draw
                                                ;dec   VUBarValues,x           ; dec value
                  lda   VUBarValues,x
                  sec
                  sbc   VUBarDecayValue
                  bvc   :not_under
                  lda   #0
:not_under        sta   VUBarValues,x
                  bpl   :reset_decay
                  stz   VUBarValues,x           ; don't underflow
                  bra   :reset_decay            ; and reset decay

:draw             lda   VUBarValues,x           ; a = 00-ff   y = offset

                  sta   _VBUVAL
                                                ; stx   _VBUOFF


:bar0             GOXY  $FE;#VUBarY+4


                  lda   _VBUVAL
                  cmp   #$10
                  bcs   :over10
:under10          PRINTSTR VUOffString
                  bra   :bar1
:over10           PRINTSTR VUOnString


:bar1
                  GOXY  $FE;#VUBarY+3
                  lda   _VBUVAL
                  cmp   #$60
                  bcs   :over60
:under60          PRINTSTR VUOffString
                  bra   :bar2
:over60           PRINTSTR VUOnString


:bar2             GOXY  $FE;#VUBarY+2
                  lda   _VBUVAL
                  cmp   #$A0
                  bcs   :overA0
:underA0          PRINTSTR VUOffString
                  bra   :bar3
:overA0           PRINTSTR VUOnString


:bar3             GOXY  $FE;#VUBarY+1
                  lda   _VBUVAL
                  cmp   #$C0
                  bcs   :overC0
:underC0          PRINTSTR VUOffString
                  bra   :bar_done
:overC0           PRINTSTR VUOnString

:bar_done
                  clc
                  xce
                  rep   $30
                  plx
                  ply
                  rts

_VBUVAL           db    0
_VBUOFF           db    0
