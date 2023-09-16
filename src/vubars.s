VUBarDelay      db    #$A                   ; how much time between stepdown.. can change if needed
VUBarDecayAmount db   #$1                   ; how much to decay between stepdown.. can change if needed

VUBarValues     ds    #VUBarMaxTracks*2     ; 'live' values
VUBarDelays     ds    #VUBarMaxTracks*2     ; these values decay over time according to settings above
VUBarX          dw    #$0004                ; todo: move to zp
VUBarY          =     #9
VUBarY_GR       =     #7
VUBarCount      dw    #$04
VUBarMaxTracks  =     #32                   ; I'm not sure of max tracks so doing 32 to be safe
VUBarWidth      db    #4                    ; 2,4,6?



GRVUOnString2   hex   DD,99,00
GRVUOffString2  hex   E7,DD,00

VUOnString2     asc   'VW',$00
VUOffString2    asc   '_I',$00

VUBoxTopString2 asc   "__",00
VUBoxMidString2 asc   '_'," ",00
VUBoxBotString2 asc   'LL',00

GRVUOnString4   hex   99,99,00
GRVUOffString4  hex   dd,E7,00

VUOnString4     asc   'VW',$00
VUOffString4    asc   'II',$00

VUBoxTopString4 asc   " __ ",00
VUBoxMidString4 asc   'Z',"  ",'_',00
VUBoxBotString4 asc   " ",'LL'," ",00
                *     XX,MM,XX,MM
GRVUOnString6   hex   DD,99,99,11,00
GRVUOffString6  hex   E7,DD,EE,DD,00

VUOnString6     asc   'VWVW',$00
VUOffString6    asc   "_",'II',"_",$00

VUBoxTopString6 asc   " ____   ",00
VUBoxMidString6 asc   'Z',"    ",'_',"  ",00
VUBoxBotString6 asc   " ",'LLLL',"   ",00


VUBoxRightLip   asc   '_',00
VUBoxLeftLip    asc   'Z',00

* a = number of tracks
SetVUBarWidth   mx    %00
                sep   #$30
                cmp   #16
                bcc   :under16
:over16         lda   #2
                bra   :out
:under16        cmp   #5
                bcc   :under5
                lda   #4
                bra   :out
:under5         lda   #6
:out            sta   VUBarWidth
                rep   #$30
                rts

* x = number of tracks
SetVUBarOffset  mx    %00
                sep   #$30
                cpx   #4                    ; IS IT 4?!!!
                bne   :not4
                lda   #25
                sta   VUBarX
                rts
:not4           cpx   #16
                bcc   :under16
:over16
                lda   #38                   ; midpoint, eh?
:to_tha_left16  dex
                beq   :vu_offset_complete
                dec
                cmp   #3
                bcc   :vu_offset_complete   ; cant go left-er
                bra   :to_tha_left16

:under16        lda   #38                   ; midpoint, eh?
:to_tha_left    dex
                beq   :vu_offset_complete
                sec
                sbc   #2
                cmp   #3
                bcc   :vu_offset_complete   ; cant go left-er
                bra   :to_tha_left

:vu_offset_complete sta VUBarX
                rts



* a = count
RenderVUBoxes   mx    %00
                sep   $30

:prep_offset    lda   VUBarX
                sta   _cur_vu_x             ; ptr for goxy

:tops           GOXY  _cur_vu_x;#VUBarY     ; DO TOPS
                ldx   VUBarCount
:prn_tops       lda   VUBarWidth
                cmp   #2
                bne   :chk4
                PRINTSTR VUBoxTopString2
                bra   :chklp
:chk4           cmp   #4
                bne   :chk6
                PRINTSTR VUBoxTopString4
:chk6           cmp   #6
                bne   :chklp                ; THIS SHOULDN'T HAPPEN
                PRINTSTR VUBoxTopString6

:chklp          dex
                bne   :prn_tops
                                            ; DO MIDS
]VU_Y_LINES     =     #VUBarY+1             ; <--- LUP START
                LUP   4
                GOXY  _cur_vu_x;#]VU_Y_LINES
                ldx   VUBarCount
:prn_mids       lda   VUBarWidth
                cmp   #2
                bne   :chk4_b
                PRINTSTR VUBoxMidString2
                bra   :chklp_b
:chk4_b         cmp   #4
                bne   :chk6_b
                PRINTSTR VUBoxMidString4
                bra   :chklp_b
:chk6_b         cmp   #6
                bne   :chklp_b              ; THIS SHOULDN'T HAPPEN
                PRINTSTR VUBoxMidString6

:chklp_b        dex
                bne   :prn_mids
]VU_Y_LINES     =     ]VU_Y_LINES+1
                --^                         ; <--- LUP END

:bots           GOXY  _cur_vu_x;#VUBarY+5   ; DO BOTTOMS
                ldx   VUBarCount
:prn_bots       lda   VUBarWidth
                cmp   #2
                bne   :chk4c
                PRINTSTR VUBoxBotString2
                bra   :chklpc
:chk4c          cmp   #4
                bne   :chk6c
                PRINTSTR VUBoxBotString4
                bra   :chklpc
:chk6c          cmp   #6
                bne   :chklpc               ; THIS SHOULDN'T HAPPEN
                PRINTSTR VUBoxBotString6

:chklpc         dex
                bne   :prn_bots

                lda   VUBarWidth
                cmp   #2
                beq   :compactVuBoxCap
                rts

:compactVuBoxCap
                lda   text_h
                sta   _cur_vu_x

                dec   VUBarX                ; temporarily shift left to put a line on the leftmost side of the box
]VU_Y_LINES     =     #VUBarY+1             ; <--- LUP START
                LUP   4
                GOXY  _cur_vu_x;#]VU_Y_LINES
                PRINTSTR VUBoxRightLip
                GOXY  VUBarX;#]VU_Y_LINES
                PRINTSTR VUBoxLeftLip
]VU_Y_LINES     =     ]VU_Y_LINES+1
                --^
                inc   VUBarX                ; restore
                rts

* y = (vu number*2)+2   a = value
RenderGRVUBar   mx    %00

                phy                         ; preserve y/x
                phx

                sep   $30
                mx    %11
                pha
                lda   VUBarY_GR
                sta   _cur_vu_y
                lda   VUBarWidth            ; POSITION CALCULATION WITHIN VUS
                cmp   #2
                bne   :chk4
                tya                         ; Width = 2
                dec
                dec
                bra   :next
:chk4           cmp   #4
                bne   :chk6
                tya                         ; Width = 4
                dec
                dec
                asl                         ; *4
                inc
                bra   :next
:chk6           tya                         ; Width = 6
                dec
                dec
                asl                         ; *8 (has extra spacing which looks nice)
                asl
                inc
:next           clc
                adc   VUBarX                ; add global x offset

                sta   _cur_vu_x             ; ptr for goxy?
                pla
                tyx
                dex
                dex                         ; x = vu number * 2   -  now we can index to the vu data
                                            ; a = current note value

                cmp   #0
                beq   :no_note
:note           sta   VUBarValues,x
                bra   :draw2
:no_note        lda   VUBarValues,x
                beq   :draw2
                dec
                dec
                * dec
                bpl   :positive
                lda   #0
:positive       sta   VUBarValues,x

* :draw               lda   VUBarValues,x         ; a = 00-ff   y = offset
:draw2          sta   _VBUVAL
                                            ; stx   _VBUOFF

                lda   _cur_vu_y
                clc
                adc   #4
                sta   _cur_vu_y

:bar0           GOXY  _cur_vu_x;_cur_vu_y


                lda   _VBUVAL
                cmp   #$10
                bcs   :over10
:under10        jsr   GRVUOffPrint
                bra   :bar1
:over10         jsr   GRVUOnPrint


:bar1           dec   _cur_vu_y
                GOXY  _cur_vu_x;_cur_vu_y
                lda   _VBUVAL
                cmp   #$60
                bcs   :over60
:under60        jsr   GRVUOffPrint
                bra   :bar2
:over60         jsr   GRVUOnPrint


:bar2           dec   _cur_vu_y
                GOXY  _cur_vu_x;_cur_vu_y
                lda   _VBUVAL
                cmp   #$A0
                bcs   :overA0
:underA0        jsr   GRVUOffPrint
                bra   :bar3
:overA0         jsr   GRVUOnPrint


:bar3           dec   _cur_vu_y
                GOXY  _cur_vu_x;_cur_vu_y
                lda   _VBUVAL
                cmp   #$C0
                bcs   :overC0
:underC0        jsr   GRVUOffPrint
                bra   :bar_done
:overC0         jsr   GRVUOnPrint

:bar_done
                clc
                xce
                rep   $30
                plx                         ; restore y/x
                ply
                rts


* y = (vu number*2)+2   a = value
RenderVUBar     mx    %00

                phy                         ; preserve y/x
                phx

                sep   $30
                mx    %11
                pha
                lda   VUBarWidth            ; POSITION CALCULATION WITHIN VUS
                cmp   #2
                bne   :chk4
                tya                         ; Width = 2
                dec
                dec
                bra   :next
:chk4           cmp   #4
                bne   :chk6
                tya                         ; Width = 4
                dec
                dec
                asl                         ; *4
                inc
                bra   :next
:chk6           tya                         ; Width = 6
                dec
                dec
                asl                         ; *8 (has extra spacing which looks nice)
                asl
                inc
:next           clc
                adc   VUBarX                ; add global x offset

_cur_vu_val     =     $FC
_cur_vu_x       =     $FE
_cur_vu_y       =     $FF

                sta   _cur_vu_x             ; ptr for goxy?
                pla
                tyx
                dex
                dex                         ; x = vu number * 2   -  now we can index to the vu data
                                            ; a = current note value


* Do I have a new note?
* yes?  set note and reset delay
* no?
*  check delay - if not zero - done
*     else decay - reset delay

*
* I can pretty much guarantee this is bullshit
* :calculate          cmp   #0
*                     beq   :delay_check
*                     sta   VUBarValues,x
* :reset_decay        lda   VUBarDelay            ;reset decay
*                     sta   VUBarDelays,x
*                     bra   :draw
* :delay_check        dec   VUBarDelays,x         ; dec delay
*                     bpl   :draw
*                                                 ;dec   VUBarValues,x           ; dec value
*                     lda   VUBarValues,x
*                     sec
*                     sbc   VUBarDecayAmount
*                     bvc   :not_under
*                     lda   #0
* :not_under          sta   VUBarValues,x
*                     bpl   :reset_decay
*                     stz   VUBarValues,x         ; don't underflow
*                     bra   :reset_decay          ; and reset decay
                cmp   #0
                beq   :no_note
:note           sta   VUBarValues,x
                bra   :draw2
:no_note        lda   VUBarValues,x
                beq   :draw2
                dec
                dec
                * dec
                bpl   :positive
                lda   #0
:positive       sta   VUBarValues,x

* :draw               lda   VUBarValues,x         ; a = 00-ff   y = offset
:draw2          sta   _VBUVAL
                                            ; stx   _VBUOFF


:bar0           GOXY  _cur_vu_x;#VUBarY+4


                lda   _VBUVAL
                cmp   #$10
                bcs   :over10
:under10        jsr   VUOffPrint
                bra   :bar1
:over10         jsr   VUOnPrint


:bar1
                GOXY  _cur_vu_x;#VUBarY+3
                lda   _VBUVAL
                cmp   #$60
                bcs   :over60
:under60        jsr   VUOffPrint
                bra   :bar2
:over60         jsr   VUOnPrint


:bar2           GOXY  _cur_vu_x;#VUBarY+2
                lda   _VBUVAL
                cmp   #$A0
                bcs   :overA0
:underA0        jsr   VUOffPrint
                bra   :bar3
:overA0         jsr   VUOnPrint


:bar3           GOXY  _cur_vu_x;#VUBarY+1
                lda   _VBUVAL
                cmp   #$C0
                bcs   :overC0
:underC0        jsr   VUOffPrint
                bra   :bar_done
:overC0         jsr   VUOnPrint

:bar_done
                clc
                xce
                rep   $30
                plx                         ; restore y/x
                ply
                rts

_VBUVAL         db    0
_VBUOFF         db    0

VUOnPrint       mx    %11
                lda   VUBarWidth
                cmp   #2
                bne   :chk4
                PRINTSTR VUOnString2
                rts
:chk4           cmp   #4
                bne   :chk6
                PRINTSTR VUOnString4
                rts
:chk6           cmp   #6
                bne   :chkskip
                PRINTSTR VUOnString6
:chkskip        rts

VUOffPrint      mx    %11
                lda   VUBarWidth
                cmp   #2
                bne   :chk4
                PRINTSTR VUOffString2
                rts
:chk4           cmp   #4
                bne   :chk6
                PRINTSTR VUOffString4
                rts
:chk6           cmp   #6
                bne   :chkskip
                PRINTSTR VUOffString6
:chkskip        rts

GRVUOnPrint     mx    %11
                lda   VUBarWidth
                cmp   #2
                bne   :chk4
                PRINTSTR GRVUOnString2
                rts
:chk4           cmp   #4
                bne   :chk6
                PRINTSTR GRVUOnString4
                rts
:chk6           cmp   #6
                bne   :chkskip
                PRINTSTR GRVUOnString6
:chkskip        rts

GRVUOffPrint    mx    %11
                lda   VUBarWidth
                cmp   #2
                bne   :chk4
                PRINTSTR GRVUOffString2
                rts
:chk4           cmp   #4
                bne   :chk6
                PRINTSTR GRVUOffString4
                rts
:chk6           cmp   #6
                bne   :chkskip
                PRINTSTR GRVUOffString6
:chkskip        rts



_vu6_offs       db    #$20,#$30,#$40,#$50
* y = (vu number*2)+2
RenderVUVals    mx    %00
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
:vu2            clc
                adc   VUBarX
                bra   :print

:vu6            lsr
                tax
                lda   #2
                clc
                adc   VUBarX
:add            clc
                adc   #8
                dex
                bne   :add
                bra   :print

:vu4            asl                         ; now *4
                clc
                adc   VUBarX                ; add offset
                INC

:print          plx
                * sta   8
                * GOXY  8;VUBarY+6
                * plx
                * lda   VUBarValues,x
                * jsr   PrHex
                *    GOXY  8;VUBarY+7
                * lda   VUBarDelays,x
                * jsr   PrHex

:vu_done        rep   $30
                ply
                plx
                rts
