**************************************************************************
** scrollist library - implement a scrolling selection list in a window **
**************************************************************************
                mx    %00


** init example - set window and list properties
**
**                  SL_SETWINDOWPOS 31;11               ; set position
**                  SL_SETWINDOWSIZE 22;8               ; set size
**                  SL_SETRENDERFUNCTION MyItemRender   ; set item renderer function
**                  lda   MyListCount                   ;\_ set number of items
**                  sta   SL_itemscount                 ;/

** handle keys example
**     if up -> jsr SL_IncSelected
**     if dn -> jsr SL_DecSelected
**     if enter -> jsr SL_GetSelected and do something with it!

** How the vertical scroll-center algorithm works (in case I forget)
** - if selected item is over half the value of the window height then offset becomes selected minus window height /2
** - if at the end, the offset is greater than list size minus window height then offset = list size minus window height (stop scrolling)


SL_SETWINDOWSIZE MAC
                lda   #]1
                sta   SL_windowsize_x
                lda   #]2
                sta   SL_windowsize_y
                lsr                         ; /2
                sta   SL_windowsize_ydiv2
                <<<

SL_SETWINDOWPOS MAC
                lda   #]1
                sta   SL_windowpos
                lda   #]2
                sta   SL_windowpos+2
                <<<

SL_SETRENDERFUNCTION MAC
                lda   #]1
                sta   SL_renderfunction
                <<<


SL_CalculateOffset mx %00
:bound_checking lda   SL_itemscount         ; if fewer items..
                cmp   SL_windowsize_y       ;   than window height... then zero
                bcc   :zero_offset
                lda   SL_selected           ; sets offset to zero if OOB
                cmp   #$FFFF                ; -1 = none
                beq   :zero_offset
                cmp   SL_itemscount         ; make sure we're not over
                bcc   :calc_offset          ; all clear - do the calculation

:zero_offset    stz   SL_offset             ; OOB - zero offset
                rts

:calc_offset    cmp   SL_windowsize_ydiv2   ; is selected over half the value of the window height (5)
                bcc   :zero_offset          ; otherwise don't scroll - zero
                sec
                sbc   SL_windowsize_ydiv2
                sta   SL_offset

:eol_check      lda   SL_itemscount
                sec
                sbc   SL_windowsize_y
                cmp   SL_offset
                bcc   :over_eol
                rts                         ; we're ok

:over_eol       sta   SL_offset
                rts


SL_IncSelected  mx    %00
                inc   SL_selected
                lda   SL_selected
                cmp   SL_itemscount
                bcc   :ok
:over           dec   SL_selected
:ok             rts


SL_DecSelected  mx    %00
                lda   SL_selected
                beq   :already_zero
:ok             dec   SL_selected
:already_zero   rts


SL_GetSelected  mx    %00
                lda   SL_selected
                rts

SL_DrawWindow   mx    %00
                lda   SL_offset             ; 16-bit
                sta   SL_cur_off
                lda   SL_renderfunction     ; smc
                sta   :render_item+1
                sep   #$30                  ; 8-bit
                ldy   SL_windowpos_y
                sty   SL_cur_y

:set_xy         ldx   SL_windowpos_x
                stx   text_h
                ldy   SL_cur_y
                sty   text_v
                rep   #$30
                lda   SL_cur_off
:render_item    jsr   $0000                 ; smc  "jsr SL_renderfunction"

                inc   SL_cur_off            ; item++
                inc   SL_cur_y              ; y++
                lda   SL_windowpos_y
                clc
                adc   SL_windowsize_y

                cmp   SL_cur_y
                beq   :done
:next_item      sep   $30
                bra   :set_xy
:done
                rts


SL_selected     dw    0
SL_offset       dw    0                     ; list offset (start position)
SL_itemscount   dw    0                     ; set during init

SL_renderfunction da  0                     ; $nnnn
SL_windowsize                               ; xxxxyyyyzzzz
SL_windowsize_x dw    0
SL_windowsize_y dw    0
SL_windowsize_ydiv2 dw 0
SL_windowpos                                ; xxxxyyyy
SL_windowpos_x  dw    0
SL_windowpos_y  dw    0
SL_cur_y        dw    0                     ; used in window renderer
SL_cur_off      dw    0                     ;
