                  mx    %00
* if selected is over half the value of the window heigh then offset becones selected minus window heigght /2
* if at the end, the offfset is greater than list size minus window height then offset = list size minus window height (stop scrolling)

*scrollist


* setup

* -- draw list
*     init window ranges?
*      for i in range(start,finish):
*        dispatch_draw_function(i)

* -- handle keys
*     if up selected--
*       if selected <0 selected = 0
*     if dn selected++
*       if selected > itemscount selected = itemscount
*     if enter return index?  or trigger list "execution" routine


SL_SETWINDOWSIZE  MAC
                  lda   #]1
                  sta   SL_windowsize_x
                  lda   #]2
                  sta   SL_windowsize_y
                  lsr                           ; /2
                  sta   SL_windowsize_ydiv2
                  <<<

SL_SETWINDOWPOS   MAC
                  lda   #]1
                  sta   SL_windowpos
                  lda   #]2
                  sta   SL_windowpos+2
                  <<<

SL_SETRENDERFUNCTION MAC
                  lda   #]1
                  sta   SL_renderfunction
                  <<<


SL_CalculateOffset mx   %00
:bound_checking   lda   SL_selected             ; sets offset to zero if OOB
                  cmp   #$FFFF                  ; -1 = none  @TODO ASSEMBLER ISSUE?  F0FF ??????!!!!
                  beq   :zero_offset
                  cmp   SL_itemscount           ; make sure we're not over
                  bcc   :calc_offset            ; all clear - do the calculation

:zero_offset      stz   SL_offset               ; OOB - zero offset
                  rts

:calc_offset      cmp   SL_windowsize_ydiv2     ; is selected over half the value of the window height (5)
                  bcc   :zero_offset            ; otherwise don't scroll - zero
                  sec
                  sbc   SL_windowsize_ydiv2
                  sta   SL_offset

:eol_check        lda   SL_itemscount
                  sec
                  sbc   SL_windowsize_y
                  cmp   SL_offset
                  bcc   :over_eol
                  rts                           ; we're ok

:over_eol         sta   SL_offset
                  rts


SL_IncSelected    mx    %00
                  inc   SL_selected
                  lda   SL_selected
                  cmp   SL_itemscount
                  bcc   :ok
:over             dec   SL_selected
:ok               rts


SL_DecSelected    mx    %00
                  lda   SL_selected
                  beq   :already_zero
:ok               dec   SL_selected
:already_zero     rts


SL_GetSelected    mx    %00
                  lda   SL_selected
                  rts

SL_DrawWindow     mx    %00
                  lda   SL_offset               ; 16-bit
                  sta   SL_cur_off
                  lda   SL_renderfunction       ; smc
                  sta   :render_item+1
                  sep   #$30                    ; 8-bit
                  ldy   SL_windowpos_y
                  sty   SL_cur_y

:set_xy           ldx   SL_windowpos_x
                  stx   $24
                  ldy   SL_cur_y
                  sty   $25
                  jsr   VTAB
                  clc
                  xce
                  rep   #$30
                  lda   SL_cur_off
:render_item      jsr   $0000                   ; smc  "jsr SL_renderfunction"

                  inc   SL_cur_off              ; item++
                  inc   SL_cur_y                ; y++
                  lda   SL_windowpos_y
                  clc
                  adc   SL_windowsize_y

                  cmp   SL_cur_y
                  beq   :done
:next_item        sep   $30
                  bra   :set_xy
:done
                  rts




SL_selected       dw    0
SL_offset         dw    0                       ; list offset (start position)
SL_itemscount     dw    0                       ; set during init

SL_renderfunction da    0                       ; $nnnn
SL_windowsize                                   ; xxxxyyyyzzzz
SL_windowsize_x   dw    0
SL_windowsize_y   dw    0
SL_windowsize_ydiv2 dw  0
SL_windowpos                                    ; xxxxyyyy
SL_windowpos_x    dw    0
SL_windowpos_y    dw    0
SL_cur_y          dw    0                       ; used in window renderer
SL_cur_off        dw    0                       ;


* assume cursor xy is set for string printing
* a = item index   -  uses 0 and 2 on dp
SL_DemoList1RenderItem mx %00
                  cmp   SL_itemscount           ; see if item exists
                  bcc   :exists
                  stz   2                       ; uninvert if blank
                  lda   #SL_DemoListBlank       ; if not we draw blank lines
                  bra   :continue
                                                ; get item index
:exists           tax
                  stz   2                       ; don't change chars
                  cpx   SL_selected
                  bne   :not_selected
                  lda   #$80
                  sta   2
:not_selected
                  lda   #SL_DemoList1           ; start w pointer at beginning of list
                  cpx   #0
                  beq   :continue               ; no need to add anything

                  clc
:calc_item_start  adc   #SL_DemoList1ItemLen
                  dex
                  bne   :calc_item_start
:continue         sta   0                       ; address of string at zero
                  sep   #$30
                  ldx   #SL_DemoList1ItemLen
                  ldy   #0
:pr_loop          lda   (0),y
                  sec                           ;\___  char inverter
                  sbc   2                       ;/
                  jsr   COUT
                  iny
                  dex
                  bne   :pr_loop
                  rep   #$30
                  rts

SL_DemoList1ItemLen =   9
SL_DemoList1ItemCount = 3
SL_DemoList1
                  asc   "Liam     "
                  asc   "Olivia   "
                  asc   "Noah     "
                  asc   "Riley    "
                  asc   "Jackson  "
                  asc   "Emma     "
                  asc   "Aiden    "
                  asc   "Ava      "
                  asc   "Elijah   "
                  asc   "Isabella "
                  asc   "Grayson  "
                  asc   "Aria     "
                  asc   "Lucas    "
                  asc   "Aaliyah  "
                  asc   "Oliver   "
                  asc   "Amelia   "
                  asc   "Caden    "
                  asc   "Mia      "
                  asc   "Mateo    "
                  asc   "Layla    "
                  asc   "Muhammad "
                  asc   "Zoe      "
                  asc   "Mason    "
                  asc   "Camilla  "
                  asc   "Carter   "
                  asc   "Charlotte"
                  asc   "Jayden   "
                  asc   "Eliana   "
                  asc   "Ethan    "
                  asc   "Mila     "
                  asc   "Sebastian"
SL_DemoListBlank  asc   "         "

SL_DemoList1Run   mx    %00
:setup            SL_SETWINDOWPOS 5;2
                  SL_SETWINDOWSIZE 16;10
                  SL_SETRENDERFUNCTION SL_DemoList1RenderItem
                  lda   #SL_DemoList1ItemCount
                  sta   SL_itemscount
                  rts
                  mx    %00
