* 80 col routines for IIgs adapted from Jesse Blue's

* Los Pointeros Non-Tradicionales
text_h            =     $C2
text_v            =     $C4
text_ptr          =     $C6
text_char         =     $C8
InitTextTools     php
                  clc
                  xce
                  rep   $30
                  stz   text_h
                  stz   text_v
                  stz   text_ptr
                  stz   text_char               ; DEBUG!
                  sep   $30
                  plp
                  rts


COOT8             mx    %11
text_print_char8  =     *
                  phy
                  phx
                  sta   text_char

                  rep   $30

                  lda   text_v
                  asl
                  tay
                  lda   text_h
                  lsr
                  bcc   text_print_char_8b

                  clc
                  adc   table,y
                  tax
                  sep   $20                     ; shortm
                  lda   text_char               ; could push/pop
                  stal  $e00000,x
                  sep   $30
                  inc   text_h
                  plx
                  ply
                  rts

text_print_char_8b =    *
                  mx    %00
                  adc   table,y
                  tax
                  sep   $20                     ; shortm
                  lda   text_char
                  stal  $e10000,x
                  sep   $30
                  inc   text_h
                  plx
                  ply
                  rts

;--------------------------
                  mx    %11
text_clear        =     *
                  stz   text_h
                  stz   text_v
]loop             lda   #" "
                  jsr   COOT8
                  lda   text_h
                  cmp   #80
                  blt   ]loop
                  stz   text_h
                  inc   text_v
                  lda   text_v
                  cmp   #24
                  blt   ]loop
                  rts

table             dw    $400,$480,$500,$580,$600,$680,$700,$780,$428,$4a8,$528,$5a8,$628,$6a8
                  dw    $728,$7a8,$450,$4d0,$550,$5d0,$650,$6d0,$750,$7d0
