* 80 col routines for IIgs adapted from Jesse Blue's

* Los Pointeros Non-Tradicionales
text_h              =     $C2
text_v              =     $C4
text_ptr            =     $C6
text_char           =     $C8
InitTextTools       php
                    clc
                    xce
                    rep   $30
                    stz   text_h
                    stz   text_v
                    stz   text_ptr
                    stz   text_char             ; DEBUG!
                    sep   $30
                    plp
                    rts


COOT8               mx    %11
text_print_char8    =     *
                    phx
                    sta   text_char

                    rep   $30

                    lda   text_v
                    asl
                    tax
                    lda   text_h
                    lsr
                    bcc   text_print_char_8b

                    clc
                    adc   table,x
                    tax
                    sep   $20                   ; shortm
                    lda   text_char             ; could push/pop
                    stal  $e00000,x
                    sep   $30
                    inc   text_h
                    plx
                    rts

text_print_char_8b  =     *
                    mx    %00
                    adc   table,x
                    tax
                    sep   $20                   ; shortm
                    lda   text_char
                    stal  $e10000,x
                    sep   $30
                    inc   text_h
                    plx
                    rts

;--------------------------
                    mx    %11
text_clear          =     *
                    stz   text_h
                    stz   text_v
]loop               lda   #" "
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

table               dw    $400,$480,$500,$580,$600,$680,$700,$780,$428,$4a8,$528,$5a8,$628,$6a8
                    dw    $728,$7a8,$450,$4d0,$550,$5d0,$650,$6d0,$750,$7d0




* a= val
PrHex               mx    %11
                    pha
                    lsr
                    lsr
                    lsr
                    lsr
                    tax
                    lda   _PrHexCaps,X
                    jsr   COOT8
                    pla
                    and   #$0f
                    tax
                    lda   _PrHexCaps,x
                    jsr   COOT8
                    rts


_PrHexCaps          asc   "0123456789ABCDEF"
_PrHexLow           asc   "0123456789abcdef"


** Draws all the mousetext chars (assumes 80 col + alt charset on?)
DebugShowMouseText  mx    %11
                    GOXY  #10;#16
                    lda   #0
:loop1              jsr   COOT8
                    inc
                    cmp   #$40
                    bne   :loop1

                    GOXY  #10;#17
                    lda   #$40
:loop2              jsr   COOT8
                    inc
                    cmp   #$80
                    bne   :loop2

                    GOXY  #10;#18
                    lda   #$80
:loop3              jsr   COOT8
                    inc
                    cmp   #$C0
                    bne   :loop3

                    GOXY  #10;#19
                    lda   #$C0
:loop4              jsr   COOT8
                    inc

                    bne   :loop4
                    rts

* byte in a
PRBYTEDEC           mx    %11
                    sta   HEX24                 ; set conversion bytes
                    stz   HEX24+1
                    stz   HEX24+2
                    jsr   HEX24TODEC8
                    lda   DEC8+1
                    beq   :no_hundreds
                    jsr   PrintNum
:no_hundreds        lda   DEC8
                    lsr
                    lsr
                    lsr
                    lsr
                    beq   :no_tens
                    jsr   PrintNum
:no_tens            lda   DEC8
                    and   #$0F
                    jmp   PrintNum


* a=digit 0-9   <-- print a single digit
PrintNum            mx    %11
                    clc
                    adc   #"0"
                    jmp   COOT8

** 24 (bit) hex to 8 (nibble) / 4 byte BCD
HEX24TODEC8         mx    %11
                    LDA   #0                    ; Ensure the result is clear
                    STA   DEC8+0
                    STA   DEC8+1
                    STA   DEC8+2
                    STA   DEC8+3

                    SED                         ; Switch to decimal mode
                    LDX   #24                   ; The number of source bits

:cnvbit             ASL   HEX24+0               ; Shift out one bit
                    ROL   HEX24+1
                    ROL   HEX24+2
                    LDA   DEC8+0                ; And add into result
                    ADC   DEC8+0
                    STA   DEC8+0
                    LDA   DEC8+1                ; propagating any carry
                    ADC   DEC8+1
                    STA   DEC8+1
                    LDA   DEC8+2                ; ... thru whole result
                    ADC   DEC8+2
                    STA   DEC8+2
                    LDA   DEC8+3                ; ... thru whole result
                    ADC   DEC8+3
                    STA   DEC8+3

                    DEX                         ; And repeat for next bit
                    BNE   :cnvbit
                    CLD                         ; Back to binary

                    rts                         ; All Done.


** 8 (nibble) / 4 byte BCD to comma formatted (USA) number string
DEC8TOCHAR10R       mx    %11
                    lda   #$80
                    sta   _PADON
                    ldy   #0
                    sty   _RENDERX
:cnvloop            jsr   GETDIGIT
                    sta   _DIGIT
                    jsr   RENDERDIGIT           ; "draw" to STR area
                    iny
                    cpy   #8
                    bne   :cnvloop
                    rts

* a=value (0-9)  y=str x
RENDERDIGIT         mx    %11
:still_needs_chk    ldx   _RENDERX
                    cpx   #2                    ; position check first
                    beq   :comma
                    cpx   #6
                    bne   :digit
:comma              bit   _PADON                ; handle comma (or space)
                    bmi   :pad_comma
                    lda   #"+"+1                 ; #","  todo bugreport on commas in strings?
                    bra   :sprintf_comma
:pad_comma          lda   #" "
:sprintf_comma      sta   CHAR10,x
                    inc   _RENDERX
                    bra   :still_needs_chk
:digit              lda   _DIGIT
                    cmp   #0
                    bne   :nonzero
:zero               bit   _PADON                ; check if leading zero
                    bpl   :nonzero
                    lda   #" "
                    bra   :sprintf
:nonzero            phy
                    tay
                    lda   _PrHexCaps,y
                    ply
                    stz   _PADON
:sprintf            sta   CHAR10,x
                    inc   _RENDERX
                    rts

* X = offset into number from LEFT [01, 23, 45, 67, ..]
GETDIGIT            mx    %11
                    tya
                    lsr
                    bcc   :even
:odd                sta   :sub_o+1
                    sec
                    lda   #3                    ; (size-1 of DEC8)
:sub_o              sbc   #0
                    tax
                    lda   DEC8,x
                    and   #$0F
                    rts
:even               sta   :sub_e+1
                    sec
                    lda   #3                    ; (size-1 of DEC8)
:sub_e              sbc   #0
                    tax
                    lda   DEC8,x
                    lsr
                    lsr
                    lsr
                    lsr
                    rts

_PADON              db    $80
_RENDERX            db    0
_DIGIT              db    0

** MAXVAL = $FF FFFF =  16,777,215
HEX24               ADR   $ffffff               ; $123456              ;1,193,046
DEC8                DS    4                     ; 8 digits of BCD
CHAR10              DS    10








GOXY                MAC
                    ldx   ]1
                    ldy   ]2
                    stx   text_h
                    sty   text_v
                    <<<

PRINTSTR            MAC
                    lda   #]1
                    ldy   #>]1
                    jsr   PrintString
                    <<<


PRINTSTRXY          MAC
                    lda   #]1
                    sta   text_h
                    lda   #]2
                    sta   text_v
                    lda   #]3
                    ldy   #>]3
                    jsr   PrintString
                    <<<

PRINTSTRSXY         MAC
                    lda   #]2
                    sta   text_v
                    lda   #]3
                    ldy   #>]3
                    ldx   #]1                   ; horiz pos
                    jsr   PrintStringsX         ; implied rts
                    <<<

;PRINTSTRXLUP #0;#3;#12;MenuMidStr              ; x;y;loopcount;str
PRINTSTRXLUP        MAC
                    lda   #]1
                    sta   _printstringsx_horiz
                    lda   #]2
                    sta   text_v
                    clc
                    adc   #]3
                    sta   _printstringsy_clip   ; end line #
_lup                lda   _printstringsx_horiz
                    sta   text_h
                    lda   #]4
                    ldy   #>]4
                    jsr   PrintString
                    inc   text_v
                    lda   text_v
                    cmp   _printstringsy_clip
                    bne   _lup
                    <<<


* PrintString (A=Low Byte,  Y=High Byte)
PrintString         mx    %11
                    sta   :loop+1
                    sty   :loop+2

                    clc
                    xce

                    ldy   #0
:loop               lda   $FFFF,y               ; dummy bytes
                    beq   :done
                    jsr   COOT8
                    iny

                    bra   :loop
:done               rts


BigNum              MAC
                    lda   #]2+{]1*$100}         ; multiply low byte by 256 and add high byte
                    <<<

* lda #MainMenuStrs
* ldy #>MainMenuStrs
* ldx #05 ; horiz pos
PrintStringsX       mx    %11
                    stx   _printstringsx_horiz

                    sta   $0
                    sty   $1
:loop               lda   _printstringsx_horiz
                    sta   text_h
                    lda   $0                    ; slower, but allows API reuse
                    ldy   $1
                    jsr   PrintString           ; y is last val
                    inc   text_v                ; update cursor pos
                    iny
                    lda   ($0),y
                    beq   :done
                    tya                         ; not done so add strlen to source ptr
                    clc
                    adc   $0
                    sta   $0
                    bcc   :nocarry
                    inc   $1
:nocarry            bra   :loop

:done               rts

_printstringsx_horiz db   00
_printstringsy_clip db    00
* lda #MainMenuStrs
* ldy #>MainMenuStrs
* ldx #05 ; horiz pos
SetYClip            sta   _printstringsy_clip
                    rts
PrintStringsXYClip  stx   _printstringsx_horiz
                    sta   $0
                    sty   $1
                    stz   $12                   ; y clip
:loop
                    lda   $12
                    cmp   _printstringsy_clip
                    beq   :done
                    lda   _printstringsx_horiz
                    sta   text_h
                    lda   $0                    ; slower, but allows API reuse
                    ldy   $1
                    jsr   PrintString           ; y is last val
                    inc   text_v                ; update cursor pos
                    inc   $12                   ; update y clip
                    iny
                    lda   ($0),y
                    beq   :done
                    tya                         ; not done so add strlen to source ptr
                    clc
                    adc   $0
                    sta   $0
                    bcc   :nocarry
                    inc   $1
:nocarry            bra   :loop

:done               rts

SetGSText           mx    %11
                    lda   #01
                    stal  $00c029
                    rts

Setup80Col          mx    %11
                    lda   #$A0                  ; USE A BLANK SPACE TO
                    jsr   $C300                 ; TURN ON THE VIDEO FIRMWARE
                    rts
GoXCenter
                    lsr                         ; /2  < - CENTER ROUTINE
                    sta   :center+1
                    lda   #40
                    sec
:center             sbc   #0                    ;smc
                    sta   text_h
                    rts