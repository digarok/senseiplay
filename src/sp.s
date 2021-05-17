********************************************************************************
*                                                                              *
*     _____                                _     ____     __                   *
*    / ___/  ___    ____    _____  ___    (_)   / __ \   / /  ____ _   __  __  *
*    \__ \  / _ \  / __ \  / ___/ / _ \  / /   / /_/ /  / /  / __ `/  / / / /  *
*   ___/ / /  __/ / / / / (__  ) /  __/ / /   / ____/  / /  / /_/ /  / /_/ /   *
*  /____/  \___/ /_/ /_/ /____/  \___/ /_/   /_/      /_/   \__,_/   \__, /    *
*                                                                   /____/     *
*                                                                              *
*  (c) 2021 Dagen Brock                                                        *
*  NinjaTrackerPlus (NTP) Engine by Jesse Blue of Ninjaforce                   *
********************************************************************************


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
                  clc
                  xce
                  rep   #$30
                  jsr   PrepareTools
                  jsr   PrepareNTP

                              ; .... TEST CODE ....
                lda #$0003 ; bank 3 
                sta $02 ; dp ptr hi
                stz $00  ; dp ptr lo 
                   PT_LoadFilenameToPtr 'ntp/eshaa.ntp';0 
                   jsr StartMusic
        FUNHALT
  mx %00

StartMusic          lda         #$0003
                    ;xba
                    tay
                    ldx         #0
                    jsr         _NTPprepare
                    bcc         ok
                    brk         $ee                      ;@todo?
ok                  lda         #0
                    jsr         _NTPplay
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

                ;  jsr   P8CALL_GET_PREFIX
                 ; lda   P8BUFF_PREFIXPATH
                 ; jsr   P8CALL_ONLINE
                 ; lda   P8BUFF_DRIVES_ONLINE
                 ; jsr   DirTest
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
* If the Memory Manager reported an error, we need to allocate our own memory first.
                  _MTStartUp
* First we need a user ID.
                  pha
                  pea   $1000
                  _GetMasterId                  ; Get me a new user ID (Application)
                  pla
                  sta   MasterId                ; Save it for later
* Now give us all of bank zero and bank one.
                  pha
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
P8Quit            jsr   P8_MLI_CALL                     ; first actual command, call ProDOS vector
                  dfb   $65                     ; with "quit" request ($65)
                  da    QuitParm
                  bcs   Error                   ; what's the point?  ;)
Error             brk   $00                     ; shouldn't ever  here!

QuitParm          dfb   4                       ; number of parameters
                  dfb   0                       ; standard quit type
                  da    $0000                   ; not needed when using standard quit
                  dfb   0                       ; not used
                  da    $0000                   ; not used

******************************************
* ToolCall Macros                        *
******************************************
Tool              MAC
                  LDX   #]1
                  JSL   $E10000
                  <<<
_TLStartUp        MAC
                  Tool  $201
                  <<<
_TLShutDown       MAC
                  Tool  $301
                  <<<
_NewHandle        MAC
                  Tool  $902
                  <<<
_MMStartUp        MAC
                  Tool  $202
                  <<<
_GetMasterId      MAC
                  Tool  $2003
                  <<<
_MTStartUp        MAC
                  Tool  $203
                  <<<

_TLTextMountVol   MAC
                  Tool  $1201
                  <<<
_UnPackBytes      MAC
                  Tool  $2703
                  <<<

LoadLongXY        MAC
                  LDX   #^]1
                  LDY   #]1
                  <<<

PushLongXY        MAC
                  PHX
                  PHY
                  <<<

PushLong          MAC
                  IF    #=]1
                  PushWord #^]1
                  ELSE
                  PushWord ]1+2
                  FIN
                  PushWord ]1
                  <<<

PushWord          MAC
                  IF    #=]1
                  PEA   ]1
                  ELSE
                  IF    MX/2
                  LDA   ]1+1
                  PHA
                  FIN
                  LDA   ]1
                  PHA
                  FIN
                  <<<
Tool              MAC
                  LDX   #]1
                  JSL   $E10000
                  <<<

MasterId          ds    2
UserId            ds    2
BnkNTP            hex   0000                    ; used for NTP engine
Bnk0Hnd           hex   00000000
Bnk1Hnd           hex   00000000



**********************************************************   NINJATRACKERPLUS
** STUB FUNCTIONS
_NTPprepare       jsl   NTPprepare
                  rts
_NTPplay          jsl   NTPplay
                  rts
_NTPstop          jsl   _NTPstop
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



                  mx    %11                     ; we don't know actually
RANDOPAUSE        sec
                  xce
                  sep   #$30
                  stal  $00c034
:lp               bra   :lp

FUNHALT           MAC
                  sec
                  xce
                  sep   #$30
:loop                lda $c019

                  bpl :skip
                  lda #$f6
                  sta $c022
:skip                  lda   #$db
                  jsr   $fdda

                  lda   $c022
                  clc
                  adc   #$10
                  sta   $c022
                  lda   #$FF
:delay            dec
                  bne   :delay
                  bra   :loop
                  <<<




                  put   p8tools
                  dsk   sensei.system



