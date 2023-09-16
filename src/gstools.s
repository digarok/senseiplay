******************************************
* ToolCall Macros                        *
******************************************
Tool            MAC
                LDX   #]1
                JSL   $E10000
                <<<
_TLStartUp      MAC
                Tool  $201
                <<<
_TLShutDown     MAC
                Tool  $301
                <<<
_NewHandle      MAC
                Tool  $902
                <<<
~DisposeHandle  MAC
                PushLong ]1
_DisposeHandle  MAC
                Tool  $1002
                <<<
_MMStartUp      MAC
                Tool  $202
                <<<
_GetMasterId    MAC
                Tool  $2003
                <<<
_MTStartUp      MAC
                Tool  $203
                <<<

_TLTextMountVol MAC
                Tool  $1201
                <<<
_UnPackBytes    MAC
                Tool  $2703
                <<<

LoadLongXY      MAC
                LDX   #^]1
                LDY   #]1
                <<<

PushLongXY      MAC
                PHX
                PHY
                <<<

PushLong        MAC
                IF    #=]1
                PushWord #^]1
                ELSE
                PushWord ]1+2
                FIN
                PushWord ]1
                <<<

PushWord        MAC
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
Tool            MAC
                LDX   #]1
                JSL   $E10000
                <<<


****************************************
* AllocOneBank                         *
* This is a custom allocation function *
* that makes use of the fact that we   *
* request an entire locked bank and so *
* simply returns the bank in the       *
* accumulator. (basically dereference  *
* the Handle to get the pointer)       *
****************************************
AllocOneBank    mx    %00
                PushLong #0
                PushLong #$10000
                PushWord UserId
                PushWord #%11000000_00011100
                PushLong #0
                _NewHandle                  ; returns LONG Handle on stack
                plx                         ; base address of the new handle
                pla                         ; high address 00XX of the new handle (bank)
                xba                         ; swap accumulator bytes to XX00
                sta   :bank+2               ; store as bank for next op (overwrite $XX00)
:bank           ldal  $000001,X             ; recover the bank address in A=XX/00
                rts


* X/Y = length in bytes (24bit); returns handle in x/a ??
AllocContiguousPageAlign mx %00
                PushLong #0                 ; result space
                PushLongXY
                PushWord UserId
                PushWord #%11000000_00001100

                PushLong #0
                _NewHandle                  ; returns LONG Handle on stack
                bcc   :returnHandle
:allocError     plx                         ; clear stack result
                plx
                ldx   #$FFFF
                rts                         ; return error in A
:returnHandle   plx                         ; base address of the new handle
                pla                         ; high address 00XX of the new handle (bank)
                rts


MM_memErr       =     $0201                 ; Unable to allocate block
MM_lockErr      =     $0204                 ; Illegal operation on a locked or immovable block
MM_idErr        =     $0207                 ; Invalid user ID
