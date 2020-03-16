    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files with VCS register memory mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "macro.h"
    include "vcs.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare the variables starting from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u variables
    org $80

PF0Variable         byte

P0XPosition         byte
P0YPosition         byte
P0Height            byte

P1XPosition         byte
P1YPosition         byte
P1Height            byte

PlayfieldStartPos   byte
PlayfieldHeight     byte

MissileXOffset      byte
MissileXFineOffset  byte
MissileDirection    byte
MissileXPos         byte
MissileYPos         byte

P0FacingOffset      byte
P0CrouchedOffset    byte
P0SpritePtr         word
P0ColorPtr          word
P0MissileHit        byte

P1SpriteOffset      byte
P1SpritePtr         word
P1ColorPtr          word
P1BitmapLine        byte
P1MissileHit        byte

LastPressedKey      byte

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg Code
    org $F000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clear memory.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Reset:
    CLEAN_START

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set initial value of variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #10
    sta P0XPosition
    
    lda #45
    sta P1XPosition
    
    lda #30
    sta P0YPosition
    
    lda #31
    sta P1YPosition
    
    lda #9
    sta P0Height

    lda #7
    sta P1Height

    lda #6
    sta P0CrouchedOffset
    
    lda #22
    sta PlayfieldStartPos
    
    lda #10
    sta PlayfieldHeight
    
    lda #%00110000
    sta PF0Variable
    
    lda #15
    sta MissileXOffset

    lda #0
    sta LastPressedKey
    
    lda #%00010101
    sta NUSIZ0

    lda #1
    sta VDELP1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the pointers to the correct lookup addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #<P0SpriteCrouchingRight
    sta P0SpritePtr
    lda #>P0SpriteCrouchingRight
    sta P0SpritePtr+1

    lda #<P0ColorCrouched
    sta P0ColorPtr
    lda #>P0ColorCrouched
    sta P0ColorPtr+1

    lda #<P1SpriteZerglingLeft
    sta P1SpritePtr
    lda #>P1SpriteZerglingLeft
    sta P1SpritePtr+1

    lda #<P1ColorZergling
    sta P1ColorPtr
    lda #>P1ColorZergling
    sta P1ColorPtr+1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the main display loop and frame rendering.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FrameLoop:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the X position of our objects just before we start rendering.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda P0XPosition
    ldy #0
    jsr SetObjectXPos

    lda P1XPosition
    ldy #1
    jsr SetObjectXPos

    jsr UpdateMissileXPosition
    lda MissileXPos
    ldy #2
    jsr SetObjectXPos

    sta WSYNC
    sta HMOVE    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VSYNC
    sta VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw the 3 recommended lines of VSYNC, and then turn it off.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 3
        sta WSYNC
    REPEND
    lda #0
    sta VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw the 37 recommended lines of VBLANK and then turn it off.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 30
        sta WSYNC
    REPEND
    sta VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw the 192 visible scanlines.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLines:
    lda #$AE
    sta COLUBK                ; Set the background color to light blueish
    lda #$DE
    sta COLUPF                ; Set the playfield color to a grassy green color.
    lda #%00000001
    sta CTRLPF                ; Set the option to reflect, NOT repeat

    sta CXCLR                ; clear all collisions registers
    ldx #64   
.VisibleLineLoop:

DRAW_MISSILE:
    lda #0                 ; start accumualtor with 0 (null position)
    cpx MissileYPos        ; compare X/scanline with missile y-position
    bne .SKIP_MISSILE_DRAW ; if is not equal, skip the draw of missile0
    lda #%00000010         ; and set ENABL second bit to enable missile

.SKIP_MISSILE_DRAW:
    sta ENAM0             ; store correct value in the TIA missile register

.AreWeInsidePlayer0:
    txa
    sec
    sbc P0YPosition
    cmp P0Height
    bcc .DrawPlayer0
    lda #0

.DrawPlayer0:
    clc
    adc P0CrouchedOffset
    tay 
    lda (P0ColorPtr),Y
    sta WSYNC
    sta COLUP0
    tya
    clc
    adc P0FacingOffset
    tay
    lda (P0SpritePtr),Y 
    sta GRP0 

.AreWeInsidePlayer1:
    txa 
    sec
    sbc P1YPosition
    cmp P1Height
    bcc .DrawPlayer1
    lda #0

.DrawPlayer1:
    clc
    adc P1SpriteOffset
    tay
    lda (P1SpritePtr),Y
    sta WSYNC
    sta GRP1
    lda (P1ColorPtr),Y
    sta COLUP1

AreWeInsidePlayField:
    ldy #0
    txa
    sec
    sbc PlayfieldStartPos
    cmp PlayfieldHeight
    lda #$FF
    bcc .DisableP0
    lda #0
    jmp .DrawPlayfield

.DisableP0:
    sta WSYNC
    sty GRP0
    sty GRP1
    jmp .FallbackDrawPlayfield

.DrawPlayfield:
    sta WSYNC
.FallbackDrawPlayfield:
    sta PF1
    sta PF2
    ora PF0Variable
    sta PF0
    
GoToNextLine:
    dex                 
    bne .VisibleLineLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on VBLANK again and draw the 30 recommended overscan lines.
;; And then turn VBLANK off again.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2                     ; Load the accumulator with the value 2
    sta VBLANK                 ; Store the value 2 in VBLANK to enable VBLANK
    REPEAT 30
        sta WSYNC              ; Wait for the 30 overscan lines.
    REPEND
    lda #0                     ; Load the accumulator with the value 0. 
    sta VBLANK                 ; Store 0 in VBLANK to turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check the user input after the overscan and before we start a new frame.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000              ; Load the accumulator with the binary value 00010000 
    bit LastPressedKey
    bne CheckP0Down
    bit SWCHA                   ; Test the SWCHA register to see if the joystick is pointing upwards.
    bne CheckP0Down             ; If a 0 is present at D4 of SWCHA we have a joystick pointing up.
    ldx #0
    stx MissileXFineOffset
    
    ldy #6
    ldx #9
    lda #0                      ; Load the accumulator with the value 0
    cmp P0CrouchedOffset
    bcs IncrementCrouchedOffset
    ldx #11
    stx MissileXFineOffset

    ldx #16
    ldy #15
IncrementCrouchedOffset:
    sty P0CrouchedOffset        ; store 0 at the P0Crouched address offset so we point to the correct bitmap.
    stx P0Height                ; Store the value 9 in P0Height to ensure we have the correct height set for our upright sprite.
    lda #%00010000
    sta LastPressedKey

CheckP0Down:
    lda #%00100000              ; Load the accumulator with the binary value 00100000
    bit LastPressedKey
    bne CheckP0Left
    bit SWCHA                   ; Test the SWCHA register to see if the joystick is pointing down.
    bne CheckP0Left             ; If a 0 is present at D5 of SWCHA we have a joystick pointing down.
    ldx #0
    stx MissileXFineOffset

    ldy #6
    ldx #9
    lda #14
    cmp P0CrouchedOffset
    bcc DecrementCrouchedOffset
    ldx #6
    ldy #0
DecrementCrouchedOffset:
    sty P0CrouchedOffset        ; store 9 at the P0Crouched address offset so we point to the correct bitmap.
    stx P0Height                ; Store the value 6 in P0Height to ensure we have the correct height set for our crouched sprite.
    lda #%00100000
    sta LastPressedKey

CheckP0Left:
    lda #%01000000              ; Load the accumulator with the binary value 01000000
    bit SWCHA                   ; Test the SWCHA register to see if the joystick is pointing left. 
    bne CheckP0Right            ; If a 0 is present at D6 of SWCHA we have a joystick pointing left.
    lda #31                     ; Load the accumulator with the value 14   
    sta P0FacingOffset          ; store 14 at the P0FacingOffset address offset so we point to the correct bitmap.
    lda #3                      ; Load the accumulator with the value 3 
    sta MissileXOffset          ; Store the missile X offset corresponding to the left facing sprite in preparation for missile fire.
    lda P0XPosition             ; Decrement the X position of our sprite because the Joystick is pushed to the left.
    sec
    sbc #1
    jsr ValidateDecrementedX
    sta P0XPosition
    

CheckP0Right:
    lda #%10000000              ; Load the accumulator with the binary value 10000000
    bit SWCHA                   ; Test the SWCHA register to see if the joystick is pointing to the right.
    bne CheckP0Button           ; If a 0 is present at D7 of SWCHA we have a joystick pointing to the right.
    lda #0                      ; Load the accumulator with the value 0.
    sta P0FacingOffset          ; Store 0 at the P0FacingOffset address offset so we point to the correct bitmap.
    lda #15                     ; Load the accumulator with the value 15.
    sta MissileXOffset          ; Store the missile X offset corresponding to the right facing sprite in preparation for missile fire.
    lda P0XPosition             ; Increment the X position of our sprite because the Joystick is pushed to the right.
    clc
    adc #1
    jsr ValidateIncrementedX
    sta P0XPosition

CheckP0Button:
    lda #%10000000              ; Load the accumulator with the binary value 10000000
    bit INPT4                   ; Test the INPT4 register to see if the player0 trigger button is pressed. 
    bne EndInputCheck           ; If a 0 is present at D7 of INPT4 we know that the trigger button is pressed.

    jsr SetMissileYPosition
    jsr SetMissileXPosition
    lda #0                      ; Load the accumulator with the value 0
    sta RESMP0                  ; Store 0 in RESMP0 thereby activating the missile allowing it to be fired.
    jmp EndInputCheck

SetMissileYPosition subroutine
    ldy #$FC
    ldx #4
    lda P0CrouchedOffset
    cmp #15
    beq .ChangeMissileYPos
    ldy #$FE
.ChangeMissileYPos:
    tya
    adc P0YPosition             ; Load the accumulator with the Player0 Y position.
    clc
    adc P0Height                ; Add to the y-position the P0Height.
    sta MissileYPos             ; Store the result in the variable MissileYPos.
    rts



EndInputCheck:                  ; Unused.
    lda #%00110000
    and SWCHA
    cmp #%00110000
    beq ResetLastPressed
    jmp CheckMissilePFCollision
ResetLastPressed:
    lda #0
    sta LastPressedKey

CheckMissilePFCollision:
    lda #%10000000              ; Load the accumulator with the binary number 10000000
    bit CXM0FB                  ; Test the CXM0FB register to see if there is a colission between the missile and the playfield.
    bne DisableMissile          ; If a zero is present at D7 of of CXM0FB we have a colission and we go to DisableMissile.     

CheckMissileP1Collision:
    bit CXM0P
    bne PlayerHitAI
    jmp GetNextFrame            ; Otherwise we jump to the next frame of our game.

PlayerHitAI:
    lda #0
    sta NUSIZ1
    sta P1Height
DisableMissile:
    lda #2                      ; Load the accumulator with the integer value 2 
    sta RESMP0                  ; Store 2 in RESMP0 disabling the missile and locking it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jump back to "DisplayLoop" to start a brand new frame of our game.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetNextFrame:
    lda P1XPosition
    clc
    adc #1
    jsr ValidateIncrementedX
    sta P1XPosition
    jmp FrameLoop               ; Jump to the next frame of the game.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The subroutine used to set the X position of our objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
    sta WSYNC
    sec

.Div15Loop:
    sbc #15
    bcs .Div15Loop
    eor #7
    asl
    asl
    asl
    asl
    sta HMP0,Y
    sta RESP0,Y
    rts

SetMissileXPosition subroutine
    lda MissileXOffset
    sec
    sbc MissileXFineOffset
    tay

    ldx #%11000000              ; Load the X register with the binary number 11000000
    lda P0FacingOffset          ; Load the Accumulator with the P0FacingOffset variable 
    cmp #0                      ; Compare the "sprite facing" offset with 0 to determine whether we are facing right or left.
    beq .ChangeMissileXPos      ; If the offset is 0 go to SetMissileDirection with 11000000 representing right. 
    
    lda MissileXOffset
    clc
    adc MissileXFineOffset
    tay
    
    ldx #%00110000              ; Otherwise go to SetMissileDirection with 00110000 representing left.
.ChangeMissileXPos:
    stx MissileDirection        ; Store the missile direction  in the variable MissileDirection.
    tya
    clc
    adc P0XPosition             ; Load the accumulator with the X position of our player sprite.
    sta MissileXPos             ; Store the missile X Position in the MissileXPos variable. 
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The subroutine to determine the next x position of our missile.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateMissileXPosition subroutine
    lda #%11000000
    bit MissileDirection
    bne .IncrementX
    jmp .DecrementX

.IncrementX:
    REPEAT 4
        inc MissileXPos
    REPEND
    lda MissileXPos
    jsr ValidateIncrementedX
    sta MissileXPos    
    rts

.DecrementX:
    REPEAT 4
        dec MissileXPos
    REPEND
    lda MissileXPos
    jsr ValidateDecrementedX
    sta MissileXPos    
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validate X position subroutine.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ValidateIncrementedX subroutine
    cmp #$A0
    bcc .StoreIncrementedX
    lda #$00
.StoreIncrementedX:
    rts

ValidateDecrementedX subroutine
    cmp #$A0
    bcc .StoreDecrementedX
    lda #$A0
.StoreDecrementedX:
    rts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;---Graphics Data from PlayerPal 2600---
P0SpriteCrouchingRight:
    .byte #%00000000;Padding
    .byte #%11111100;--
    .byte #%11111100;--
    .byte #%11011000;--
    .byte #%10111111;--
    .byte #%11010001;--

P0SpriteUprightRight:
    .byte #%00000000;Padding
    .byte #%11000000;--
    .byte #%10000000;--
    .byte #%10000000;--
    .byte #%11100000;--
    .byte #%11110000;--
    .byte #%11011000;--
    .byte #%10111111;--
    .byte #%11001001;--

P0SpriteUrightRightUp:
    .byte #%00000000;Padding
    .byte #%11000000;--
    .byte #%10000000;--
    .byte #%10000000;--
    .byte #%11100000;--
    .byte #%11110000;--
    .byte #%11110000;--
    .byte #%10111000;--
    .byte #%11010000;--
    .byte #%00010000;--
    .byte #%00010000;--
    .byte #%00010000;--
    .byte #%00110000;--
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000

P0SpriteCrouchingLeft:
    .byte #%00000000;Padding
    .byte #%00111111;--
    .byte #%00111111;--
    .byte #%00011011;--
    .byte #%11111101;--
    .byte #%10001011;--

P0SpriteUprightLeft:
    .byte #%00000000;Padding
    .byte #%00000011;--
    .byte #%00000001;--
    .byte #%00000001;--
    .byte #%00000111;--
    .byte #%00001111;--
    .byte #%00011011;--
    .byte #%11111101;--
    .byte #%10001011;--

P0SpriteUprightLeftUp:
    .byte #%00000000;Padding
    .byte #%00000011;--
    .byte #%00000001;--
    .byte #%00000001;--
    .byte #%00000111;--
    .byte #%00001111;--
    .byte #%00001111;--
    .byte #%00011101;--
    .byte #%00001011;--
    .byte #%00001000;--
    .byte #%00001000;--
    .byte #%00001000;--
    .byte #%00001100;--
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000

P1SpriteZerglingLeft:
    .byte #%00000000;Padding
    .byte #%00010010;--
    .byte #%00100100;--
    .byte #%01111100;--
    .byte #%00111100;--
    .byte #%10100010;--
    .byte #%01000001;--

P1SpriteHydraliskRight:
    .byte #%00000000;Padding
    .byte #%11111101;$6C
    .byte #%00001101;$6C
    .byte #%00011110;$6C
    .byte #%00111100;$6C
    .byte #%00011110;$6C
    .byte #%00001101;$6C
    .byte #%00111110;$6C
    .byte #%00100000;$6C

P1SpriteHydraliskLeft:
    .byte #%00000000;Padding
    .byte #%10111111;$6C
    .byte #%10110000;$6C
    .byte #%01111000;$6C
    .byte #%00111100;$6C
    .byte #%01111000;$6C
    .byte #%10110000;$6C
    .byte #%01111100;$6C
    .byte #%00000100;$6C

P1SpriteHydraliskDead:
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000

;---End Graphics Data---


;---Color Data from PlayerPal 2600---
P0ColorCrouched:
    .byte #$00;Padding
    .byte #$72;
    .byte #$70;
    .byte #$20;
    .byte #$04;
    .byte #$04;

P0ColorUpright:
    .byte #$00;Padding
    .byte #$02;
    .byte #$72;
    .byte #$72;
    .byte #$70;
    .byte #$70;
    .byte #$20;
    .byte #$04;
    .byte #$04;

P0ColorUprightUp:
    .byte #$00;Padding
    .byte #$02;
    .byte #$72;
    .byte #$72;
    .byte #$70;
    .byte #$70;
    .byte #$20;
    .byte #$04;
    .byte #$04;
    .byte #$04;
    .byte #$04;
    .byte #$04;
    .byte #$04;
    .byte #$04;
    .byte #$04;
    .byte #$04;

P1ColorZergling:
    .byte #$26; Padding
    .byte #$26;
    .byte #$26;
    .byte #$64;
    .byte #$64;
    .byte #$64;
    .byte #$24;

P1ColorHydralisk:
    .byte #$6C;
    .byte #$6C;
    .byte #$6C;
    .byte #$6C;
    .byte #$6C;
    .byte #$6C;
    .byte #$6C;
    .byte #$6C;
    .byte #$6C;
    
;---End Color Data---

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC               ; Move to memory position $FFFC
    word Reset              ; Write 2 bytes with the program reset address
    word Reset              ; Write 2 bytes with the interruption vector