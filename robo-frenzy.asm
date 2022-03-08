;         V I C   R O B O - F R E N Z Y 
;
;             originally by Davide Bucci, April-June 2018
;             modified by Nicholas Baldridge (foramusementonlypodcast@gmail.com)
;
; This program is a version of the new EM arcade game "Robo-Frenzy"
; that runs on an unexpanded VIC-20.  See the following URL for more info:
; https://pinside.com/pinball/forum/topic/new-em-arcade-game
;
; The assembler used is ca65
;
; The tentacles are described by four arrays:
; BombSpeed, BombPosX, BombPosY and BombPosOY. Their name should be quite
; self-descriptive, except for BombPosOY, that contains the old position of the
; bombs (Y coordinate, as bombs fall vertically) and is used for erasing the
; bombs when they are to be drawn at the new coordinate. Speed is positive for
; bombs falling.
;
; The cannon shots operates with a very similar principle with respect to bombs
; and are described by FireSpeed, FirePosX, FirePosY and FirePosOY. The only
; difference is that a positive speed means that shots move upwards.
;
; A speed of $FF (or 255 in decimal) means that the bomb is exploding and
; should be destroyed; i.e. erased from the screen and then deactivated, by
; putting a final speed of 0.
;
; Plenty of things are done during the IRQ handling routine, synchronized with
; the PAL refresh rate of the monitor. For this reason, expect a lot of flicker
; in the aliens when this game is played on a NTSC machine. Differences between
; PAL and NTSC include a different screen height that is 31 rows for PAL
; machines and 28 for NTSC ones.

;; Mothership should be robot
;; alien should be tentacles
;; cannon should be player

; Difficulty-related constants
        PERIODS = 5         ; Initial difficulty (less=faster)

; General-use addresses
        GRCHARS1 = $1C00    ; Address of user-defined characters. Since in the
                            ; unexpanded VIC the screen matrix starts at
                            ; $1E00, there are 512 bytes free, i.e. 64 chars
                            ; that can be defined. That leaves 3059 bytes free
                            ; for the machine language code (counting the
                            ; 752 SYS4109 stub in BASIC that launches the
                            ; program.

; Colour constants for the VIC 20
        BLACK    = $00
        WHITE    = $01
        RED      = $02
        CYAN     = $03
        MAGENTA  = $04
        GREEN    = $05
        BLUE     = $06
        YELLOW   = $07
        MULTICOLOUR = $08

; KERNAL routines used
        GETIN = $FFE4

; Page-0 addresses used (for indirect indexed addressing and other things)

        LAB_01 = $01
        LAB_02 = $02
        LAB_03 = $03
        LAB_04 = $04
        LAB_05 = $05
        LAB_06 = $06
        LAB_07 = $07
        LAB_08 = $08
        LAB_09 = $09
        LAB_0A = $0A
        LAB_0B = $0B

        CharCode = LAB_03   ; Employed in DrawChar
        PosX = LAB_04
        PosY = LAB_05
        Colour = LAB_06     ; Colour to be used by the printing routines
        tmpindex = LAB_07   ; Temporary variables
        tmpx = LAB_08
        tmpy = LAB_09
        tmp4 = LAB_0A
        SPRITECH  = $0C     ; Pointer to the group of 4 ch. for a sprite (word)
        CHRPTR    = $0E     ; Pointer to the original ch. in a sprite (word)
        SpriteX   = $10     ; X position (offset in a char) of a sprite (byte)
        SpriteY   = $11     ; Y position (offset in a char) of a sprite (byte)
        CharShr   = $12     ; Employed in LoadSprite
        PixPosX   = $14     ; Position in characters
        ColourRead= $15     ; Colour read by GetChar
        POSCHARPT = $1A     ; Pointer for a character in memory (word)
        POSCOLPT  = $1C     ; Pointer for a colour in memory (word)
        Tentaclecntr  = $1E     ; Counter for tentacle interrupts
        PixPosXO  = $1F     ; Old Position in characters
        TmpScan   = $20     ; Used in raster line sync code
        Random    = $21     ; Position where to store a random word by GetRand
        IrqCn     = $23     ; Counter for interrupt
        keyin     = $24     ; Last key typed.
        Val       = $25     ; Used for the BCD conversion (word)
        Res       = $27     ; The result of the BCD conversion (3 bytes)
        Joystick  = $2A     ; Different from zero if the joystick was used
        Period    = $2C     ; Higher = slower alien movement
        Direction = $3E     ; The first bit indicates aliens' X direction
        CannonPos = $3F     ; Horizontal position of the cannon (in characters)
        OldCannonP= $40     ; Old position of the cannon
        Win       = $41     ; If 1, the level is won. If $FF, game over
        Score     = $42     ; Current score (divided by 10) (word)
        HiScore   = $44     ; High Score (divided by 10) (word)
        TentaclePeriod= $46     ; Period for updating bomb positions
        Level     = $47     ; Current level
        tmpp      = $48     ; Temp for bomb dropping position
        CannonYPos= $4B
        BunkerY   = $4D

        VoiceBase = $4E

        Voice1ptr = $4F
        Voice1ctr = $50
        Loop1ctr  = $51
        Loop1str  = $52
        Voice1drt = $53
        Voice1nod = $54

        Voice2ptr = $55
        Voice2ctr = $56
        Loop2ctr  = $57
        Loop2str  = $58
        Voice2drt = $59

        Voice2nod = $60

        tprnd1    = $61
        tprnd2    = $62

        OldCannonY= $65
        GearPos   = $66
        GearPosY  = $68
        RobotNum  = $69
        GearHeld  = $6A


        INITVALC=$ede4

; VIC-chip addresses
        VICSCRHO = $9000    ; Horizontal position of the screen
        VICSCRVE = $9001    ; Vertical position of the screen
        VICCOLNC = $9002    ; Screen width in columns and video memory addr.
        VICROWNC = $9003    ; Screen height, 8x8 or 8x16 chars, scan line addr.
        VICRAST  = $9004    ; Bits 8-1 of the current raster line
        VICCHGEN = $9005    ; Character gen. and video matrix addresses.
        GEN1     = $900A    ; First sound generator
        GEN2     = $900B    ; Second sound generator
        GEN3     = $900C    ; Third sound generator
        NOISE    = $900D    ; Noise sound generator
        VOLUME   = $900E    ; Volume and additional colour info
        VICCOLOR = $900F    ; Screen and border colours

        PORTAVIA1 = $9111   ; Port A 6522 (joystick)
        PORTAVIA1d = $9113  ; Port A 6522 (joystick)
        PORTBVIA2 = $9120   ; Port B 6522 2 value (joystick)
        PORTBVIA2d = $9122  ; Port B 6522 2 direction (joystick

        MEMSCR   = $1E00    ; Start address of the screen memory (unexp. VIC)
        MEMCLR   = $9600    ; Start address of the colour memory (unexp. VIC)

        REPEATKE = $028A    ; Repeat all keys

        VOICE1  = GEN1      ; Voice 1 for music
        VOICE2  = GEN2      ; Voice 2 for music
        EFFECTS = GEN3      ; Sound effects (not noise)

.export main
.segment "STARTUP"
.segment "LOWCODE"
.segment "INIT"
.segment "GRCHARS"
.segment "CODE"

; Main program here.

main:       jsr Init        ; Init the game (load graphic chars, etc...)
restart:    jsr StartGame   ; Set the starting values of game variables
mainloop:   lda Joystick
            beq @ccc
            jsr ShortDelay
            lda #$00
            sta Joystick
@ccc:       jsr GETIN       ; Main loop waiting for keyboard events
            sta keyin
            lda #$ff
            sta Joystick
            lda PORTAVIA1
            and #%00010000  ; Left
            beq left
            lda PORTBVIA2
            and #%10000000  ; Right
            beq right
            lda PORTAVIA1
            and #%00100000  ; Fire
            beq fire
            lda #$00
            sta Joystick
            lda keyin
            beq mainloop
            cmp #$0D        ; Wait for return if the game stopped
            bne @norestart
            lda Win         ; If the game has stopped, restart
            bne restart
@norestart: lda keyin
            cmp #$58        ; X: increase position of the cannon (right)
            beq right
            cmp #$5A        ; Z: decrease position of the cannon (left)
            beq left
            cmp #$20        ; Space: fire!
            beq fire
            cmp #$4D        ; M toggle music on/off
            bne mainloop
            lda VoiceBase
            eor #$80
            sta VoiceBase
@continue4: jmp mainloop

fire:       lda Win         ; If the game has stopped, restart
            bne restart
            ;ldx #0          ; Search for the first free shot

right:      
            lda CannonPos
            inc CannonPos
            ;sta CannonPos
            inc CannonYPos
            cmp #7
            bcc @continue
            lda #7
            sta CannonPos
            ldy #19
            sty CannonYPos
            sta OldCannonY
            lda CannonPos
            cmp #7
            beq @gearcheck
@continue:  jmp mainloop
@gearcheck: lda #1
            sta GearHeld
            jmp DrawGear

left:       dec CannonPos
            bmi @zeroc
            dec CannonYPos
            lda CannonPos
            cmp #0
            beq @checkpos
            jmp mainloop
@checkpos:  lda GearHeld
            cmp #0
            beq @chkrobo
@chkrobo:   jmp CheckRobo
            ;rts

@zeroc:     lda #0
            sta CannonPos
            jmp mainloop


CheckRobo:
            lda GearHeld
            cmp #1
            beq @draw
            jmp mainloop
@draw:
            lda RobotNum
            cmp #0
            beq @p1
            cmp #1
            beq @p2
            cmp #2
            beq @p3
            cmp #3
            beq @p4
            cmp #4
            beq @p5
            cmp #5
            beq @p6
@p1:        jmp DrawPart1
            lda #$01            ; Update the score: +10 pts
            jsr AddScore
@p2:        jmp DrawPart2
@p3:        jmp DrawPart3
@p4:        jmp DrawPart4
@p5:        jmp DrawPart5
@p6:        jmp DrawPart6


; INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT
;
; Initialization code: prepare the screen to the correct size, center it and
; load the graphic chars and configure the IRQ handler.
;
; INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT

Init:
            lda #$80        ; Autorepeat on on the keyboard
            sta REPEATKE
            lda #$08        ; Define screen colour and background (black)
            sta VICCOLOR
            lda #$90        ; Set a 16 column-wide screen
            sta VICCOLNC
            lda INITVALC
            cmp #$05
            beq CenterScreenNTSC    ; Load the screen settings
            bne CenterScreenPAL
ContInit:   sty VICSCRVE    ; Centre the screen vertically...
            stx VICSCRHO    ; ... and horizontally
            lda #$FF        ; Move the character generator address to $1C00
            sta VICCHGEN    ; while leaving ch. 128-255 to their original pos.
            jsr MovCh       ; Load the graphic chars
            sei             ; Configure the interrupt handler
            lda INITVALC
            cmp #$05
            beq SyncNTSC    ; Load the screen settings
            bne SyncPAL
ContInit1:  stx $9125       ; Set up the timer
            sta $9126
            lda #<IrqHandler; And the IRQ handler
            sta $0314
            lda #>IrqHandler
            sta $0315
            cli
            jsr ZeroScore   ; A contains 0 after this routine
            sta Level
            sta HiScore     ; Zero the high score
            sta HiScore+1
            sta PORTAVIA1d  ; Prepare VIAs for joystick
            lda #$7F
            sta PORTBVIA2d
            rts

; Synchronize raster on PAL systems

SyncPAL:
            ; Data for PAL machines. See for example:
            ; http://www.antimon.org/dl/c64/code/stable.txt
            LINES_PAL = 312
            CYCLES_PER_LINE_PAL = 71
            TIMER_VALUE_PAL = LINES_PAL * CYCLES_PER_LINE_PAL - 2
@loopsync:  lda VICRAST     ; Synchronization loop
            cmp #112
            bne @loopsync
            lda #<TIMER_VALUE_PAL
            ldx #>TIMER_VALUE_PAL
            jmp ContInit1


SyncNTSC:
            ; Data for NTSC machines. See for example:
            ; http://www.antimon.org/dl/c64/code/stable.txt
            LINES_NTSC = 261
            CYCLES_PER_LINE_NTSC = 65
            TIMER_VALUE_NTSC = LINES_NTSC * CYCLES_PER_LINE_NTSC - 2
@loopsync:  lda VICRAST     ; Synchronization loop
            cmp #70
            bne @loopsync
            lda #<TIMER_VALUE_NTSC
            ldx #>TIMER_VALUE_NTSC
            jmp ContInit1


; Screen init value for PAL and NTSC

CenterScreenPAL:
            lda #16
            sta CannonYPos
            lda #$3E        ; Set a 31 row-high column
            sta VICROWNC
            lda #29
            sta BunkerY
            ldx #$12
            ldy #$16
            jmp ContInit

CenterScreenNTSC:
            lda #12
            sta CannonYPos
            lda #$36        ; Set a 27 row-high column
            sta VICROWNC
            lda #19
            sta BunkerY
            ldx #$0A
            ldy #$10
            jmp ContInit

StartGame:
            sei
            lda #$2F        ; Turn on the volume, set multicolour add. colour 2
            sta VOLUME
            lda #$FF
            sta OldCannonP
            lda #0
            sta CannonPos   ; Initial position of the cannon
            lda #0
            sta GearHeld
            sta RobotNum
            lda #$00
            sta Direction
            sta Win
            sta IrqCn
            sta NOISE
            jsr ConfLevel
@loopp:     ;lda #$00
            lda #EMPTY
            dex
            bne @loopp
            lda #EMPTY      ; Clear the screen
            jsr CLS
            lda #BLACK
            jsr PaintColour
            jsr DrawShield
            lda #32
            bit VoiceBase
            bpl @musicok
            lda #$A0
@musicok:   sta VoiceBase
            jsr draw1l
            cli
            rts

; Put zero in the current score

ZeroScore:  lda #$00
            sta Score
            sta Score+1
            rts

; Configure the level (in Level)

ConfLevel:  ldx Level
            cpx #NUMLEVEL-1
            bmi @validlevel
            ldx #NUMLEVEL-1
            stx Level
@validlevel:;lda LevelsBomb,x
            sta TentaclePeriod
            lda LevelsPer,x
            sta Period
            rts

; Draw the shields in the following positions:
; ------------1---
; 0---4---8---2---
;  **  **  **  **

DrawShield: ldx #8
            ldy BunkerY
            lda Period
            lda #MAGENTA
            sta Colour
            lda #1
            jsr DrawChar
            inx
            ldx #15
            ldy #9
            lda Period
            sta Colour
            lda #3
            jsr DrawChar
            ldx #15
            ldy #10
            lda Period
            sta Colour
            lda #2
            jsr DrawChar

draw1l:
            lda Score       ; Load the current score and convert it to BCD
            sta Val
            lda Score+1
            sta Val+1
            jsr Bin2BCD
            lda #WHITE
            ldx #0
            jsr PrintRes
            lda #(48+$80)   ; Write a zero, to multiply x10 the score
            jsr DrawChar
            lda Score+1
            cmp HiScore+1
            bcc @noupdate
            beq @nou
            jsr UpdateHiSc
@nou:       lda Score       ; Update the high score if needed
            cmp HiScore
            bcc @noupdate
            jsr UpdateHiSc
@noupdate:  lda HiScore     ; Load the current hi score and convert it to BCD
            sta Val
            lda HiScore+1
            sta Val+1
            jsr Bin2BCD
            ldx #09
            lda #CYAN
            jsr PrintRes
            lda #(48+$80)   ; Write an additional zero
            jmp DrawChar

UpdateHiSc: lda Score       ; Update the high score
            sta HiScore
            lda Score+1
            sta HiScore+1
            rts

PrintRes:   sta Colour
            ldy #0
            lda Res+2       ; Print all the BCD chars
            jsr PrintBCD
            lda Res+1
            jsr PrintBCD
            lda Res
            jsr PrintBCD
            rts

; Copy the graphic chars. They are subjected to be changed during the pixel-by
; pixel movement, so that routine moves only the characters not used as sprites

MovCh:      ldx #(LASTCH+1)*8+1
@loop:      lda DefChars-1,x
            sta GRCHARS1-1,x
            dex
            bne @loop
            rts

; IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ
;
; This is the interrupt handler, called 50 times each second when the VIC-20
; is a PAL unit or 60 when NTSC. It does the following things:
;
; 1 - Calculate positions of the aliens and, if necessary, redraw them
; 2 - Calculate positions of the falling bombs and fire shoots and draw them
; 3 - Update the position of the cannon and draw it
; 5 - Check for collisions and handle explosions
; 6 - Call the sound drivers
; 7 - Jump to the original IRQ handler (for scanning the keyboard, etc).
;
; The user interface is handled outside of the interrupt, in the main program
; loop and the communication with the IRQ handler is made by a set of
; appropriate flags. This approach has the following advantages:
;
; - The speed of the aliens and of the cannon is controlled very precisely as
;   the IRQ handler is called at a predictable and stable rate.
; - The code for the visualization and for the user interface (i.e. recognizing
;   keyboard and joystick movements) is kept separate.
;
; The main drawback is that the IRQ is supposed to do many things and it should
; be doing that VERY RAPIDLY in order not to mess with the calling order. As a
; rule of thumb, I would say that the IRQ should be completed in less than 5 ms
; at least most of times.
;
; IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ

IrqHandler: pha
            txa             ; Save registers
            pha
            tya
            pha
@still:
            ldx #$00
            stx EFFECTS
@nomute:    ;lda ExplMCnt
@noerMexp:  bit Win         ; If Win <0 stop the game
            bpl @contirq
            jmp @exitirq
@contirq:   lda IrqCn
            cmp Period      ; Execute every PERIOD/60 of second
            beq @contint
            jmp @cont3
@contint:   ldx #$00
            stx IrqCn
            stx NOISE
            pha
            lsr
            lsr
            bit VoiceBase   ; If music is active, VoiceBase is linked to the
            bmi @nomusic    ; vertical alien position (music becomes higher
            sta VoiceBase   ; pitched when aliens scroll down).
@nomusic:   lsr
            pla
            and #7
            sta SpriteY     ; Calculate Y shift in pixel
            jsr MoveTentacles   ; Make bombs fall. Aliens will be on top of bombs
            bit Win         ; If Win <0 stop the game
            bmi @exitirq
@draw:      lda PixPosX
            ror
            bcs @altaliens
            ldx #TENTACLE1
            ldy #TENTACLE1
@altaliens: ldx #TENTACLE1
            ldy #TENTACLE1
@cont3:     lda CannonPos   ; Check if the cannon position has changed
            cmp OldCannonP
            beq @nochange
            jsr ClearCannon ; If yes, redraw it
            lda GearHeld
            cmp #1
            beq @geardraw
@geardraw:  jsr ClearGear
@nochange:  jsr DrawCannon
            inc IrqCn
@exitirq:   bit Win
            bmi @nomusic1
            jsr Music1
            jsr Music2
@nomusic1:
            pla             ; Restore registers
            tay
            pla
            tax
            pla
            jmp $EABF       ; Jump to the standard IRQ handling routine


; Draw the cannon on the screen, at the current position, contained in
; CannonPos (in characters).

DrawCannon: lda CannonPos
            sta OldCannonP
            tax
            ldy CannonYPos      ; Vertical position of the cannon
            sty OldCannonY
            lda #WHITE          ; Cannon in white
            sta Colour
            lda #PLAYER         ; Cannon char
            jmp DrawChar

; Draw the gear on the screen, at the position specified in 
; CannonPos (x + 1), CannonPosY - 1
; Lower Y = higher, lower X = further left

DrawGear: 
          lda CannonPos
          sta GearPos
          inc GearPos
          ldx GearPos
          lda CannonYPos
          sta GearPosY
          dec GearPosY
          ldy GearPosY
          lda #YELLOW
          sta Colour
          lda #GEAR
          jmp DrawChar

; Clear the Gear at last remembered position

ClearGear:
          ldx GearPos
          ldy GearPosY
          lda #WHITE
          sta Colour
          lda #EMPTY
          jmp DrawChar

DrawPart1:
          lda RobotNum
          inc RobotNum
          sta RobotNum
          ldx #1
          ldy #5
          lda #BLUE
          sta Colour
          lda #LLEG
          jmp DrawChar

DrawPart2:
          lda RobotNum
          inc RobotNum
          sta RobotNum
          lda #$01            ; Update the score: +10 pts
          jsr AddScore
          lda #BLUE
          sta Colour
          lda #RLEG
          ldx #3
          ldy #5
          jmp DrawChar

DrawPart3:
          lda RobotNum
          inc RobotNum
          sta RobotNum
          lda #$01            ; Update the score: +10 pts
          jsr AddScore
          lda #BLUE
          sta Colour
          lda #LARM
          ldx #1
          ldy #4
          jmp DrawChar

DrawPart4:
          lda RobotNum
          inc RobotNum
          sta RobotNum
          lda #$01            ; Update the score: +10 pts
          jsr AddScore
          lda #BLUE
          sta Colour
          lda #TORSO
          ldx #2
          ldy #4
          jmp DrawChar

DrawPart5:
          lda RobotNum
          inc RobotNum
          sta RobotNum
          lda #$01            ; Update the score: +10 pts
          jsr AddScore
          lda #BLUE
          sta Colour
          lda #RARM
          ldx #3
          ldy #4
          jmp DrawChar

DrawPart6:
          lda RobotNum
          inc RobotNum
          sta RobotNum
          lda #$02            ; Update the score: +10 pts
          jsr AddScore
          lda #BLUE
          sta Colour
          lda #HEAD
          ldx #2
          ldy #3
          jmp DrawChar

ClearRobo:
          ldx #2
          ldy #3
          lda #WHITE
          sta Colour
          lda #EMPTY
          jmp DrawChar

          ldx #3
          ldy #4
          lda #WHITE
          sta Colour
          lda #EMPTY
          jmp DrawChar

          ldx #2
          ldy #4
          lda #WHITE
          sta Colour
          lda #EMPTY
          jmp DrawChar

          ldx #1
          ldy #4
          lda #WHITE
          sta Colour
          lda #EMPTY
          jmp DrawChar
          
          ldx #3
          ldy #5
          lda #WHITE
          sta Colour
          lda #EMPTY
          jmp DrawChar
          
          ldx #1
          ldy #5
          lda #WHITE
          sta Colour
          lda #EMPTY
          jmp DrawChar

; Clear the cannon on the screen, at the current position, contained in
; OldCannonP (in characters).

ClearCannon:
            ldx OldCannonP
            ldy OldCannonY      ; Vertical position of the cannon
            lda #WHITE          ; Cannon in white
            sta Colour
            lda #EMPTY          ; Space
            jmp DrawChar

; Wait for the wanted rasterline (NTSC only)

Waitrast:
Waitrast1:  lda VICRAST     ; Wait if there is a risk of flicker
            lsr             ; Divide by 4 as VICRAST contains bits 1:8 of
            lsr             ; the raster line position -> position in chars
            clc
            sta TmpScan
            clc
            adc #5
            lda TmpScan
            sec
            sbc #10
            jmp Waitrast1
@noflicker:
            rts

; End of PAL-only code

; Add the value contained in A to the current score

AddScore:   clc
            adc Score
            sta Score
            bcc @contadd
            inc Score+1
@contadd:   jsr draw1l
            rts

; Check if the player won the game.

CheckWin:   ;lda AliensR3s       ; Check if all aliens have been destroyed
            lda #$FF            ; If we come here, all aliens have been shot
            sta Win             ; That will stop the game
            lda #$00            ; Mute all effects
            sta NOISE
            sta EFFECTS
            ldx #4              ; write "YOU WON"
            ldy #15
            lda #YELLOW
            sta Colour
            lda #<YouWonSt
            sta LAB_01
            lda #>YouWonSt
            sta LAB_02
            jsr PrintStr
            lda #$00
            sta VOICE2
            lda #$B0            ; Win! Play a chime!
            sta VOICE1
            jsr Delay
            lda #$C0
            sta VOICE1
            jsr Delay
            lda #$D0
            sta VOICE1
            jsr Delay
            lda #$00
            sta VOICE1
            inc Level
@exit:      rts

; Game over! Zero the score, turn the screen to red and write "GAME OVER"

GameOver:   lda #$00            ; Mute all effects
            sta EFFECTS
            sta Level
            lda #$FF
            sta Win             ; Stop the game
            jsr ZeroScore       ; Put the score to zero
            lda #RED            ; Put all the screen in red (sooo bloody!)
            sta Colour
            jsr PaintColour
            lda #$B0            ; Explosion sound
            sta NOISE
            lda #$FF
@loop:      jsr ShortDelay
            sec
            sbc #$11
            sta VOLUME
            bne @loop
            lda #$00
            sta NOISE
            ldx #4              ; write "GAME OVER"
            ldy #15
            lda #<GameOverSt
            sta LAB_01
            lda #>GameOverSt
            sta LAB_02
            jsr PrintStr
@exit:      rts

; Control bombs dropping. A maximum of 8 bombs can be falling at the same
; time. A bomb is active and falling if its speed is greater than 0.
; BombSpeed, BombPosX and BombPosY are the arrays containing the speed and the
; positions. Exploit tmpindex and tmpx, change registers A, X, Y.
; This routine does three things:
;
; 1 - For each alien alive, and for each of the NMBOMBS bombs available, decide
;     if a bomb is dropped by drawing a random number and check if it is inside
;     a given interval
; 2 - Update the positions of the bombs active in the screen and check for
;     collisions.
; 3 - Draw the bombs in the new positions on the screen.
;

MoveTentacles:  inc Tentaclecntr
            lda Tentaclecntr
            cmp TentaclePeriod
            lda #0
            sta Tentaclecntr
            jsr GetRand         ; To avoid having to draw a pseudorandom number
            lda Random          ; each time, this is done once and the results
            sta tprnd1          ; are shifted inside two temporary registers
            lda Random+1        ; This yields a result random enough for the
            sta tprnd1+1        ; purposes of dropping bombs.

; Music driver for voice 1. It should be called every IRQ to handle music

Music1:     ldy Voice1ctr
            beq @playnext
            cpy Voice1nod
            bne @dec
            lda #$00
            sta VOICE1
@dec:       dey
            sty Voice1ctr
            rts

@playnext:  ldx Voice1ptr
            lda Voice1data,x
            cmp #repeatm
            beq @repeat
            cmp #endloop
            beq @endmloop
            and #maskcode
            cmp #loopcode
            beq @loopmusic
            cmp #notecode
            beq @note
            cmp #duracode
            beq @duration

@exitmusic: inx
            stx Voice1ptr
            rts

@loopmusic: lda Voice1data,x
            and #unmask
            sta Loop1ctr
            inx
            stx Voice1ptr
            stx Loop1str
            jmp @playnext

@endmloop:  ldy Loop1ctr
            dey
            sty Loop1ctr
            beq @exitloop
            ldx Loop1str
            stx Voice1ptr
            jmp @playnext
@exitloop:  inx
            stx Voice1ptr
            jmp @playnext

@note:      lda Voice1data,x
            and #unmask
            clc
            adc #128
            adc VoiceBase
            sta VOICE1
            lda Voice1drt
            sta Voice1ctr
            jmp @exitmusic

@duration:  lda Voice1data,x
            and #unmask
            sta Voice1drt
            inx
            lda Voice1data,x
            sta Voice1nod
            inx
            stx Voice1ptr
            jmp @playnext

@repeat:    ldx #$FF            ; That will overflow to 0 at the next inx
            jmp @exitmusic

; Music driver for the second voice. Very similar to voice 1.

Music2:     ldy Voice2ctr
            beq @playnext
            cpy Voice2nod
            bne @dec
            lda #$00
            sta VOICE2
@dec:       dey
            sty Voice2ctr
            rts

@playnext:  ldx Voice2ptr
            lda Voice2data,x
            cmp #repeatm
            beq @repeat
            cmp #endloop
            beq @endmloop
            and #maskcode
            cmp #loopcode
            beq @loopmusic
            cmp #notecode
            beq @note
            cmp #duracode
            beq @duration

@exitmusic: inx
            stx Voice2ptr
            rts

@loopmusic: lda Voice2data,x
            and #unmask
            sta Loop2ctr
            inx
            stx Voice2ptr
            stx Loop2str
            jmp @playnext

@endmloop:  ldy Loop2ctr
            dey
            sty Loop2ctr
            beq @exitloop
            ldx Loop2str
            stx Voice2ptr
            jmp @playnext
@exitloop:  inx
            stx Voice2ptr
            jmp @playnext

@note:      lda Voice2data,x
            and #unmask
            clc
            adc #128
            adc VoiceBase
            sta VOICE2
            lda Voice2drt
            sta Voice2ctr
            jmp @exitmusic

@duration:  lda Voice2data,x
            and #unmask
            sta Voice2drt
            inx
            lda Voice2data,x
            sta Voice2nod
            inx
            stx Voice2ptr
            jmp @playnext

@repeat:    ldx #$FF            ; That will overflow to 0 at the next inx
            jmp @exitmusic


; Draw the character in A in the position given by the X and Y registers
; Since the screen is 16 characters wide, we need to shift the Y register
; to multiply times 16 and then add the X contents. It uses PosX and PosY.
; Colour contains the colour code of the character. It uses 1 byte in the
; stack and does not change A, X, Y.

DrawChar:   cpx #16         ; Check if the X value is out of range
            bcs @exit       ; Exit if X greater than 16 (no of columns)
            cpy #31         ; Check if the Y value is out of range
            bcs @exit       ; Exit if Y greater than 31 (no of rows)
            sty PosY
            pha
            jsr PosChar
            ldy #0
            lda Colour
            sta (POSCOLPT),Y
            pla
            sta (POSCHARPT),Y
            ldy PosY
@exit:      rts

; Get the screen code of the character in the X and Y locations.
; The character is returned in A. The character colour is returned in Colour

GetChar:    cpx #16         ; Check if the X value is out of range
            bcs @exit       ; Exit if X greater than 16 (no of columns)
            cpy #31         ; Check if the Y value is out of range
            bcs @exit       ; Exit if Y greater than 31 (no of rows)
            sty PosY
            jsr PosChar
            ldy #0
            lda (POSCOLPT),Y
            sta ColourRead
            lda (POSCHARPT),Y
            ldy PosY
            rts
@exit:      lda #EMPTY
            rts

; Calculate the address of a screen position and put it in POSCHARPT. Do the
; same for the color address and put it in POSCOLPT.
; X and Y contain screen coordinates.

PosChar:    stx PosX
            lda #<MEMSCR
            sta POSCHARPT
            lda #>MEMSCR
            sta POSCHARPT+1
            lda #<MEMCLR
            sta POSCOLPT
            lda #>MEMCLR
            sta POSCOLPT+1
            tya
            asl             ; 16 columns per line. Multiply!
            asl
            asl
            asl             ; If it shifts an 1 in the carry, this means that
            bcc @nocorr     ; we need to write in the bottom-half of the screen
            inc POSCHARPT+1
            inc POSCOLPT+1
            clc
@nocorr:    adc PosX
            pha
            adc POSCHARPT
            sta POSCHARPT
            pla
            adc POSCOLPT
            sta POSCOLPT
            rts


; Put a "sprite", that is a 8x8 cell in a 2x2 character position.
; A contains the character code. It should be less than 32.
; The 4 characters are disposed as follows:
;
;    AC
;    BD
;
; Characters are redefined starting from address contained in SPRITECH
; SpriteX: the horizontal offset in pixels [0,8]
; SpriteY: the vertical offset in pixels [0,8]
; CharCode: the character to be used for the sprite
; Employs SPRITECH pointer to the group of 4 ch. for a sprite (word)
; and CHRPTR pointer to the original ch. in a sprite (word)

LoadSprite: clc
            lda SPRITECH        ; Calculate the vert. offset in ch. table
            adc SpriteY
            sta SPRITECH
            bcc @normal         ; Correct if page change
            inc SPRITECH+1
@normal:    jsr CalcChGenOfs
            ldy #0              ; Copy 8 bytes
@loop1:     lda #0
            sta CharShr
            lda (CHRPTR),y      ; Charge source
            ldx SpriteX
            beq @noshift
@loop2:     lsr                 ; At the beginning of the cycle, x contains
            ror CharShr         ; the shift in pixels to the right
            dex
            bne @loop2
@noshift:   sta (SPRITECH),y    ; Save
            tya
            pha
            adc #16
            tay
            lda CharShr
            sta (SPRITECH),y    ; Save
            pla
            tay
            iny
            cpy #08
            bne @loop1
            rts

; Load CHRPTR and CHRPTR+1 with the address of the character generator area
; corresponding to the character contained in CharCode

CalcChGenOfs:
            lda #<GRCHARS1      ; Charge the address of the ch. gen in CHARPTR
            sta CHRPTR
            lda #>GRCHARS1
            sta CHRPTR+1
            lda CharCode        ; Charge the ch. code to be used
            asl                 ; Multiply it times 8
            asl
            asl
            adc CHRPTR          ; Add to the CHRPTR (to get address of the ch.)
            sta CHRPTR
            bcc @normal         ; Correct if page change
            inc CHRPTR+1
@normal:    rts

; Clear the contents of a "sprite".
ClearSprite:
            lda #0
            ldy #32
@loop:      dey
            sta (SPRITECH),y    ; sta does not affect processor flags
            bne @loop
            rts

; A basic test showing a "sprite"
; testsprite: lda #<(GRCHARS1+(LASTCH+1)*8)
;             sta SPRITECH
;             lda #>(GRCHARS1+(LASTCH+1)*8)
;             sta SPRITECH+1
;             jsr ClearSprite
;
;             lda SpriteX
;             tax
;             lda AlienPosY
;             and #7
;             tay
;             ;ldx #7
;             lda #TENTACLE3
;             jsr LoadSprite
;
;             lda #MAGENTA
;             sta Colour
;
;             lda #(LASTCH+1)
;             ldx #8
;             ldy #15
;             jsr DrawChar
;
;             lda #(LASTCH+2)
;             ldx #8
;             ldy #16
;             jsr DrawChar
;
;             lda #(LASTCH+3)
;             ldx #9
;             ldy #15
;             jsr DrawChar
;
;             lda #(LASTCH+4)
;             ldx #9
;             ldy #16
;             jsr DrawChar
;             rts

; Print a string (null terminated) whose address is contained in LAB_01 and
; LAB_02 at the position given by X and Y pointers

PrintStr:   sty PosY
            ldy #$00
@loop:      lda (LAB_01),Y
            beq @exit
            sty tmp4
            ldy PosY
            jsr DrawChar
            ldy tmp4
            iny
            inx
            jmp @loop
@exit:      ldy PosY
            rts

; Clear the screen. This maybe is too slow to be used in an interrupt handler.
; Draw everywhere the character contained in the A register. Employs X.

CLS:
            size=16*31/4+1
            ldx #size
@loop:      sta MEMSCR-1,X          ; A (small) degree of loop unrolling avoids
            sta MEMSCR+size-1,X     ; to mess with a 16-bit loop.
            sta MEMSCR+size*2-1,X
            sta MEMSCR+size*3-1,X
            dex
            bne @loop
            rts

; Put the colour code contained in A everywhere in the screen

PaintColour:
            ldx #size
@loop:      sta MEMCLR-1,X
            sta MEMCLR+size-1,X
            sta MEMCLR+size*2-1,X
            sta MEMCLR+size*3-1,X
            dex
            bne @loop
            rts

; A simple delay

Delay:      ldx #$FF
            ldy #$FF
Delayloop:  dex
            bne Delayloop
            dey
            bne Delayloop
            rts

ShortDelay: ldy #$40
            ldx #$FF
            jmp Delayloop

; Random number generation routine. Adapted from here:
; http://sleepingelephant.com/ipw-web/bulletin/bb/viewtopic.php?t=2304
; Creates a pseudo-random number in Random and Random+1
; Change register A and employs tmpindex

GetRand:    lda Random+1
            sta tmpindex
            lda Random
            asl
            rol tmpindex
            asl
            rol tmpindex
            clc
            adc Random
            pha
            lda tmpindex
            adc Random+1
            sta Random+1
            pla
            clc             ; added this instruction - kweepa
            adc #$11
            sta Random
            lda Random+1
            adc #$36
            sta Random+1
            rts

; Convert a 16-bit word to a 24-bit BCD. Adapted from here:
; http://www.obelisk.me.uk/6502/algorithms.html
; I like how it is compact and the clever use of the BCD mode of the 6502

; Convert an 16 bit binary value into a 24bit BCD value
Bin2BCD:    lda #0          ; Clear the result area
            sta Res+0
            sta Res+1
            sta Res+2
            ldx #16         ; Setup the bit counter
            sed             ; Enter decimal mode
@loop:      asl Val+0       ; Shift a bit out of the binary
            rol Val+1       ; ... value
            lda Res+0       ; And add it into the result, doubling
            adc Res+0       ; ... it at the same time
            sta Res+0
            lda Res+1
            adc Res+1
            sta Res+1
            lda Res+2
            adc Res+2
            sta Res+2
            dex             ; More bits to process?
            bne @loop
            cld             ; Leave decimal mode
            rts

; Print the BCD value in A as two ASCII digits

PrintBCD:   pha             ; Save the BCD value
            lsr             ; Shift the four most significant bits
            lsr             ; ... into the four least significant
            lsr
            lsr
            clc
            adc #(48+$80)   ; Make a screen code char
            jsr DrawChar
            inx
            pla             ; Recover the BCD value
            and #$0F        ; Mask out all but the bottom 4 bits
            clc
            adc #(48+$80)   ; Make an screen code char
            jsr DrawChar
            inx
            rts

; DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA
;
; Data and configuration settings. Tables and some music-related stuff.
; Characters!
;
; DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA

;BombSpeed:  .res NMBOMBS, $00   ; Array containing the speed of the bombs sent

; Music data. Much is loop-based, to reduce mem occupation.
; The code for a loop is as follows:
; 1 byte: 10xx xxxx where the xxx xxx represent the number of times the loop
;                   should be repeated
; Special code: 1111 1111 repeat from start
; The code for a note is as follows:
;         01 zz zzzz where zz zzzz represents the note (standard VIC 20)
;         01 11 1111 is a silence
; Special codes for note durations:
;         00 ss ssss specify that the following notes should have the given
;            duration in 1/50's of seconds
;         it should be followed by a byte giving the duration of the silence in
;         the note
loopcode = %10000000
notecode = %01000000
silence  = %01111111
duracode = %00000000
endloop  = %11000000
repeatm  = %11111111
maskcode = %11000000
unmask   = %00111111



Voice1data: .byte duracode + 30, 25
            .byte loopcode + 2
            ; a simple scale
            .byte notecode + 0, notecode + 2, notecode + 4, notecode + 5
            .byte notecode + 7, notecode + 9, notecode + 11, notecode + 12
            .byte endloop

            .byte loopcode + 2
            ; a simple scale
            .byte notecode + 24, notecode + 26, notecode + 28, notecode + 29
            .byte notecode + 31, notecode + 33, notecode + 35, notecode + 36
            .byte endloop

            .byte repeatm

Voice2data: .byte duracode + 15, 12
            .byte loopcode + 8
            ; a simple scale
            .byte notecode + 40, silence
            .byte notecode + 48, silence
            .byte endloop

            .byte loopcode + 16
            ; a simple scale
            .byte notecode + 32, silence
            .byte endloop

            .byte repeatm

YouWonSt:   .byte (25+$80), (15+$80), (21+$80), (32+$80), (23+$80), (15+$80)
            .byte (14+$80), 0

GameOverSt: .byte (7+$80), (1+$80), (13+$80), (5+$80), (32+$80), (15+$80)
            .byte (22+$80), (5+$80), (18+$80), 0

DefChars:
            TENTACLE1 = 0
            .byte %00000000     ; Alien #1, associated to ch. 0 (normally @)
            .byte %00000001
            .byte %11000111
            .byte %11101110
            .byte %00111100
            .byte %00111000
            .byte %00000000
            .byte %00000000

            GEARCHEST = 1
            .byte %00101000     ; Alien #2, associated to ch. 1 (normally A)
            .byte %01111100
            .byte %01111100
            .byte %00101000
            .byte %11111111
            .byte %11111111
            .byte %11111111
            .byte %11111111

            OCTO = 2
            .byte %11001111     ; Alien #3, associated to ch. 2 (normally B)
            .byte %11001111
            .byte %01100111
            .byte %00110000
            .byte %00011000
            .byte %00001100
            .byte %00000111
            .byte %00000000

            OCTO2 = 3
            .byte %00000000     ; Alien #4, associated to ch. 3 (normally C)
            .byte %00000111
            .byte %00001100
            .byte %00011000
            .byte %00110000
            .byte %01100111
            .byte %11001111
            .byte %11001111

            PLAYER = 4
            .byte %00110011     ; Mother ship 1
            .byte %00110110
            .byte %00111100
            .byte %01111000
            .byte %00110000
            .byte %01001000
            .byte %01001000
            .byte %01101100

            HEAD = 5
            .byte %00111100     ; Alien #2, associated to ch. 1 (normally A)
            .byte %01111110
            .byte %11011011
            .byte %11111111
            .byte %01100110
            .byte %00111100
            .byte %01100110
            .byte %00000000

            RARM = 6
            .byte %11100000     ; Alien #1, associated to ch. 0 (normally @)
            .byte %01100000
            .byte %01100000
            .byte %01100000
            .byte %01100000
            .byte %01100110
            .byte %00111100
            .byte %00011000

            LARM = 7
            .byte %00000111     ; Alien #1, associated to ch. 0 (normally @)
            .byte %00000110
            .byte %00000110
            .byte %00000110
            .byte %00000110
            .byte %01100110
            .byte %00111100
            .byte %00011000

            TORSO = 8
            .byte %11111111     ; Alien #1, associated to ch. 0 (normally @)
            .byte %11100111
            .byte %01100110
            .byte %01100110
            .byte %01100110
            .byte %01100110
            .byte %00111100
            .byte %00011000

            EMPTY = 9
            .byte %00000000     ; Blank char, ch. 5 (E)
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000

            LLEG = 10
            .byte %00000111     ; Bomb, associated to ch. 6 (normally F)
            .byte %00000110
            .byte %00000110
            .byte %00000110
            .byte %00000110
            .byte %00000110
            .byte %00011110
            .byte %00111110

            SHOTMSK = %00010000 ; Mask for detecting a collision

            RLEG = 11
            .byte %11100000     ; Bomb, associated to ch. 6 (normally F)
            .byte %01100000
            .byte %01100000
            .byte %01100000
            .byte %01100000
            .byte %01100000
            .byte %01110000
            .byte %01111000

            GEAR=12
            .byte %10011001     ; Block, ch. 11 (normally M)
            .byte %01011010
            .byte %00111100
            .byte %11111111
            .byte %00111100
            .byte %11111111
            .byte %01011010
            .byte %10011001

            LASTCH = GEAR
            SPRITE1A = LASTCH+1
            SPRITE1B = LASTCH+2
            SPRITE1C = LASTCH+3
            SPRITE1D = LASTCH+4

            ; The sprites characters should be continuous.
            ; This simplifies collision detection.

            SPRITE2A = LASTCH+5
            SPRITE2B = LASTCH+6
            SPRITE2C = LASTCH+7
            SPRITE2D = LASTCH+8

            BLENDCH = LASTCH+9
            ;Level 0 1 2 3 4 5 6 7 8 9 A B
LevelsPer:  .byte  5,4,3,3,2,2,2,2,1,1,1,1
;LevelsBomb: .byte  3,2,3,2,2,3,2,1,4,3,2,1

NUMLEVEL   = 12 ; Total number of levels.

EndMemMrk:  .byte 0

            
