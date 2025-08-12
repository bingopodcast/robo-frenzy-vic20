;   		      V I C   R O B O - F R E N Z Y 
;
;             originally by Davide Bucci, April-June 2018
;             modified by Nicholas Baldridge for For Amusement Only Games, LLC.
;
; This program is a version of the new EM arcade game "Robo-Frenzy"
; that runs on an unexpanded VIC-20.  See the following URL for more info:
; https://pinside.com/pinball/forum/topic/new-em-arcade-game
;
; The assembler used is ca65. 

.segment "HEADER"
    .addr _main                   ; 2 bytes: Cold Start vector for SYS command
    .addr GameStart               ; 2 bytes: Warm Start vector
	.byte $30, $41, $C3, $C2, $CD ; A0CBM Signature
    
; Plenty of things are done during the IRQ handling routine, synchronized with
; the PAL refresh rate of the monitor. For this reason, there may be some flicker
; of the tentacles when this game is played on a NTSC machine. Differences between
; PAL and NTSC include a different screen height that is 31 rows for PAL
; machines and 28 for NTSC ones and a couple of audio-related tweaks.

; Difficulty-related constants
    PERIODS = 5         ; Initial difficulty (less=faster)
    MAX_TENTACLE_LENGTH = 8
 
; General-use addresses
    GRCHARS1 = $1C00    ; Address of user-defined characters. Since in the
                        ; unexpanded VIC the screen matrix starts at
                        ; $1E00, there are 512 bytes free, i.e. 64 chars
                        ; that can be defined.

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
    ORANGE = $0A
    LIGHT_BLUE = $0B
    BROWN = $09

; KERNAL routines used
    GETIN = $FFE4

; Page-0 addresses used (for indirect indexed addressing and other things)
    ; Temporary variables in zero page for fastest access
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
    CharCode = LAB_03   ; Used in DrawChar
    PosX = LAB_04
    PosY = LAB_05
    Colour = LAB_06     ; Colour to be used by the printing routines
        
    ; Dedicated temporaries for DrawTentacles and CheckTentacleCollision *only*.
    ; The risk of another routine stomping on these variables is too great and
    ; in fact, WILL happen if these variables are eliminated.  The similar tmp
    ; var names (to help understand usage) are included here. Drawing tentacles
    ; needs to happen very quickly within the IRQ handler to provide the
    ; player with an appropriate representation of the tentacle length.
    DT_tmpindex = $90  ; tmpindex
    DT_tmpx     = $91  ; tmpx
    DT_tmpy     = $92  ; tmpy
    DT_tmp4     = $93  ; tmp4
    DT_tmp5     = $94  ; tmp5
    DT_LAB_01   = $95  ; LAB_01
    DT_LAB_02   = $96  ; LAB_02
    DT_TentaclePtr = $97 ; TentaclePtr
    
    ; Dedicated temporaries for the collision check routine.
    CTC_tmpx     = $9A
    CTC_tmpy     = $9B

    BCD_Val      = $A2 
    BCD_Val_Hi   = $A3 
    BCD_Res      = $A4 
    BCD_Res_1    = $A5 
    BCD_Res_2    = $A6 
        
    tmpindex     = LAB_07 
    tmpx         = LAB_08
    tmpy         = LAB_09
    tmp4         = LAB_0A
    CHRPTR       = $0E     ; Pointer to the original ch. in a sprite (word)
    POSCHARPT    = $1A     ; Pointer for a character in memory (word)
    POSCOLPT     = $1C     ; Pointer for a colour in memory (word)
    TmpScan      = $20     ; Used in raster line sync code
    Random       = $21     ; Position where to store a random word by GetRand
    IrqCn        = $23     ; Counter for interrupt
    keyin        = $24     ; Last key typed.
    Joystick     = $2A     ; Different from zero if the joystick was used
    Period       = $2C     ; Higher = slower tentacle movement
    CannonPos    = $3F     ; Horizontal position of the cannon (in characters)
    OldCannonP   = $40     ; Old position of the cannon (X)
    Win          = $41     ; If 1, the level is won. If $FF, game over
    Score        = $42     ; Current score (divided by 10) (word)
    HiScore      = $44     ; High Score (divided by 10) (word)
    Level        = $47     ; Current level
    CannonYPos   = $4B
    BunkerY      = $4D
    MusicMuted   = $4E     ; Flag to mute music
    Voice1Ptr    = $4F     ; Music data pointer for voice 1
    Voice1Ctr    = $50     ; Note duration counter for voice 1
    Voice2Ptr    = $55     ; Music data pointer for voice 2
    Voice2Ctr    = $56     ; Note duration counter for voice 2
    GameOverSfxCtr = $57   ; Counter for the game over sound
    SfxTimer 	 = $58
    AccentColour = $5A

    OldCannonY	 = $65 ; Old position of the cannon (Y)
    GearPos      = $66
    GearPosY     = $68
    RobotNum     = $69
    GearHeld     = $6A ; Tracking whether the gear is currently held.
    GearPresent  = $6B ; Tracking if the gear is on the "Bunker"
    IsNTSC       = $6C

    ; Variables to set the upper/lower bounds of Y movement for player.
    PlayerY_TopLimit = $6D
    PlayerY_BottomLimit = $6E

    ; These variables are used to track the tentacle and collision states,
    ; as well as set up the delays in pickup and find the correct tentacle
    ; to move.  Last tentacle is tracked to ensure randomizer doesn't
    ; move the same tentacle back-to-back.
    TentacleData 			  = $70
    ActiveTentacleID          = $FD
    PlayerHitByTentacle       = $85
    PickupLockoutTimer        = $86
    TentacleSelectionCounter  = $87
    TentacleCollisionDelayCounter = $88
    TentacleMoveSpeedDivisor  = $89
    TentacleSelectPeriod      = $8A
    LastTentacleID            = $8B
    Tentaclecntr      		  = $F0 ; For loop counting while drawing.
    DT_LOOP_COUNTER   		  = $F1 ; DrawTentacles requires this to ensure proper animation.
    PlayerCharToDraw  		  = $F2 ; Flashing player character to draw.
        
    SecondsLeft       		  = $F3 ; Countdown timer
    FrameCounter      		  = $F4 ; Counts IRQs from 50 down to 0 to track seconds
    BottomRow         		  = $F5

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

    PORTAVIA1  = $9111  ; Port A 6522 (joystick)
    PORTAVIA1d = $9113  ; Port A 6522 (joystick)
    PORTBVIA2  = $9120  ; Port B 6522 2 value (joystick)
    PORTBVIA2d = $9122  ; Port B 6522 2 direction (joystick

    MEMSCR = $1E00    ; Start address of the screen memory (unexp. VIC)
    MEMCLR = $9600    ; Start address of the colour memory (unexp. VIC)

    REPEATKE = $028A    ; Repeat all keys

    VOICE1  = GEN1      ; Voice 1 for music
    VOICE2  = GEN2      ; Voice 2 for music
    EFFECTS = GEN3      ; Sound effects (not noise)

    TENTACLE_STATE_RETRACTED = 0 ; Inactive and fully retracted
    TENTACLE_SEGMENT_CHAR    = 0
    NUM_TENTACLES            = 10

.export _main
.segment "STARTUP"
.segment "LOWCODE"
.segment "INIT"
.segment "GRCHARS"
.segment "CODE"

; This was a clever idea that didn't work, but it made for a memorable 
; memory address to start the game.  SYS 40999.
_main:
    ldx #0

loop:
    lda command_string,x
    beq done_copy
    sta $0277,x         ; Manually poke character into the keyboard buffer, rpt.
    inx
    jmp loop

done_copy:
    stx $C6             ; Set the number of characters in the buffer    
    jmp $E3BF           ; Jump to the KERNAL's warm start vector to run the command

command_string:
    .byte "SYS 40999", $0D, 0

; Main program routines.
GameStart:
    jsr Init        ; The Init routine for the game itself
    jsr StartGame   ; The StartGame routine
	
restart:    
    jsr StartGame   ; Set the starting values of game variables

mainloop:   
    lda Joystick
    beq @ccc
    jsr ShortDelay
    lda #$00
    sta Joystick
@ccc:       
    jsr GETIN       ; Main loop waiting for keyboard events
    sta keyin
    lda #$FF
    sta Joystick
    lda PORTAVIA1
    and #%00010000  ; Left
	bne @check_right
    jsr left
	jmp mainloop
@check_right: 
    lda PORTBVIA2   ; The Vic uses a second VIA for one direction
                    ; on the joystick.
    and #%10000000  ; Right
    bne @check_fire
    jsr right
	jmp mainloop
@check_fire: 
    lda PORTAVIA1
    and #%00100000  ; Fire
	bne @no_joystick_input
    jsr fire
	jmp mainloop
@no_joystick_input: 
    lda #$00
    sta Joystick
    lda keyin
    beq mainloop
    cmp #$0D        ; Wait for return if the game stopped
    bne @norestart
    lda Win
    ;cmp #1
    bne restart
@norestart: 
    lda keyin
    cmp #$58        ; X: increase position of the player (aka Cannon - right)
    beq @call_right_key
    cmp #$5A        ; Z: decrease position of the player (aka Cannon - left)
    beq @call_left_key
    cmp #$4D        ; M toggle music on/off
    bne mainloop
    lda MusicMuted
    eor #1
    sta MusicMuted
@continue4: 
    jmp mainloop
@call_right_key: 
    jsr right
	jmp mainloop
@call_left_key: 
    jsr left
	jmp mainloop
            
fire:       
    lda Win         ; If the game has stopped, restart
    bne restart
    rts
            
right:
    lda PlayerHitByTentacle
    bne @continue_right

    ; First, check if we are already at the rightmost boundary.
    lda CannonPos
    cmp #8              ; Is player at the right edge (X=8)?
    bcs @checkY_right   ; If so, skip the horizontal move and just handle vertical.

    ; Keep on movin'.  Not at the edge.
    jsr ClearCannon
    inc CannonPos
@checkY_right:
    inc CannonYPos
    lda CannonYPos
    cmp PlayerY_BottomLimit
    bcc @check_for_gear
@cap_y_right:
    lda PlayerY_BottomLimit
    sta CannonYPos
@check_for_gear:
    ; Should a gear be spawned at the GEARCHEST?
    lda CannonYPos
    cmp PlayerY_BottomLimit
    bne @continue_right
    lda GearHeld
    bne @continue_right
    lda GearPresent
    bne @continue_right
    jsr DrawGear
    lda #1
    sta GearPresent
@continue_right:
    rts

CheckGearCollection:
    lda PlayerHitByTentacle     ; Is player stunned?
    bne @exit_collection        ; If so, cannot pick up gear.
    
	lda ProcessingGearAward
	cmp #1
	beq @exit_collection
	lda GearPresent
	cmp #1
    bne @exit_collection
    lda CannonPos
    cmp GearPos
    bne @exit_collection
    lda CannonYPos
    cmp GearPosY
    bne @exit_collection
    jsr ClearGear
    jsr Sfx_GearPickup
    lda #1
    sta GearHeld
    lda #0
    sta GearPresent
	lda #$FF
	sta OldCannonP
	lda #$FF
	sta OldCannonY
@exit_collection:
	rts

left:
    lda PlayerHitByTentacle
    bne @continue_left

    lda CannonPos
    bne @dec_position
    jmp @checkY_left        ; Already at X=0, so just do the Y move
@dec_position:
    dec CannonPos
@checkY_left:
    dec CannonYPos
    lda CannonYPos
    cmp PlayerY_TopLimit
    bcs @continue_left
    lda PlayerY_TopLimit
    sta CannonYPos
@continue_left:
    rts

; Initialization code: prepare the screen to the correct size, center it and
; load the graphic chars and configure the IRQ handler.
Init:
    sei
    ; This routine checks for a high raster line that only exists on PAL.
    ; A long timeout ensures NTSC machines are detected correctly.
    ldx #$40        ; Set a long timeout outer loop

DetectLoop:
    ldy #$FF        ; Set a long timeout inner loop

InnerDetectLoop:
    lda VICRAST     ; Read the high 8 bits of the raster line counter.
    cmp #140        ; Check for line 280+ (140 * 2), which is only present on PAL.
    bcs IsPAL       ; If found, it's a PAL machine, carry on with PAL routines.
    dey
    bne InnerDetectLoop
    dex
    bne DetectLoop

; NTSC Path (Timeout Occurred)
InitNTSC_Path:
    lda #1
    sta IsNTSC

; Sync with raster
ntsc_sync_loop:
    lda VICRAST
    cmp #70
    bne ntsc_sync_loop
    ; Set NTSC timer values
    lda #<(261 * 65 - 2)
    ldx #>(261 * 65 - 2)
    stx $9125
    sta $9126
    ; Set screen parameters for NTSC
    jsr CenterScreenNTSC
    jmp ContinueInit

; PAL Path (High Raster Line Detected)
IsPAL:
    lda #0
    sta IsNTSC

; Sync with raster
pal_sync_loop:
    lda VICRAST
    cmp #112
    bne pal_sync_loop
    ; Set PAL timer values
    lda #<(312 * 71 - 2)
    ldx #>(312 * 71 - 2)
    stx $9125
    sta $9126
    ; Set screen parameters for PAL
    jsr CenterScreenPAL

ContinueInit:
    sty VICSCRVE            ; Apply the vertical centering value
    stx VICSCRHO            ; Apply the horizontal centering value

    lda #$08                ; Set screen and border to black
    sta VICCOLOR
    lda #$90                ; Set a 16 column-wide screen
    sta VICCOLNC
    lda #$FF                ; Set character generator address
    sta VICCHGEN
    
	; Add specially defined characters
    jsr MovCh
    
    ; Initialize game variables and peripherals
    lda #<IrqHandler
    sta $0314
    lda #>IrqHandler
    sta $0315
    cli
    
    jsr ZeroScore
    sta Level
    sta HiScore
    sta HiScore+1
    sta PORTAVIA1d
    lda #$7F
    sta PORTBVIA2d
    jsr InitTentacles
    rts

; Screen init value for PAL and NTSC
CenterScreenPAL:
    lda #16
    sta CannonYPos
    lda #16
    sta PlayerY_TopLimit
    lda #24
    sta PlayerY_BottomLimit
    lda #$3E        ; Set a 31 row-high column
    sta VICROWNC
    lda #25
    sta BunkerY
    lda #28
    sta BottomRow
    ldx #$12
    ldy #$16
    lda #MAGENTA
    sta AccentColour
    rts

CenterScreenNTSC:
    lda #13
    sta CannonYPos
    lda #13
    sta PlayerY_TopLimit
    lda #21
    sta PlayerY_BottomLimit
    lda #$36        ; Set a 27 row-high column
    sta VICROWNC
    lda #22
    sta BunkerY
    lda #25
    sta BottomRow
    ldx #$0A
    ldy #$10
    lda #ORANGE
    sta AccentColour
    rts

DrawHeader:
    lda #GREEN
    sta Colour
    lda #<CurrentStr
    sta LAB_01
    lda #>CurrentStr
    sta LAB_02
    ldx #1
    ldy #0
    jsr PrintStr

    lda #<HighStr
    sta LAB_01
    lda #>HighStr
    sta LAB_02
    ldx #9
    ldy #0
    jsr PrintStr
    rts

DrawLevelDisplay:
    ; Set color once for all text elements in this routine
    lda #WHITE
    sta Colour

    ; Print "ROBOT:"
    lda #<LevelStr
    sta LAB_01
    lda #>LevelStr
    sta LAB_02
    ldx #4
    ldy BunkerY
    iny
    iny
    iny
    jsr PrintStr

    ; Print "TIME:"
    lda #<TimeStr
    sta LAB_01
    lda #>TimeStr
    sta LAB_02
    ldx #4
    iny
    jsr PrintStr
    
    ; Print the current level number
    dey
    ldx #11
    lda Level
    clc
    adc #1
    ora #$30
    jsr DrawChar
    rts

DrawTimerDisplay:
    lda BunkerY     ; Load the system-specific Y position of the bunker.
    clc
    adc #4          ; Add 4 to get the same line as "TIME:".
    tay             ; Transfer to Y.
    lda #3
    sta TempLoopY   ; Store loop counter (3) in a temp var.
    ldx #8          ; Set initial X position for clearing - 1

@ClearLoop:
    inx             ; X becomes 9, 10, 11
    lda #EMPTY
    jsr DrawChar    ; Draws at (X, 29) because Y is correctly set and not changed

    dec TempLoopY   ; Decrement counter in memory
    lda TempLoopY
    bne @ClearLoop  ; Loop if not zero

    ; Convert the current time to a number the screen can display
    ; The VIC needs BCD conversion.
    lda SecondsLeft
    sta BCD_Val
    lda #0
    sta BCD_Val_Hi
    jsr Bin2BCD

    ; And print it.
    ldx #9
    lda #YELLOW
    sta Colour
    jsr PrintTimer
    rts

PrintTimer:
    lda BCD_Res_1
    and #$0F
    clc
    adc #(48+$80)
    jsr DrawChar
    
    inx
    lda BCD_Res
    jmp PrintBCD

StartGame:
    sei
    jsr InitMusic
    lda #$2F             ; Turn on the volume
    sta VOLUME
    lda Level
    bne @skip_timer_reset
    lda #120             ; It is Level 0, so reset timer to default,
    sta SecondsLeft
    lda #50              ; and reset the frame counter for the first second.
    sta FrameCounter
@skip_timer_reset:
    lda #$FF
    sta OldCannonP
	lda PlayerY_TopLimit
    sta CannonYPos
    lda #0
    sta CannonPos        ; Initial position of the player
    sta GearHeld
    sta GearPresent
    sta RobotNum
    sta ProcessingGearAward
    sta Win
    sta IrqCn
    sta NOISE
    sta tmpindex
    sta tmpy
    jsr InitTentacles
    sta ActiveTentacleID
    sta Tentaclecntr
    sta PlayerHitByTentacle 
    jsr ConfLevel
    lda #EMPTY
    jsr CLS              ; Clear the entire screen
    jsr DrawLevelDisplay
    jsr DrawTimerDisplay
    jsr DrawHeader
    jsr DrawShield
    jsr SpawnGearAtBunker
    lda #1
    sta GearPresent
    jsr DrawGear         ; Draw initial gear
    lda HiScore+1        ; Check the high score bytes to determine
    ora HiScore          ; if default high score should be set.
    bne @skip_hiscore_set
    
    lda #$FA             ; Default score value, low byte.
    sta HiScore
    lda #$00             ; Default score value, high byte.
    sta HiScore+1
@skip_hiscore_set:
    jsr draw1l
    cli                  ; IRQs re-enabled.
    rts
    
; Put zero in the current score
ZeroScore:  
    lda #$00
    sta Score
    sta Score+1
    rts

; Configure the level (in Level)
ConfLevel:  
    ldx Level
    cpx #NUMLEVEL-1
    bmi @validlevel
    ldx #NUMLEVEL-1
    stx Level
@validlevel:
    lda LevelsPer,x
    sta Period
    rts

; This routine sets some basic sprite placement.
; Things like the octopus body, and gear chest (bunker)
DrawShield:
    lda AccentColour
    sta Colour

    ; Determine the base X coordinate (8 for PAL, 9 for NTSC)
    ldx #8
    lda IsNTSC
    beq @x_coord_set
    ldx #9
@x_coord_set:
    stx tmpx                ; Store base X in a temporary variable

    ; Draw the bunker/chest at 8
	ldx #8
    ldy BunkerY
    lda #1
    jsr DrawChar

    ; Draw top 2 Octo shapes
    ; Determine Y coordinate (17 for PAL, 14 for NTSC)
    ldy #17
    lda IsNTSC
    beq @top_y_set
    ldy #14
@top_y_set:
    ldx tmpx                ; Load base X once for this row
    lda #OCTO2
    jsr DrawChar            ; Draw left part at base X
    inx
    lda #OCTO4
    jsr DrawChar            ; Draw right part at base X + 1

    ; Draw bottom 2 Octo shapes
    ; Determine Y coordinate (18 for PAL, 15 for NTSC)
    ldy #18
    lda IsNTSC
    beq @bottom_y_set
    ldy #15
@bottom_y_set:
    ldx tmpx                ; Load base X once for this row
    lda #OCTO
    jsr DrawChar            ; Draw left part at base X
    inx
    lda #OCTO3
    jsr DrawChar            ; Draw right part at base X + 1
    rts

draw1l:
    lda Score       ; Load the current score and convert it to BCD.
    sta BCD_Val
    lda Score+1
    sta BCD_Val_Hi
    jsr Bin2BCD
    lda #WHITE
    sta Colour
    ldx #0
	ldy #1
	sty PosY
    jsr PrintRes
    lda #(48+$80)   ; Write a zero, to multiply the score x10.
    jsr DrawChar
    lda Score+1
    cmp HiScore+1
    bcc @noupdate
    beq @nou
    jsr UpdateHiSc
@nou:       
    lda Score       ; Update the high score if needed
    cmp HiScore
    bcc @noupdate
    jsr UpdateHiSc
@noupdate:  
    lda HiScore     ; Load the current hi score and convert it to BCD
    sta BCD_Val
    lda HiScore+1
    sta BCD_Val_Hi
    jsr Bin2BCD
    ldx #09
    ldy #1
    lda #CYAN
    sta Colour
    jsr PrintRes
    lda #(48+$80)   ; Write an additional zero
    jsr DrawChar
    rts

UpdateHiSc: 
    lda Score       ; Update the high score
    sta HiScore
    lda Score+1
    sta HiScore+1
    rts

PrintRes:   
    sta Colour
    lda BCD_Res_2   ; Print all the BCD chars
    jsr PrintBCD
    lda BCD_Res_1
    jsr PrintBCD
    lda BCD_Res
    jsr PrintBCD
    rts

; Copy the graphic chars. They are subject to be changed during the pixel-by
; pixel movement, so that routine moves only the characters not used as sprites
MovCh:
    ; Copy the first 1KB of the standard character set from ROM to RAM
    lda #<$8000
    sta POSCHARPT
    lda #>$8000
    sta POSCHARPT+1
    lda #<GRCHARS1
    sta CHRPTR
    lda #>GRCHARS1
    sta CHRPTR+1
    ldx #4
@copy_page_loop:
    ldy #0
@copy_byte_loop:
    lda (POSCHARPT),y
    sta (CHRPTR),y
    iny
    bne @copy_byte_loop
    inc POSCHARPT+1
    inc CHRPTR+1
    dex
    bne @copy_page_loop
    ; Overwrite specific slots with our custom graphics
    ; Needed to avoid overwriting used parts of charset.
    ldy #0
@overwrite_loop:
    lda DefChars+(0*8),y
    sta GRCHARS1+(0*8),y    ; TENTACLE_SEGMENT_CHAR
    lda DefChars+(1*8),y
    sta GRCHARS1+(1*8),y    ; GEARCHEST
    lda DefChars+(2*8),y
    sta GRCHARS1+(31*8),y   ; OCTO
    lda DefChars+(4*8),y
    sta GRCHARS1+(4*8),y    ; PLAYER
    lda DefChars+(6*8),y
    sta GRCHARS1+(6*8),y    ; RARM
    lda DefChars+(10*8),y
    sta GRCHARS1+(10*8),y   ; LLEG
    lda DefChars+(11*8),y
    sta GRCHARS1+(11*8),y   ; RLEG
    lda DefChars+(12*8),y
    sta GRCHARS1+(12*8),y   ; GEAR

    ; Remapped/new characters
    lda DefChars+(3*8),y
    sta GRCHARS1+(16*8),y   ; OCTO2 -> 16
    lda DefChars+(5*8),y
    sta GRCHARS1+(17*8),y   ; HEAD -> 17
    lda DefChars+(7*8),y
    sta GRCHARS1+(26*8),y   ; LARM -> 20
    lda DefChars+(8*8),y
    sta GRCHARS1+(22*8),y   ; TORSO -> 22
    lda DefChars+(9*8),y
    sta GRCHARS1+(25*8),y   ; EMPTY -> 25
    lda DefChars+(13*8),y
    sta GRCHARS1+(27*8),y   ; OCTO3 -> 27
    lda DefChars+(14*8),y
    sta GRCHARS1+(28*8),y   ; OCTO4 -> 28
    lda DefChars+(15*8),y
    sta GRCHARS1+(29*8),y   ; TENTACLE2 -> 29
    lda DefChars+(16*8),y
    sta GRCHARS1+(30*8),y   ; TENTACLE3 -> 30
    iny
    cpy #8                  ; Loop until all 8 bytes for each character are copied
    bne @overwrite_loop
    rts
    
; IRQ
; This is the interrupt handler, called 50 times each second when the VIC-20
; is a PAL unit or 60 when NTSC. It does as little as possible
; to maintain a very high frame rate and handle input and UI very rapidly.
IrqHandler:
    pha               ; Save all registers
    txa
    pha
    tya
    pha

    ; Check the game's state and call the appropriate subroutine.
    lda Win
    bne @check_if_won
    jsr Irq_HandleActiveGame
    jmp @irq_done
@check_if_won:
    cmp #$FF
    bne @level_is_won
    jsr Irq_HandleGameOver
    jmp @irq_done
@level_is_won:
@irq_done:
    pla                 ; Restore all registers
    tay
    pla
    tax
    pla
    jmp $EABF           ; Jump to the original KERNAL IRQ handler

; Active Gameplay IRQ
; Contains all logic that runs every frame when the game is being played.
Irq_HandleActiveGame:
    ; Sound Engine setup.
    lda SfxTimer
    beq @play_music
    dec SfxTimer
    bne @play_music
    lda #0
    sta VOICE1
    sta VOICE2
    sta NOISE
@play_music:
    ; Game clock, gear pickup, tentacle movement, music overrides
    inc IrqCn
    lda IrqCn
    cmp Period
    bne @run_per_frame_logic    ; Logic that runs only on a game "tick"
    lda #0
    sta IrqCn
    jsr MusicUpdate
    jsr MoveTentacles
    jsr CheckGearCollection
    jsr GameLogic
@run_per_frame_logic:
    jsr HandlePlayerStun
    lda Colour
    pha
    jsr DrawTentacles
    pla
    sta Colour
    lda PlayerHitByTentacle
    bne @sprite_has_moved        ; Redraw player sprite only if it has moved 
    lda CannonPos
    cmp OldCannonP
    bne @sprite_has_moved
    lda CannonYPos
    cmp OldCannonY
    bne @sprite_has_moved
    jmp @no_movement_so_no_redraw
@sprite_has_moved:
    jsr ClearCannon
    lda GearHeld
    beq @draw_phase
    jsr ClearGear
@draw_phase:
    jsr DrawCannon
    lda GearHeld
    beq @drawing_done
    lda CannonPos
    clc
    adc #1
    sta GearPos
    lda CannonYPos
    sec
    sbc #1
    sta GearPosY
    jsr DrawGear
@drawing_done:
@no_movement_so_no_redraw:
    ; Countdown timer logic.
    dec FrameCounter
    bne @active_irq_exit
    lda #50
    sta FrameCounter
    lda SecondsLeft
    beq @active_irq_exit
    dec SecondsLeft
    jsr DrawTimerDisplay
    lda SecondsLeft
    bne @active_irq_exit
    jsr GameOver
@active_irq_exit:
    rts

InitTentacles:
    ldx #NUM_TENTACLES - 1
@init_loop:
    ; Set initial state to ready-to-grow
    lda #TENTACLE_STATE_RETRACTED
    sta Tentacle_State,x
    
    ; Set initial length to 0
    lda #0
    sta Tentacle_Length,x
    sta Tentacle_Old_Length,x
    
    ; Assign a path to each tentacle (Tentacle 0 -> Path 0, etc.)
    txa
    sta Tentacle_PathIndex,x
    
    dex
    bpl @init_loop
    rts

; Advances the tentacle growth animation state and displays debug info.
MoveTentacles:
    ldx ActiveTentacleID

    ; Look up the path data for the currently active tentacle.
    lda Tentacle_PathIndex,x
    sta DT_tmp4
    asl a
    clc
    adc DT_tmp4
    tay

    ; Compare current length with the path's max length.
    lda TentaclePathData+1,y
    cmp Tentacle_Length,x
    bne @GrowTentacle       ; If not at max length, branch to the growth logic.

    ; This runs when a tentacle is finished growing
    ; Reset the finished tentacle's length to 0.
    lda #0
    sta Tentacle_Length,x

    ; Store the ID of the tentacle that just finished.
    lda ActiveTentacleID
    sta LastTentacleID
@GenerateNewID:
    jsr GetRand                 ; Get a new random number.
    lda Random                  ; Use the low byte.
    and #%00001111              ; Mask to a value between 0-15.
    cmp #NUM_TENTACLES          ; Is the value 10 or greater?
    bcs @GenerateNewID          ; If so, it's out of range, try again.

    cmp LastTentacleID
    beq @GenerateNewID          ; If it's the same, try again.

    sta ActiveTentacleID
    jmp @ExitMoveTentacles      ; Exit after selecting a new tentacle
@GrowTentacle:
    ; If the tentacle is not at max length, increment its length by one.
    inc Tentacle_Length,x
@ExitMoveTentacles:
    rts

; IN: X = Tentacle Index (0-7), A = Segment Index (0 to length-1)
; OUT: Address of segment data is stored in DT_TentaclePtr
GetSegmentAddress:
    sta DT_tmp5                 ; Temporarily store the segment index
    lda Tentacle_PathIndex,x
    sta DT_tmp4
    asl a
    clc
    adc DT_tmp4
    tay
    lda TentaclePathData,y
    sta DT_tmpy
    lda DT_tmp5
    asl a
    sta DT_LAB_01
    adc DT_tmp5
    sta DT_LAB_02
    clc
    lda DT_LAB_02
    adc DT_tmpy
    sta DT_LAB_01
    lda #<TentacleDrawData
    adc DT_LAB_01
    sta DT_TentaclePtr
    lda #>TentacleDrawData
    adc #0
    sta DT_TentaclePtr+1
    rts

; Adjusts tentacle coordinates stored in DT_tmpx and DT_LAB_02 for NTSC systems.
AdjustTentacleCoordsForNTSC:
    lda IsNTSC
    beq @exit_adjust
    inc DT_tmpx         ; X = X + 1
    dec DT_LAB_02       ; Y = Y - 3
    dec DT_LAB_02
    dec DT_LAB_02
@exit_adjust:
    rts

; This routine clears the previously drawn tentacles and draws the current ones.
DrawTentacles:
    lda Win
    beq @continue_drawing 
    jmp @DT_Loop_Finished
@continue_drawing:
    ldx #NUM_TENTACLES - 1
@DT_Outer_Loop:
    lda VICRAST
    cmp #40     				; Loop while raster line is >= 40 (flicker prevention on NTSC)
    bcs @DT_Outer_Loop
    stx DT_LOOP_COUNTER
    lda Tentacle_Old_Length,x
    jsr ClearTentacle
@DT_Prepare_Draw:
    lda #0
    sta DT_tmp5
    lda Tentacle_Length,x
    sta DT_tmpindex             ; New length
@DT_Draw_Inner_Loop:
    lda DT_tmp5
    cmp DT_tmpindex
    bcs @DT_Update_Old
    lda DT_tmp5                 ; Load Segment Index into A
    ldx DT_LOOP_COUNTER         ; Load Tentacle Index into X
    jsr GetSegmentAddress
    ldy #0
    lda (DT_TentaclePtr),y
    sta DT_tmpx
    iny
    lda (DT_TentaclePtr),y
    sta DT_LAB_02
    iny
    jsr AdjustTentacleCoordsForNTSC 
    lda (DT_TentaclePtr),y
    sta CharCode
    lda AccentColour
    sta Colour
    ldx DT_tmpx
    ldy DT_LAB_02
    lda CharCode
    jsr DrawChar
    inc DT_tmp5
    ldx DT_LOOP_COUNTER
    jmp @DT_Draw_Inner_Loop
@DT_Update_Old:
    lda Tentacle_Length,x
    sta Tentacle_Old_Length,x
    dex
    bmi @DT_Loop_Finished
    jmp @DT_Outer_Loop
@DT_Loop_Finished:
    rts

; Clear a single segment or full tentacle.
ClearTentacle:
    stx DT_LOOP_COUNTER         ; Save Tentacle ID
    sta DT_tmpindex             ; Length to erase
    lda #0
    sta DT_tmp5                 ; Segment loop counter
@ClearLoop:
    lda DT_tmp5
    cmp DT_tmpindex
    bcs @ClearDone
    lda DT_tmp5                 ; Load Segment Index into A
    ldx DT_LOOP_COUNTER         ; Load Tentacle Index into X
    jsr GetSegmentAddress
    ldy #0
    lda (DT_TentaclePtr),y
    sta DT_tmpx
    iny
    lda (DT_TentaclePtr),y
    sta DT_LAB_02
    jsr AdjustTentacleCoordsForNTSC ; Apply the NTSC shift
    ldx DT_tmpx
    ldy DT_LAB_02
    lda #EMPTY
    jsr DrawChar
    inc DT_tmp5
    ldx DT_LOOP_COUNTER
    jmp @ClearLoop
@ClearDone:
    rts

; At end of stage or game over, erase all tentacles to prevent
; odd graphical artifacts.
ClearAllTentacles:
    ldx #NUM_TENTACLES - 1
@CAT_Outer_Loop:
    lda Tentacle_Length,x
    beq @CAT_Next_Tentacle  ; Skip if length is 0
    jsr ClearTentacle     
@CAT_Next_Tentacle:
    dex
    bpl @CAT_Outer_Loop
    rts

; Collision detection for tentacles to the player.  This only
; executes when the player is holding a gear.  Note collision 
; occurs when comparing x,y coords of final tentacle segment in
; the tentacle to the x,y coordinates of the gear (not the player).
CheckTentacleCollision:
    lda GearHeld
    beq @CTC_Exit
    lda PlayerHitByTentacle
    bne @CTC_Exit

    ldx #NUM_TENTACLES - 1
@CTC_Loop:
    ; Check if tentacle is at max length and is a stop path.
    lda Tentacle_PathIndex,x
    sta DT_tmp4
    asl a
    clc
    adc DT_tmp4
    tay
    lda TentaclePathData+1,y
    cmp Tentacle_Length,x
    bne @CTC_Next_Tentacle
    lda TentaclePathData+2,y
    beq @CTC_Next_Tentacle

    ; Dynamically find the tip's coordinates based upon current tentacle.
    lda Tentacle_Length,x
    sec
    sbc #1
    jsr GetSegmentAddress

    ; Get the X,Y coordinates from the calculated address.
    ldy #0
    lda (DT_TentaclePtr),y
    sta CTC_tmpx                 ; Tip X.
    iny
    lda (DT_TentaclePtr),y
    sta CTC_tmpy                 ; Tip Y.

    ; Apply NTSC shift to ensure collision is checked at the proper position.
    lda IsNTSC
    beq @no_ctc_shift
    dec CTC_tmpy
    dec CTC_tmpy
    dec CTC_tmpy
@no_ctc_shift:
    ; Compare BOTH X and Y coordinates with the gear's current position.
    lda CTC_tmpx
    cmp GearPos
    bne @CTC_Next_Tentacle

    lda CTC_tmpy
    cmp GearPosY
    bne @CTC_Next_Tentacle

    ; COLLISION DETECTED!
    jsr ForceDropGear
    jmp @CTC_Exit
@CTC_Next_Tentacle:
    dex
    bpl @CTC_Loop
@CTC_Exit:
    rts

; Ensures player can move after a time.
UpdatePlayerStunStatus:
    lda PlayerHitByTentacle
    beq @exit_stun_update

    dec TentacleCollisionDelayCounter
    bne @exit_stun_update

    lda #0
    sta PlayerHitByTentacle
@exit_stun_update:
    rts

HandlePlayerStun:
    lda PlayerHitByTentacle
    beq @set_normal_player_state

    ; Player is stunned. Decrement the timer and blink sprite.
    dec TentacleCollisionDelayCounter
    lda TentacleCollisionDelayCounter
    bne @flash_logic
    
    ; Stun's over - back to work!
    lda #0
    sta PlayerHitByTentacle 
    lda #$FF                    ; Force a redraw of the player sprite...
    sta OldCannonP              ; ...by setting the old position to an invalid value.
@set_normal_player_state:
    lda #PLAYER
    sta PlayerCharToDraw
    lda #WHITE
    sta Colour
    rts
@flash_logic:
    ; Check if timer is even or odd
    lsr a
    bcc @flash_on_red
    
    ; Odd Frame: Flash OFF (Black/#EMPTY sprite)
    lda #EMPTY
    sta PlayerCharToDraw
    lda #BLACK
    sta Colour
    rts
@flash_on_red:
    ; Even Frame: Flash ON (Red/Player sprite)
    lda #PLAYER
    sta PlayerCharToDraw
    lda #RED
    sta Colour
    rts
    
; Draw the player sprite on the screen, at the current position, 
; contained in CannonPos/CannonYPos.
DrawCannon:
    lda CannonPos
    sta OldCannonP
    tax
    ldy CannonYPos
    sty OldCannonY
    lda PlayerCharToDraw
    jsr DrawChar
    rts

; Draw the gear on the screen, at the position specified in 
; CannonPos (x + 1), CannonPosY - 1
; Lower Y = higher, lower X = further left
DrawGear:
    ldx GearPos
    ldy GearPosY
    lda #YELLOW
    sta Colour
    lda #GEAR
    jmp DrawChar

; Clear the Gear at last remembered position.
ClearGear:
    ldx GearPos
    ldy GearPosY
    lda #WHITE
    sta Colour
    lda #EMPTY
    jmp DrawChar

; Clear the cannon on the screen, at the current position, contained in
; OldCannonP/OldCannonY.
ClearCannon:
    ldx OldCannonP
    ldy OldCannonY
    lda #EMPTY
    jmp DrawChar

GameLogic:
    jsr CheckRobotBuild
    jsr CheckTentacleCollision
    jmp UpdatePlayerStunStatus
	rts

; Drops the gear currently held by the player at their location.
ForceDropGear:
    jsr ClearGear
    jsr ClearCannon

    ; Update flags and move the gear's location to the GEARCHEST (bunker).
    lda #0
    sta GearHeld
    lda #1
    sta GearPresent
    jsr SpawnGearAtBunker
    jsr DrawGear

    ; Immediately redraw the player at their current location.
    lda #RED                    ; Set stun color
    sta Colour
    jsr DrawCannon
    
    ; And set the stun state.
    lda #1
    jsr Sfx_PlayerStun
    sta PlayerHitByTentacle
    lda #60
    sta TentacleCollisionDelayCounter
    rts

CheckRobotBuild:
    ; Is the player at the build location (the far left, X=0)?
    lda CannonPos
    cmp #0
    bne @exit_build_check

    ; Is the player holding a gear?
    lda GearHeld
    beq @exit_build_check
    
    ; Time to build a robot!
    ; We set a flag to prevent gear pickup during the build animation.
    lda #1
    sta ProcessingGearAward
    jsr RoboCompare
    lda #0
    sta ProcessingGearAward
@exit_build_check:
    rts

RoboCompare:
    ldx RobotNum
    cpx #6              ; Check if the robot is ALREADY complete.
    bcs @win_game       ; If so, trigger the win sequence.
    
    ; If we are here, the robot is not complete, so build the next part.
    jsr BuildRobotPart
    
    ; Now that the part is built, check AGAIN to see if we just finished.
    ldx RobotNum
    cpx #6
    bne @exit_compare
    jmp WinRobot
@exit_compare:
    rts
@win_game:
    jmp WinRobot

BuildRobotPart:
    lda #$00
    sta GearHeld
    lda #$01
    sta GearPresent
    jsr ClearGear
    
    ; Look up the data for the current robot part in the table.
    ldx RobotNum
    txa
    asl a       ; Multiply index by 3 to get offset in table.
    adc RobotNum
    tay         ; Y register is now the index into the table.

    ; Saving some memory space by loading coords from the table
    ; and storing in the appropriate tmp vars.
    lda RobotPartData+0, y ; Load X-coord
    sta tmpx
    lda RobotPartData+1, y ; Load Y-coord
    sta tmpy
    lda #BLUE
    sta Colour
    lda RobotPartData+2, y
    
    ; Push tmp vars into x and y registers to draw.
    ldx tmpx
    ldy tmpy
    ; A register holds the correct character code in lda
    ; command above the space.
    jsr DrawChar
    jsr Sfx_RobotBuild ; BLEEP BLOOP!
    
	ldx RobotNum
    lda RobotPartScores,x
    jsr AddScore
    inc RobotNum
    
    lda RobotNum                ; Load the new number of parts built to determine if speed+.
    lsr a                       ; Check if it's odd or even by shifting bit 0 into Carry.
    bcc @skip_speed_increase    ; If Carry is clear, the number is even, so skip.

    ; If we get here, an odd number of parts have been built, so we apply the speed increase.
    lda Period
    cmp #1                      ; Is speed already maxed out?
    beq @skip_speed_increase    ; If so, do nothing.
    dec Period                  ; Otherwise, increase the speed.
@skip_speed_increase:
    jsr SpawnGearAtBunker
    lda #YELLOW
    sta Colour
    lda #GEAR
    ldx GearPos
    ldy GearPosY
    jmp DrawChar

SpawnGearAtBunker:
    lda #$08
    sta GearPos
    lda BunkerY
    sec
    sbc #$01
    sta GearPosY
    rts

; Logic to determine if stage is complete.
WinRobot:
    lda #0
    sta RobotNum
	sta GearHeld
	lda #1
	sta GearPresent
	ldy #6              ; Load Y with the number of times to blink.
@BlinkLoop:
    ; Draw RED robot
    lda #RED
    sta Colour
    jsr DrawOrEraseRobot

    ; Save and restore Y around the DelayBlink call.
    tya
    pha
    jsr DelayBlink
    pla
    tay
        
    ; Erase robot.
    lda #BLACK
    sta Colour
    jsr DrawOrEraseRobot

    ; Save and restore Y around the second DelayBlink call.
    tya
    pha
    jsr DelayBlink
    pla
    tay
    
    ; Now it is safe to decrement the blink counter.
    dey                     
    bne @BlinkLoop

    jmp CheckWin
        
DrawOrEraseRobot:
    tya                     ; Transfer Y to A.
    pha                     ; Push A (to save the original Y value).

    ldx #5                  ; Loop through the 6 robot parts (index 5 down to 0).
@DrawPartLoop:
    ; Calculate offset for RobotPartData (index * 3).
    stx tmp4                ; Save the current index (X)
    txa                     ; Copy index to A
    asl a                   ; A = index * 2
    adc tmp4                ; A = (index * 2) + index = index * 3
    tay                     ; Y is now the byte offset into RobotPartData

    ; Load part data.
    lda RobotPartData+0, y
    sta tmpx
    lda RobotPartData+1, y
    sta tmpy
    
    ; Check if we are erasing or drawing the actual part.
    lda Colour
    cmp #BLACK
    bne @DrawThePart        ; If color is not BLACK, draw the real part.
    lda #EMPTY              ; Otherwise, draw an EMPTY sprite to erase.
    jmp @DoTheDraw
@DrawThePart:
    lda RobotPartData+2, y  ; Load the character for this specific part
@DoTheDraw:
    ldx tmpx
    ldy tmpy
    jsr DrawChar

    ldx tmp4                ; Restore the loop counter.
    dex
    bpl @DrawPartLoop       ; Loop for the next part.

    pla                     ; Pull the original Y value (via A register).
    tay                     ; Transfer it back to the Y register.
    rts

DelayBlink:   
    ldx #$50  ; Load Y with max value for delay.
              ; Increase value to slow blink/decrease to speed up.

DelayOuterLoop:
    ldy #$00

DelayLoop:
    iny
    bne DelayLoop
    dex
    bne DelayOuterLoop
    rts
    
; Add the value contained in A register to the current score.
AddScore:   
    clc
    adc Score
    sta Score
    bcc @contadd
    inc Score+1
@contadd:   
    jmp draw1l

; Check if the player won the game!!
CheckWin:   
    sei
    jsr ClearAllTentacles
    lda CannonPos
    sta OldCannonP
    lda CannonYPos
    sta OldCannonY
    jsr ClearCannon
    lda #1
    sta Win             ; This will stop the game.
    lda #$00            ; Mute all effects.
    sta NOISE
    sta EFFECTS
    jsr Sfx_LevelComplete
    ldx #4
    ldy #15
    lda #YELLOW
    sta Colour
    lda #<NextLvlSt     ; Write next stage messaage.
    sta LAB_01
    lda #>NextLvlSt
    sta LAB_02
    jsr PrintStr
    inc Level
@exit:      
    cli
    rts

; Game over! Zero the score, turn the screen to red and write "GAME OVER".
GameOver:   
    jsr ClearAllTentacles
    lda #$00            ; Mute all effects
    sta EFFECTS
    sta Level
    lda #$FF
    sta Win             ; Stop the game
    jsr ZeroScore       ; Put the score to zero
    lda #RED            ; Put all the screen in red (YOU DIED!)
    sta Colour
    jsr PaintColour
    jsr Sfx_GameOver
    ldx #3
    ldy #15
    lda #<GameOverSt    ; write "GAME OVER"
    sta LAB_01
    lda #>GameOverSt
    sta LAB_02
    jsr PrintStr
@exit:
    rts

; Draw the character in A in the position given by the X and Y registers
; Since the screen is 16 characters wide, we need to shift the Y register
; to multiply times 16 and then add the X contents. It uses PosX and PosY.
; Colour contains the colour code of the character. It uses 1 byte in the
; stack and does not change A, X, Y.
DrawChar:
    cpx #22
    bcs @exit
    cpy #31
    bcs @exit

    pha                 ; Save the character code (passed in A) onto the stack
    sty PosY            ; Save the original Y-coordinate

    jsr PosChar         ; Calculate screen/color pointers (this changes A and Y)

    ldy #0              ; Set index to 0 for the indirect write
    lda Colour          ; Load the desired color into A
    sta (POSCOLPT),y    ; Write it to the color RAM location
    
    pla                 ; Pull the original character code from the stack back into A
    sta (POSCHARPT),y   ; Write the character to the screen RAM location

    ldy PosY            ; Restore the original Y-coordinate
@exit:
    rts

; Calculate the address of a screen position and put it in POSCHARPT. Do the
; same for the color address and put it in POSCOLPT.
; X and Y contain screen coordinates.
PosChar:    
    stx PosX
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
@nocorr:    
    adc PosX
    pha
    adc POSCHARPT
    sta POSCHARPT
    bcc @no_carry_1
    inc POSCHARPT+1
@no_carry_1:
    pla
    clc
    adc POSCOLPT
    sta POSCOLPT
    bcc @no_carry_2
    inc POSCOLPT+1
@no_carry_2:
    rts

; Print a string (null terminated) whose address is contained in LAB_01 and
; LAB_02 at the position given by X and Y pointers
PrintStr:   
    sty PosY
    ldy #$00
@loop:      
    lda (LAB_01),Y
    beq @exit
    sty tmp4
    ldy PosY
    jsr DrawChar
    ldy tmp4
    iny
    inx
    jmp @loop
@exit:
    ldy PosY
    rts

; Clear the screen. This maybe is too slow to be used in an interrupt handler.
; Draw everywhere the character contained in the A register. Employs X.
CLS:
    size=16*31/4+1
    ldx #size
@loop:      
    sta MEMSCR-1,X          ; A (small) degree of loop unrolling avoids
    sta MEMSCR+size-1,X     ; having to mess with a 16-bit loop.
    sta MEMSCR+size*2-1,X
    sta MEMSCR+size*3-1,X
    dex
    bne @loop
    rts

; Put the colour code contained in A everywhere in the screen
PaintColour:
    ldx #size
@loop:      
    sta MEMCLR-1,X
    sta MEMCLR+size-1,X
    sta MEMCLR+size*2-1,X
    sta MEMCLR+size*3-1,X
    dex
    bne @loop
    rts

; A simple delay - used for music.
Delay:      
    ldx #$FF
    ldy #$FF
            
Delayloop:  
    dex
    bne Delayloop
    dey
    bne Delayloop
    rts

ShortDelay: 
    ldy #$40
    ldx #$FF
    jmp Delayloop

; Random number generation routine. Adapted from here:
; http://sleepingelephant.com/ipw-web/bulletin/bb/viewtopic.php?t=2304
; Creates a pseudo-random number in Random and Random+1
; Change register A and employs tmpindex
GetRand:    
    lda Random+1
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
Bin2BCD:
    lda #0              ; Clear the result area
    sta BCD_Res         ; Use BCD_Res
    sta BCD_Res_1       ; Use BCD_Res_1
    sta BCD_Res_2       ; Use BCD_Res_2
    ldx #16             ; Setup the bit counter
    sed                 ; Enter decimal mode
@loop:      
    asl BCD_Val         ; Use BCD_Val (equivalent to BCD_Val+0)
    rol BCD_Val_Hi      ; Use BCD_Val_Hi
    lda BCD_Res         ; Use BCD_Res (equivalent to BCD_Res+0)
    adc BCD_Res
    sta BCD_Res
    lda BCD_Res_1       ; Use BCD_Res_1
    adc BCD_Res_1
    sta BCD_Res_1
    lda BCD_Res_2       ; Use BCD_Res_2
    adc BCD_Res_2
    sta BCD_Res_2
    dex                 ; More bits to process?
    bne @loop
    cld                 ; Leave decimal mode
    rts

; Print the BCD value in A as two ASCII digits
PrintBCD:   
    pha             ; Save the BCD value
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

; SOUND METHODS
InitMusic:
    ; Reset simple sound effect timer.
    lda #0
    sta SfxTimer

    ; Reset background music player.
    sta Voice1Ptr
    sta Voice2Ptr
    sta GameOverSfxCtr
    lda #1              ; Set counters to 1 to trigger immediate note load
    sta Voice1Ctr
    sta Voice2Ctr
    rts

MusicUpdate:
    lda MusicMuted
    bne @music_is_muted
    lda SfxTimer            ; Check if a sound effect is playing.
    bne @exit_music         ; If so, sound effects have priority, so exit.

    dec Voice1Ctr           ; Decrement this note's duration counter
    bne @voice_2_logic      ; If note is still playing, skip to the next voice
    
    ; Note's duration is over - time to load the next one.
    ldy Voice1Ptr           ; Load current position in music data
    lda MusicData1,y        ; Get the byte (note)
    cmp #0                  ; Is it the loop marker (0)?
    bne @v1_is_note
    ; Reset pointer to the start of the data.
    ldy #0
    sty Voice1Ptr
    lda MusicData1,y        ; Load the first note of the tune
@v1_is_note:
    sta VOICE1              ; Play the note
    iny                     ; Point to the duration byte
    lda MusicData1,y        ; Load the duration
    sta Voice1Ctr           ; Set the new duration
    iny                     ; Point to the next note's position
    sty Voice1Ptr           ; Save the new pointer
@voice_2_logic:
    dec Voice2Ctr
    bne @exit_music
    ldy Voice2Ptr
    lda MusicData2,y
    cmp #0
    bne @v2_is_note
    ldy #0
    sty Voice2Ptr
    lda MusicData2,y
@v2_is_note:
    ora #%01000000          ; Set the pulse waveform bit for a different timbre
    sta VOICE2              ; Play the note
    iny
    lda MusicData2,y
    sta Voice2Ctr
    iny
    sty Voice2Ptr
    jmp @exit_music
@music_is_muted:
    lda #0                  ; When muted, write 0 to silence the channels
    sta VOICE1
    sta VOICE2
@exit_music:
    rts

Sfx_GearPickup:
    lda #$D8          ; High-pitched triangle wave.
    sta VOICE2
    lda #8            ; Duration of 8 frames.
    sta SfxTimer
    rts

Sfx_RobotBuild:
    lda #$C4          ; Medium-pitched pulse wave.
    sta VOICE1
    lda #10           ; Duration of 10 frames.
    sta SfxTimer
    rts

Sfx_PlayerStun:
    lda #$E0          ; High-pitched noise.
    sta NOISE
    lda #20           ; Duration of 20 frames.
    sta SfxTimer
    rts
    
Sfx_GameOver:
    lda #46           ; Start the Game Over sequence timer.
    sta GameOverSfxCtr
    rts

Sfx_LevelComplete:
    ; This does not use the IRQ handler as it will cause odd
    ; issues with the sound as it continues to process other aspects.
    ; Play a triumphant chord for a bit, then turn off sound awaiting
    ; input to start next level.
    lda #$AD          ; Play note C
    sta VOICE1
    lda #$B1          ; Play note E
    sta VOICE2
    lda #$B4          ; Play note G
    sta EFFECTS
    jsr Delay         ; Hold the chord for a moment
    lda #0            ; Silence all channels
    sta VOICE1
    sta VOICE2
    sta EFFECTS
    rts

Irq_HandleGameOver:
    ; Plays a series of downbeat notes to indicate that the game is over.
    ; Use noise channel to add a drumbeat, then layer a chord over the next
    ; 15 frames.
    lda GameOverSfxCtr
    beq @sound_is_finished      ; If counter is 0, the sequence is over.

    dec GameOverSfxCtr          ; Count down the main timer.
    lda GameOverSfxCtr          ; Load the new value to check our place in the sequence.

    cmp #45                     ; At frame 45, play the drums.
    beq @set_drum
    cmp #40                     ; At frame 40, play the first sad note.
    beq @set_note_1
    cmp #35                     ; At frame 35, play the second.
    beq @set_note_2
    cmp #30                     ; At frame 30, play the third.
    beq @set_note_3
    jmp @exit_game_over_irq     ; In between notes, do nothing.
@set_drum:
    lda #0                      ; Silence tone channels.
    sta VOICE1
    sta VOICE2
    lda #$8A                    ; Play a low, percussive noise.
    sta NOISE
    jmp @exit_game_over_irq
@set_note_1:
    lda #0                      ; Silence noise channel.
    sta NOISE
    lda #$9A                    ; Play descending note 1.
    sta VOICE1
    jmp @exit_game_over_irq
@set_note_2:
    lda #$96                    ; Play descending note 2.
    sta VOICE1
    jmp @exit_game_over_irq
@set_note_3:
    lda #$94                    ; Play descending note 3.
    sta VOICE1
    jmp @exit_game_over_irq
@sound_is_finished:
    lda #0                      ; When finished, silence all sound channels.
    sta VOICE1
    sta NOISE
@exit_game_over_irq:
    rts

.segment "BSS_VARS"
    ; Tentacle Data
    Tentacle_Length:      .res 10
    Tentacle_PathIndex:   .res 10
    Tentacle_State:       .res 10
    Tentacle_Old_Length:  .res 10

    ; Temporary variables
    ProcessingGearAward:  .res 1
    TempOldY:             .res 1
    TempLoopY:            .res 1
	
.segment "CODE"

; Data and configuration settings. Tables and custom characters.
MusicData1: ; Arpeggio (Voice 1 - Sawtooth wave)
    .byte $A5, 5  ; Note A, duration 5
    .byte $AC, 5  ; Note C, duration 5
    .byte $B1, 5  ; Note E, duration 5
    .byte $00     ; Loop back to start

MusicData2: ; Bassline (Voice 2 - Pulse wave)
    .byte $65, 7  ; Note A (low), duration 7
    .byte $64, 7  ; Note G# (low), duration 7
    .byte $00     ; Loop back to start

NextLvlSt: .byte 14, 5, 24, 20, 63, 0 ; "NEXT?"

GameOverSt: .byte (7+$80), (1+$80), (13+$80), (5+$80), (32+$80), (15+$80)
            .byte (22+$80), (5+$80), (18+$80), 0 ; "GAME OVER"
            
LevelStr: .byte 18, 15, 2, 15, 20, 58, 32, 0   ; "ROBOT:"

DefChars:
    ; Define/draw each character.  The numeric value is the spot
    ; in the standard VIC character table that is being replaced.
    TENTACLE1 = 0
    .byte %00000000     
    .byte %00000001
    .byte %11000111
    .byte %11101110
    .byte %00111100
    .byte %00111000
    .byte %00000000
    .byte %00000000

    GEARCHEST = 1
    .byte %00101000     ; Character 1 is the letter A
    .byte %01111100
    .byte %01111100
    .byte %00101000
    .byte %11111111
    .byte %11111111
    .byte %11111111
    .byte %11111111

    OCTO = 31
    .byte %11001111
    .byte %11001111
    .byte %01100111
    .byte %00110000
    .byte %00011000
    .byte %00001100
    .byte %00000111
    .byte %00000000

    OCTO2 = 16
    .byte %00000000
    .byte %00000111
    .byte %00001100
    .byte %00011000
    .byte %00110000
    .byte %01100111
    .byte %11001111
    .byte %11001111

    PLAYER = 4
    .byte %00110011     ; AKA Cannon
    .byte %00110110
    .byte %00111100
    .byte %01111000
    .byte %00110000
    .byte %01001000
    .byte %01001000
    .byte %01101100

    HEAD = 17
    .byte %00111100     ; Robot Head
    .byte %01111110
    .byte %11011011
    .byte %11111111
    .byte %01100110
    .byte %00111100
    .byte %01100110
    .byte %00000000

    RARM = 6
    .byte %11100000
    .byte %01100000
    .byte %01100000
    .byte %01100000
    .byte %01100000
    .byte %01100110
    .byte %00111100
    .byte %00011000

    LARM = 26
    .byte %00000111
    .byte %00000110
    .byte %00000110
    .byte %00000110
    .byte %00000110
    .byte %01100110
    .byte %00111100
    .byte %00011000

    TORSO = 22
    .byte %11111111
    .byte %11100111
    .byte %01100110
    .byte %01100110
    .byte %01100110
    .byte %01100110
    .byte %00111100
    .byte %00011000

    EMPTY = 25
    .byte %00000000     ; Blank character
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000
    .byte %00000000

    LLEG = 10
    .byte %00000111
    .byte %00000110
    .byte %00000110
    .byte %00000110
    .byte %00000110
    .byte %00000110
    .byte %00011110
    .byte %00111110

    RLEG = 11
    .byte %11100000
    .byte %01100000
    .byte %01100000
    .byte %01100000
    .byte %01100000
    .byte %01100000
    .byte %01110000
    .byte %01111000

    GEAR = 12
    .byte %10011001
    .byte %01011010
    .byte %00111100
    .byte %11111111
    .byte %00111100
    .byte %11111111
    .byte %01011010
    .byte %10011001

    OCTO3 = 27
    .byte %11110011
    .byte %11110011
    .byte %11100110
    .byte %00001100
    .byte %00011000
    .byte %00110000
    .byte %11100000
    .byte %00000000

    OCTO4 = 28
    .byte %00000000
    .byte %11100000
    .byte %00110000
    .byte %00011000
    .byte %00001100
    .byte %11100110
    .byte %11110011
    .byte %11110011

    TENTACLE2 = 29
    .byte %00000000
    .byte %00000001
    .byte %00000111
    .byte %00001110
    .byte %00111100
    .byte %00111000
    .byte %01110000
    .byte %11100000

    TENTACLE3 = 30
    .byte %11000000
    .byte %11000000
    .byte %11100000
    .byte %01110000
    .byte %00111000
    .byte %00111000
    .byte %00011100
    .byte %00001111
            
LevelsPer: .byte  8,6,4,4,3,3,2,2,1,1,1,1

CurrentStr: .byte 19, 3, 15, 18, 5, 0 ; "SCORE"

HighStr: .byte 8, 9, 19, 3, 15, 18, 5, 0 ; "HISCORE"

TimeStr: .byte 20, 9, 13, 5, 58, 0   ; "TIME:"

TentacleDrawData:
    ; Format is: X-coordinate, Y-coordinate, Character Code
    ; All expressions have been pre-calculated into literal values.

    ; TENTACLE 1=0 = wiggly noodle
    ; TENTACLE 2=29 = upper right to bottom left
    ; TENTACLE 3=30 = upper left to bottom right
    
    ; Tentacle 1 (top left)
    .byte 7, 17, 30
    .byte 6, 16, 30
    .byte 5, 16, 0
    .byte 4, 16, 0
    .byte 3, 16, 0
    .byte 2, 16, 29

    ; Stops position 1
    

    ; Tentacle 2 (bottom left)
    .byte 7, 18, 29
    .byte 6, 19, 29
    .byte 5, 20, 0
    .byte 5, 19, 25
    
    ; Stops position 4


    ; Tentacle 3 (far left)
    .byte 7, 18, 0
    .byte 6, 18, 0
    .byte 5, 18, 0
    .byte 4, 18, 0
    .byte 3, 18, 0
    .byte 3, 17, 25

    ; Stops position 2
    
    .byte 7, 18, 0
    .byte 6, 18, 0
    .byte 5, 18, 0
    .byte 4, 18, 29

    ; Stops position 3

    ; Tentacle 4 (far right)
    .byte 10, 18, 0
    .byte 11, 18, 0
    .byte 12, 18, 0

    ; Does not stop player

    ; Tentacle 5 (top right)
    .byte 10, 17, 29
    .byte 11, 16, 29
    .byte 12, 16, 30

    ; Does not stop player

    ; Tentacle 6 (bottom right-ish)
    .byte 10, 19, 30
    .byte 11, 20, 30
    .byte 12, 21, 30

    ; Tentacle 7 (far bottom left-ish)
    .byte 7, 19, 29
    .byte 6, 20, 29

    ; Stop position 5
    
    .byte 7, 19, 29
    .byte 6, 20, 29
    .byte 6, 21, 30
    .byte 7, 21, 25

    ; Stop position 6

    ; Tentacle 8 (far bottom right to left)
    .byte 9, 19, 29
    .byte 9, 20, 30
    .byte 9, 21, 29
    .byte 8, 22, 29

    ; Stop position 7
    
    .byte $FF

TentaclePathData:
    ; Format: Start Byte Offset, Number of Segments, Is a "Stop" Path (1=yes, 0=no)
    .byte 0,   6, 1  ; Path 0 (Tentacle 1)
    .byte 18,  4, 1  ; Path 1 (Tentacle 2)
    .byte 30,  6, 1  ; Path 2 (Tentacle 3, part 1)
    .byte 48,  4, 1  ; Path 3 (Tentacle 3, part 2)
    .byte 60,  3, 0  ; Path 4 (Tentacle 4)
    .byte 69,  3, 0  ; Path 5 (Tentacle 5)
    .byte 78,  3, 0  ; Path 6 (Tentacle 6)
    .byte 87,  2, 1  ; Path 7 (Tentacle 7, part 1)
    .byte 93,  4, 1  ; Path 8 (Tentacle 7, part 2)
    .byte 105, 4, 1  ; Path 9 (Tentacle 8)

NUM_TENTACLE_PATHS = 9

RobotPartScores:
    .byte 10  ; LLEG score (displays as 100)
    .byte 10  ; RLEG score (displays as 100)
    .byte 20  ; TORSO score (displays as 200)
    .byte 10  ; RARM score (displays as 100)
    .byte 10  ; LARM score (displays as 100)
    .byte 50  ; HEAD score (displays as 500)
	
RobotPartData:
    ; Format: X-coord, Y-coord, Character Code
    .byte $00, $09, LLEG
    .byte $02, $09, RLEG
    .byte $01, $08, TORSO
    .byte $02, $08, RARM
    .byte $00, $08, LARM
    .byte $01, $07, HEAD

NUMLEVEL   = 12 ; Total number of levels.

EndMemAddress = EndMemMrk

EndMemMrk:
