;			         V I C   R O B O - F R E N Z Y 
;
;             originally by Davide Bucci, April-June 2018
;             modified by Nicholas Baldridge for For Amusement Only Games, LLC.
;
; A small loader showing an instruction screen and playing a two voice tune
; (J.S. Bach, Fantasia of Partita 3, BWV 827). Attempted to retain as much
; of the nuance and tone of Davide's work, but altered to support NTSC
; a bit better.
;
; The assembler used is ca65
;

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
    ORANGE = $0A
    LIGHT_BLUE = $0B
    BROWN = $09

; KERNAL routines used
    GETIN = $FFE4

; Page-0 addresses used (for indirect indexed addressing and other things)
    LAB_01 = $01
    LAB_02 = $02
    LAB_03 = $03
    LAB_06 = $06
    LAB_07 = $07
    LAB_08 = $08
    IsNTSC = $09
    LAB_0A = $0A

    PosX_Current = $04
    PosY_Initial = $05



    CHRPTR       = $0E     ; Pointer to the original ch. in a sprite (word)
    POSCHARPT    = $1A     ; Pointer for a character in memory (word)
    POSCOLPT     = $1C     ; Pointer for a colour in memory (word)
    TmpScan      = $20     ; Used in raster line sync code
    
    Voice1ptr = $30
    Voice1ctr = $31
    Loop1ctr  = $32
    Loop1str  = $33
    Voice1drt = $34
    Voice1nod = $35
    
    Voice2ptr = $36
    Voice2ctr = $37
    Loop2ctr  = $38
    Loop2str  = $39
    Voice2drt = $3A
    Voice2nod = $3B
    ScorePrintPtr       = $41 ; Pointer to the string being printed (word)
    ScorePrintIndex     = $43 ; Index of the current character in the string
    ScorePrintX         = $44 ; Starting X position for the string
    ScorePrintY         = $45 ; Y position for the string
    InitialMuteTimer = $4D
    MusicMuted = $4E    i	  ; Flag to mute music
    IrqSyncFlag = $4F
    
    RobotAnimState      = $50 ; What is the robot animation doing?
    RobotAnimCounter    = $51 ; Which part/flash are we on?
    RobotAnimTimer      = $52 ; How long to pause between robot steps.

    ScoreAnimState      = $53 ; What score text are we showing?
    ScoreAnimTimer      = $54 ; How long to show the score text.

    TentacleAnimState   = $55 ; Is the tentacle extending or retracting?
    TentacleAnimCounter = $56 ; What is the current length of the tentacle?
    TentacleAnimTimer   = $57 ; How long to pause between tentacle steps.

    ROBOT_STATE_BUILDING    = 0
    ROBOT_STATE_FLASHING    = 1
    SCORE_STATE_IDLE        = 0
    SCORE_STATE_DRAW_LEGS   = 1
    SCORE_STATE_DRAW_BODY   = 2
    SCORE_STATE_DRAW_ARMS   = 3
    SCORE_STATE_DRAW_HEAD   = 4
	SCORE_STATE_PRINTING	= 5

    TENTACLE_STATE_EXTEND   = 0
    TENTACLE_STATE_RETRACT  = 1
    
    CharCode = LAB_03
    PosX = PosX_Current
    PosY = PosY_Initial
    Colour = LAB_06  
    tmp4 = LAB_0A
    secs = LAB_07       ; Write in the second half of the screen
    portaconfig = LAB_08

; VIC-chip addresses
    VICSCRHO = $9000    ; Horizontal position of the screen
    VICSCRVE = $9001    ; Vertical position of the screen
    VICCOLNC = $9002    ; Screen width in columns and video memory addr.
    VICROWNC = $9003    ; Screen height, 8x8 or 8x16 chars, scan line addr.
    VICRAST  = $9004    ; Bits 8-1 of the current raster line.
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


    VOICE1  = GEN3
    VOICE2  = GEN1
    VOICE3  = GEN2
    EFFECTS = GEN3
        

.export main
.segment "STARTUP"
.segment "LOWCODE"
.segment "INIT"
.segment "GRCHARS"
.segment "CODE"

; Main program here.
main:
    jsr Init        ; Init the game (load graphic chars, etc...)
    lda #32
    jsr CLS
    ldx #0
    ldy #0
    lda #BLACK
    stx secs
    lda #YELLOW
    sta Colour

    lda #<FAOGames
    sta LAB_01
    lda #>FAOGames
    sta LAB_02
    jsr PrintStr
    lda #<FAOGames2
    sta LAB_01
    lda #>FAOGames2
    sta LAB_02
    jsr PrintStr

    ldy #0
    lda #<Presents
    sta LAB_01
    lda #>Presents
    sta LAB_02
    lda #GREEN
    sta Colour
    jsr PrintStr

    ldy #1
    lda #<TitleGame
    sta LAB_01
    lda #>TitleGame
    sta LAB_02
    lda #WHITE
    sta Colour
    jsr PrintStr

    ldy #2
    lda #<Scoring
    sta LAB_01
    lda #>Scoring
    sta LAB_02
    lda #CYAN
    sta Colour
    jsr PrintStr

    lda #<PlayerDisplay
    sta LAB_01
    lda #>PlayerDisplay
    sta LAB_02
    lda #GREEN
    sta Colour
    ldx #1
    ldy #8
    jsr PrintStr

    lda #WHITE
    ldx #1
    jsr PaintJustColour
    jsr WaitForNextIRQ
    lda #CYAN
    ldx #2
    jsr PaintJustColour
    jsr WaitForNextIRQ

    ; Basic game instructions.
    ldx #7
    ldy #8
    lda #<GameDesc
    sta LAB_01
    lda #>GameDesc
    sta LAB_02
    lda #CYAN
    sta Colour
    jsr PrintStr

    lda #YELLOW
    ldx #13
    jsr PaintJustColour

    ldx #0
    ldy #9
    lda #<GameDesc2
    sta LAB_01
    lda #>GameDesc2
    sta LAB_02
    lda #CYAN
    sta Colour
    jsr PrintStr

    ldx #0
    ldy #15
    lda #<GameDesc3
    sta LAB_01
    lda #>GameDesc3
    sta LAB_02
    lda #CYAN
    sta Colour
    jsr PrintStr

    ; Description of keyboard commands
    ; and other controls.
    ldx #0
    ldy #17
    lda #<Keys
    sta LAB_01
    lda #>Keys
    sta LAB_02
    lda #CYAN
    sta Colour
    jsr PrintStr
    lda #WHITE
    ldx #1
    jsr PaintJustColour
    jsr WaitForNextIRQ
    
    ldx #0
    ldy #18
    lda #<Keys2
    sta LAB_01
    lda #>Keys2
    sta LAB_02
    lda #CYAN
    sta Colour
    jsr PrintStr
    lda #WHITE
    ldx #1
    jsr PaintJustColour
    jsr WaitForNextIRQ
    
    ldx #2
    ldy #19
    lda #<Keys3
    sta LAB_01
    lda #>Keys3
    sta LAB_02
    lda #CYAN
    sta Colour
    jsr PrintStr
    
    ldx #0
    ldy #20
    lda #<Keys4
    sta LAB_01
    lda #>Keys4
    sta LAB_02
    lda #CYAN
    sta Colour
    jsr PrintStr
    lda #WHITE
    ldx #1
    jsr PaintJustColour
    jsr WaitForNextIRQ
    
    ldx #0
    ldy #21
    lda #<Keys5
    sta LAB_01
    lda #>Keys5
    sta LAB_02
    lda #CYAN
    sta Colour
    jsr PrintStr

    lda #WHITE
    ldx #1
    ldy #21
    jsr PaintJustColour
    jsr WaitForNextIRQ

    ldx #2
    lda #WHITE
    jsr PaintJustColour
    jsr WaitForNextIRQ

    ldx #3
    lda #WHITE
    jsr PaintJustColour
    jsr WaitForNextIRQ

    ldx #4
    lda #WHITE
    jsr PaintJustColour
    jsr WaitForNextIRQ

    ldx #5
    lda #WHITE
    jsr PaintJustColour
    jsr WaitForNextIRQ

    ldx #6
    lda #WHITE
    jsr PaintJustColour

    ldx #0
    ldy #22
    lda #<Keys6
    sta LAB_01
    lda #>Keys6
    sta LAB_02
    lda #CYAN
    sta Colour
    jsr PrintStr

    lda #WHITE
    ldx #1
    ldy #22
    jsr PaintJustColour
    jsr WaitForNextIRQ

    ldx #2
    lda #WHITE
    jsr PaintJustColour
    jsr WaitForNextIRQ

    ldx #3
    lda #WHITE
    jsr PaintJustColour
    jsr WaitForNextIRQ

    ldx #4
    lda #WHITE
    jsr PaintJustColour
    jsr WaitForNextIRQ

    ldx #5
    lda #WHITE
    jsr PaintJustColour
    jsr WaitForNextIRQ

    lda #MAGENTA
    sta Colour
    lda #OCTO
    ldx #14
    ldy #18
    jsr DrawChar
    lda #OCTO2
    ldx #14
    ldy #17
    jsr DrawChar
    lda #OCTO3
    ldx #15
    ldy #18
    jsr DrawChar
    lda #OCTO4
    ldx #15
    ldy #17
    jsr DrawChar

    lda PORTAVIA1d
    sta portaconfig
    lda #0          ; Many thanks to @BedfordLvlExp for the joystick
    sta PORTAVIA1d  ; code here!

MainLoop:
	jsr WaitForNextIRQ
	jsr CheckForExit
    jmp MainLoop
            
PrepareChainload:
	sei                 

    ; Stop sound
    lda #$00
    sta GEN1
	sta GEN2
	sta GEN3
	sta VOLUME

    ldy #0
@CopyLoop:
    lda LoaderStub,y
    sta $0300,y         ; Copy to cassette buffer ($033C)
    iny
    cpy #(LoaderStubEnd - LoaderStub)
    bne @CopyLoop

    jmp $033C

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
    cmp #140        ; Check for line 280+ (140 * 2), which is only present on PAL machines.
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
    ; Set a stable NTSC timer value for ~60Hz (17095 cycles)
    lda #$CB  ; Low byte of 17095
    ldx #$42  ; High byte of 17095
    ;lda #<(261 * 58 - 2)
    ;ldx #>(261 * 58 - 2)
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
    lda #<(312 * 51 - 2)
    ldx #>(312 * 51 - 2)
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
    jsr MovCh
    
    lda #%01000000 		 	; Bit 6 = start T1 in continuous mode (not one-shot)
    sta $912B       		; VIA 2 Auxiliary Control Register

    lda #%11000000  		; Bit 7 = Set interrupt flag, Bit 6 = Enable Timer 1 IRQ
    sta $912E       		; VIA 2 Interrupt Enable Register
    
    lda #<IrqHandler
    sta $0314
    lda #>IrqHandler
    sta $0315
    cli

    lda #<Voice1data    	; Load low byte of Voice1data address
    sta Voice1ptr       	; Initialize Voice1ptr to start of music data
    lda #>Voice1data    	; Load high byte of Voice1data address
    sta Voice1ptr+1     	; Voice1ptr is a 2-byte pointer, so store high byte

    lda #0              	; Set accumulator to 0 for counters
    sta Voice1ctr       	; Initialize Voice1ctr to 0
    sta Voice1drt       	; Initialize Voice1drt to 0 (duration)
    sta Voice1nod       	; Initialize Voice1nod to 0 (note duration)

    lda #<Voice2data    	; Load low byte of Voice2data address
    sta Voice2ptr       	; Initialize Voice2ptr to start of music data
    lda #>Voice2data    	; Load high byte of Voice2data address
    sta Voice2ptr+1     	; Voice2ptr is a 2-byte pointer

    lda #0              	; Set accumulator to 0 for counters
    sta Voice2ctr       	; Initialize Voice2ctr to 0
    sta Voice2drt       	; Initialize Voice2drt to 0
    sta Voice2nod       	; Initialize Voice2nod to 0
    
    sta PORTAVIA1d
    lda #$7F
    sta PORTBVIA2d
    
    lda #$0F        		; Max volume (0-F for 16 levels)
    sta VOLUME
    
    lda #1              	; 1. Start with music muted
    sta MusicMuted
    lda #50             	; 2. Set delay timer
    sta InitialMuteTimer
    lda #0
    sta RobotAnimCounter
    sta RobotAnimState
    sta RobotAnimTimer
    sta TentacleAnimCounter
    sta TentacleAnimState
    sta TentacleAnimTimer
    sta ScoreAnimState
    sta ScoreAnimTimer
    sta ScorePrintIndex
    sta ScorePrintX
    sta ScorePrintY
    rts

; Screen init value for PAL and NTSC
CenterScreenPAL:
    lda #0
    sta IsNTSC
    ldx #$12 ; Custom horizontal centering for PAL
    ldy #$16 ; Custom vertical centering for PAL
    rts

CenterScreenNTSC:
    lda #1
    sta IsNTSC
    ldx #$0A ; Custom horizontal centering for NTSC
    ldy #$10 ; Custom vertical centering for NTSC
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

IrqHandler: 
    php
    pha
    txa 		            ; Save registers
    pha
    tya
    pha
    lda InitialMuteTimer
    beq @delay_is_over      ; If timer is already zero, skip this.
    dec InitialMuteTimer    ; Otherwise, decrement the timer.
    bne @delay_is_over      ; If it's still not zero, do nothing more.
    ; Timer just hit zero, so unmute the music now.
    lda #0
    sta MusicMuted
@delay_is_over:
    inc IrqSyncFlag
    jsr UpdateRobotAnimation
    jsr UpdateTentacleAnimation
    jsr UpdateScoreAnimation
    jsr Music1
    jsr Music2
    pla      		       ; Restore registers
    tay
    pla
    tax
    pla
    plp
    jmp $EABF       	   ; Jump to the standard IRQ handling routine

ScaleDurationForNTSC:
    ldy IsNTSC
    beq @exit_scale        ; If PAL (IsNTSC=0), exit immediately

    ; It's NTSC, so scale the value in A to approx. 7/8ths
    pha                    ; Save value
    lsr a                  ; A = A / 2
    lsr a                  ; A = A / 4
    lsr a                  ; A = A / 8
    sta tmp4               ; Store the amount to subtract
    pla                    ; Restore original value
    sec
    sbc tmp4               ; A = A - (A / 8)
@exit_scale:
    rts

; Music driver for voice 1. It should be called every IRQ to handle music
Music1:
    lda MusicMuted
    bne @music_is_muted_1

    lda Voice1ctr       ; Check if the current note's duration is over
    bne @continue_note_1; If not, branch to the fast-path handler

    jsr @playnext_1     ; If it is over, call the subroutine to get a new note
    rts
@continue_note_1:
    ; A still holds Voice1ctr, so we can check for note-off
    cmp Voice1nod
    bne @skip_silence_1 ; If it's not time to silence, skip ahead

    ; It's time to silence the note
    lda #$00
    sta VOICE1
    sta VOICE3

@skip_silence_1:
    dec Voice1ctr       ; Now, it's safe to decrement the counter
    rts

@music_is_muted_1:
    lda #$00
    sta VOICE1
    sta VOICE3
    rts

@playnext_1:
	ldx Voice1ptr
@playnext_loop_1:
    lda Voice1data,x
    cmp #repeatm
    beq @repeat_1
    and #maskcode
    cmp #notecode
    beq @note_1
    cmp #duracode
    beq @duration_1
    inc Voice1ptr
    rts
@note_1:
    lda Voice1data,x
    ldy IsNTSC
    beq @set_voice_1
    sec
    sbc #$01
@set_voice_1:
    cmp #silence
    bne @play_sound_1
    lda #$00
@play_sound_1:
    sta VOICE1
    sta VOICE3
    lda Voice1drt
    sta Voice1ctr
    inc Voice1ptr
    rts
@duration_1:
    inx
    lda Voice1data,x
    jsr ScaleDurationForNTSC
    sta Voice1drt
    inx
    lda Voice1data,x
    jsr ScaleDurationForNTSC
    sta Voice1nod
    stx Voice1ptr
    jmp @playnext_loop_1
@repeat_1:
    ldx #0
    stx Voice1ptr
    stx Voice2ptr
    stx Voice1ctr
    stx Voice2ctr
    rts

WaitForNextIRQ:
    lda IrqSyncFlag
@wait_loop:
    cmp IrqSyncFlag
    beq @wait_loop
    rts

; Music driver for voice 2. It should be called every IRQ to handle music
Music2:
    lda MusicMuted
    bne @music_is_muted_2

    lda Voice2ctr
    bne @continue_note_2

    jsr @playnext_2
    rts
@continue_note_2:
    cmp Voice2nod
    bne @skip_silence_2

    lda #$00
    sta VOICE2
    nop                 ; Balance timing with Music1
    nop                
@skip_silence_2:
    dec Voice2ctr
    rts
@music_is_muted_2:
    lda #$00
    sta VOICE2
    nop
    nop
    rts
@playnext_2:
	ldx Voice2ptr
@playnext_loop_2:
    lda Voice2data,x
    cmp #repeatm
    beq @repeat_2
    and #maskcode
    cmp #notecode
    beq @note_2
    cmp #duracode
    beq @duration_2
    inc Voice2ptr
    rts
@note_2:
    lda Voice2data,x
    ldy IsNTSC
    beq @set_voice_2
    sec
    sbc #$01
@set_voice_2:
    cmp #silence
    bne @play_sound_2
    lda #$00
@play_sound_2:
    sta VOICE2
    nop
    nop 
    lda Voice2drt
    sta Voice2ctr
    inc Voice2ptr
    rts
@duration_2:
    inx
    lda Voice2data,x
    jsr ScaleDurationForNTSC
    sta Voice2drt
    inx
    lda Voice2data,x
    jsr ScaleDurationForNTSC
    sta Voice2nod
    stx Voice2ptr
    jmp @playnext_loop_2
@repeat_2:
    ldx #0
    stx Voice1ptr
    stx Voice2ptr
    stx Voice1ctr
    stx Voice2ctr
    rts

; Draw the character in A in the position given by the X and Y registers
; Since the screen is 16 characters wide, we need to shift the Y register
; to multiply times 16 and then add the X contents. It uses CharCode and PosX.
; Colour indicates the colour code of the character. It uses 3 bytes in the
; stack and does not change registers.
DrawChar:
    sta CharCode
    stx PosX
    sty PosY

    ; This calculation determines the screen memory offset.
    tya             ; Move Y-coordinate to Accumulator
    asl
    asl
    asl
    asl             ; A = low byte of (Y * 16). Carry flag holds the high bit.

    pha             ; Save the low byte of the offset on the stack.

    ; Check the carry flag to see if we overflowed into the "bottom half" of the screen.
    lda #0          ; Assume we are in the top half (secs = 0).
    bcc @no_carry   ; If the carry flag is clear (Y < 16), skip ahead.
    lda #1          ; If the carry flag is set (Y >= 16), set secs to 1 for the bottom half.
@no_carry:
    sta secs        ; Store the result.

    ; Now, calculate the final position within the correct half.
    pla             ; Restore the low byte of the offset.
    clc
    adc PosX        ; Add the X-coordinate.
    tay             ; Y now holds the final offset within the 256-byte page.

    ; Check the 'secs' flag to write to the correct memory location.
    lda secs
    cmp #1
    bne @dc_tophalf
@dc_bottomhalf:
    lda Colour
    sta MEMCLR+256,y
    lda CharCode
    sta MEMSCR+256,y
    jmp @dc_exit
@dc_tophalf:
    lda Colour
    sta MEMCLR,y
    lda CharCode
    sta MEMSCR,y
@dc_exit:
    ldy PosY
    rts

; Paints a color at the position given by X and Y registers.
; Does not change the character on screen.
; IN: X=x-coord, Y=y-coord, A=color
PaintJustColour:
    ; On entry: A = color, X = x-coord, Y = y-coord
    sty PosY        ; Save the original Y register in your temp variable

    pha             ; Save the color (A) on the stack
    stx PosX        ; Use the X register that was passed in for the position

    tya             ; Move Y to A for the line calculation
    asl
    asl
    asl
    asl             ; A now holds the line offset (Y * 16)

    pha             ; *** Temporarily save this calculated line offset on the stack ***

    lda secs        ; Now it's safe to load secs without erasing the calculation
    cmp #$01
    bne @pjc_tophalf
@pjc_bottomhalf:
    pla             ; Pull the line offset from the stack into A
    clc
    adc PosX        ; Add the X coordinate to it
    tay             ; Y now holds the final, correct screen offset for painting

    pla             ; Pull the color from the stack into A
    sta MEMCLR+256,y; Paint the color in the bottom half of the screen
    jmp @pjc_exit
@pjc_tophalf:
    pla             ; Pull the line offset from the stack into A
    clc
    adc PosX        ; Add the X coordinate to it
    tay             ; Y now holds the final, correct screen offset for painting

    pla             ; Pull the color from the stack into A
    sta MEMCLR,y    ; Paint the color in the top half of the screen
@pjc_exit:
    ldy PosY        ; Restore the original Y register from your temp variable
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
	size=16*23/4+1
	ldx #size
@loop:
	sta MEMSCR-1,X          ; A (small) degree of loop unrolling avoids
    sta MEMSCR+size-1,X     ; having to mess with a 16-bit loop.
    sta MEMSCR+size*2-1,X
    sta MEMSCR+size*3-1,X
    dex
    bne @loop
    rts
			
; A non-blocking routine to check for an exit key or joystick fire.
CheckForExit:
    jsr GETIN         ; KERNAL routine to get a (keyboard) char from the buffer
    cmp #0            ; Is the buffer empty?
    beq @check_joy    ; If so, branch to check the joystick

    ; A key was pressed. Check if it's 'M' for mute.
    cmp #$4D         
    bne @exit_now     ; If it's NOT 'M', it's an exit key.

    ; It was 'M', so toggle the mute flag. We need a short delay
    ; to prevent the keypress from toggling hundreds of times.
    lda MusicMuted
    eor #1            ; Flip the bit (0 to 1, or 1 to 0)
    sta MusicMuted
    
    ; Add a small pause to "debounce" the mute key
    ldx #4            ; Outer loop
@debounce_loop:
    jsr WaitForNextIRQ; Wait for one screen refresh
    dex
    bne @debounce_loop
    jmp @no_exit      ; After handling mute, continue the intro loop
@check_joy:
    lda PORTAVIA1     ; Read joystick port
    and #32           ; Is the fire button bit set?
    cmp #32
    beq @no_exit      ; If not pressed, continue intro loop
@exit_now:
    jmp PrepareChainload
@no_exit:
    rts
	
UpdateScoreAnimation:
    ldx ScoreAnimState
    cpx #SCORE_STATE_IDLE
    bne @not_idle       ; If NOT idle, branch past the jmp
    jmp @exit           ; It was idle, so take the long jump to the exit
@not_idle:
    cpx #SCORE_STATE_PRINTING
    beq @print_one_char ; If printing, jump to the print logic.
@setup_print_job:
    ldx ScoreAnimState
    cpx #SCORE_STATE_DRAW_LEGS
    bne @check_body
    lda #<LegScoreDesc
    ldx #>LegScoreDesc
    sta ScorePrintPtr
    stx ScorePrintPtr+1
    lda #10
    sta ScorePrintX
    lda #13
    sta ScorePrintY
    jmp @finish_setup
@check_body:
    cpx #SCORE_STATE_DRAW_BODY
    bne @check_arms
    lda #<BodyScoreDesc
    ldx #>BodyScoreDesc
    sta ScorePrintPtr
    stx ScorePrintPtr+1
    lda #9
    sta ScorePrintX
    lda #12
    sta ScorePrintY
    jmp @finish_setup
@check_arms:
    cpx #SCORE_STATE_DRAW_ARMS
    bne @check_head
    lda #<ArmScoreDesc
    ldx #>ArmScoreDesc
    sta ScorePrintPtr
    stx ScorePrintPtr+1
    lda #1
    sta ScorePrintX
    lda #12
    sta ScorePrintY
    jmp @finish_setup
@check_head:
    cpx #SCORE_STATE_DRAW_HEAD
    bne @exit
    lda #<HeadScoreDesc
    ldx #>HeadScoreDesc
    sta ScorePrintPtr
    stx ScorePrintPtr+1
    lda #8
    sta ScorePrintX
    lda #11
    sta ScorePrintY
@finish_setup:
    lda #0
    sta ScorePrintIndex
    lda #SCORE_STATE_PRINTING
    sta ScoreAnimState
    jmp @exit
@print_one_char:
    ldy ScorePrintIndex
    lda (ScorePrintPtr),y      ; Get the character from the string
    beq @printing_finished     ; If it's 0 (null terminator), we're done.
    pha                        ; Save the character on the stack

    ; Set the color (this uses and is finished with the A register)
    lda #GREEN
    sta Colour

    lda ScorePrintX            ; Load the string's starting X coordinate
    clc
    adc ScorePrintIndex        ; Add the character's index to it
    tax                        ; Put final X position into the X register

    ldy ScorePrintY

    ; NOW, restore the character into A right before the call
    pla                        ; Pull the character from the stack into A
    jsr DrawChar               ; Call DrawChar with character in A, pos in X/Y

    inc ScorePrintIndex        ; Move to the next character for the next IRQ
    jmp @exit
@printing_finished:
    lda #SCORE_STATE_IDLE
    sta ScoreAnimState         
@exit:
    rts

UpdateRobotAnimation:
    ; First, check if our delay timer has finished.
    lda RobotAnimTimer
    bne @robot_decr_timer ; If not zero, just decrement and exit.
    
    ; Timer is zero, so perform one animation step.
    lda RobotAnimState
    cmp #ROBOT_STATE_BUILDING
    beq @robot_build_step
    cmp #ROBOT_STATE_FLASHING
    beq @robot_flash_step
    jmp @robot_exit 
@robot_build_step:
    ; Reset the delay timer for the next step.
    lda #15 ; Set pause between build steps
    sta RobotAnimTimer

    ; Get the current part index from the counter
    ldx RobotAnimCounter
    
    ; Check if we are done building
    cpx #6
    bcs @robot_start_flashing
    
    ; Draw the next part and trigger the score 
	; animation based on which part is being drawn
    cpx #1 ; After second leg (index 1)
    bne @not_legs
    lda #SCORE_STATE_DRAW_LEGS
    sta ScoreAnimState
@not_legs:
    cpx #2 ; After torso (index 2)
    bne @not_body
    lda #SCORE_STATE_DRAW_BODY
    sta ScoreAnimState
@not_body:
    cpx #4 ; After second arm (index 4)
    bne @not_arms
    lda #SCORE_STATE_DRAW_ARMS
    sta ScoreAnimState
@not_arms:
    cpx #5 ; After head (index 5)
    bne @not_head
    lda #SCORE_STATE_DRAW_HEAD
    sta ScoreAnimState
@not_head:
    lda #BLUE
    sta Colour
    jsr DrawSinglePartFromTable
    inc RobotAnimCounter ; Move to the next part for the next tick
    jmp @robot_exit
@robot_start_flashing:
    ; Robot is fully built, switch to the flashing state
    lda #ROBOT_STATE_FLASHING
    sta RobotAnimState
    lda #12 ; Set flash count (6 red, 6 black)
    sta RobotAnimCounter
    jmp @robot_exit
@robot_flash_step:
    ; Reset the delay timer for the next flash
    lda #8
    sta RobotAnimTimer
    
    ; Check if flashing is done
    ldx RobotAnimCounter
    beq @robot_restart
    
    ; Decide color based on whether the counter is even or odd
    txa
    lsr a
    bcc @robot_flash_black
@robot_flash_red:
    lda #RED
    jmp @robot_do_flash
@robot_flash_black:
    lda #BLACK
@robot_do_flash:
    jsr DrawOrEraseRobot
    dec RobotAnimCounter ; Decrement flash counter
    jmp @robot_exit
@robot_restart:
    ; Flashing is done. Erase and reset to start building again.
    jsr EraseAnimatedContent
    lda #ROBOT_STATE_BUILDING
    sta RobotAnimState
    lda #0
    sta RobotAnimCounter
    jmp @robot_exit
@robot_decr_timer:
    dec RobotAnimTimer
@robot_exit:
    rts

UpdateTentacleAnimation:
    ; First, check if our delay timer has finished.
    lda TentacleAnimTimer
    beq @tentacle_run_step
    dec TentacleAnimTimer
    rts
@tentacle_run_step:
    ; Timer is zero, so perform one step.
    lda #5 ; Reset delay timer
    sta TentacleAnimTimer

    lda TentacleAnimState
    cmp #TENTACLE_STATE_EXTEND
    beq @tentacle_extend_step
    cmp #TENTACLE_STATE_RETRACT
    beq @tentacle_retract_step
    jmp @tentacle_exit
@tentacle_extend_step:
    ldx TentacleAnimCounter
    cpx #6 ; Check if fully extended
    bcs @tentacle_start_retracting
    
    lda #MAGENTA
    sta Colour
    stx tmp4
    ; Calculate the offset into the data table (index * 3).
    txa
    asl a
    adc tmp4
    tay

    ; Load the character from the data table into A.
    lda TentacleDrawData+2,y
    
    ; Restore the segment index into X.
    ldx tmp4

    ; Call the drawing routine. A holds the char, X holds the index.
    jsr DrawOrEraseSegment

    inc TentacleAnimCounter
    jmp @tentacle_exit
@tentacle_start_retracting:
    lda #TENTACLE_STATE_RETRACT
    sta TentacleAnimState
    lda #5
    sta TentacleAnimCounter
    jmp @tentacle_exit
@tentacle_retract_step:
    ldx TentacleAnimCounter
    bmi @tentacle_start_extending
    lda #BLACK
    sta Colour
    lda #EMPTY
    jsr DrawOrEraseSegment

    dec TentacleAnimCounter
    jmp @tentacle_exit
@tentacle_start_extending:
    lda #TENTACLE_STATE_EXTEND
    sta TentacleAnimState
    lda #0
    sta TentacleAnimCounter
    jmp @tentacle_exit
@tentacle_exit:
    rts

DrawSinglePartFromTable:
    stx tmp4 ; Save index
    txa
    asl a
    adc tmp4
    tay
    lda RobotPartData,y
    sta PosX
    lda RobotPartData+1,y
    sta PosY
    lda RobotPartData+2,y
    ldx PosX
    ldy PosY
    jsr DrawChar
    ldx tmp4 ; Restore index
    rts

DrawOrEraseRobot:
    sta Colour
    txa
    pha
    ldx #5 ; Loop through all 6 robot parts (index 5 down to 0)
@part_loop:
    stx tmp4 ; Save part index

    ; Calculate offset into RobotPartData (index * 3)
    txa
    asl a       ; A = index * 2
    adc tmp4    ; A = (index * 2) + index = index * 3
    tay         ; Y is now the byte offset into the table

    ; Load part's X and Y coordinates from the table
    lda RobotPartData,y
    sta PosX
    lda RobotPartData+1,y
    sta PosY

    ; Decide whether to draw the part or an empty character
    lda Colour
    cmp #BLACK
    bne @draw_part
    lda #EMPTY
    jmp @do_the_draw
@draw_part:
    lda RobotPartData+2,y ; Load the character code for this part
@do_the_draw:
    ldx PosX
    ldy PosY
    jsr DrawChar
    
    ldx tmp4 ; Restore part index
    dex
    bpl @part_loop
    pla
    tax
    rts

; Erase all animated content before the loop restarts
EraseAnimatedContent:
    lda #BLACK
    jsr DrawOrEraseRobot

    lda #<BlankScoreLine
    sta LAB_01
    lda #>BlankScoreLine
    sta LAB_02
    lda #BLACK
    sta Colour

    ; Erase each line where score text appeared
    ldx #10
    ldy #13
    jsr PrintStr
    ldx #9
    ldy #12
    jsr PrintStr
    ldx #1
    ldy #12
    jsr PrintStr
    ldx #8
    ldy #11
    jsr PrintStr
    rts

; Draw or erase one segment of the tentacle.
; IN: X = segment index (0-5), A = character to draw (TENTACLE1 or EMPTY)
DrawOrEraseSegment:
    sta CharCode ; Save the character we want to draw
    stx tmp4     ; Save the segment index

    ; Calculate the offset into TentacleDrawData (index * 3)
    txa
    asl a       ; A = index * 2
    adc tmp4    ; A = (index * 2) + index = index * 3
    tay         ; Y is now the byte offset

    ; Get the segment's coordinates from the table
    lda TentacleDrawData,y
    sta PosX
    lda TentacleDrawData+1,y
    sta PosY

    ; Draw the character at the specified location
    lda CharCode
    ldx PosX
    ldy PosY
    jsr DrawChar

    ldx tmp4     ; Restore segment index
    rts

EraseTentacle:
    ldx #5
@erase_loop:
    lda #EMPTY
    jsr DrawOrEraseSegment
    dex
    bpl @erase_loop
    rts

LoaderStub:
    ; We are running from safe memory. Interrupts are DISABLED.
    lda #<$EABF
    sta $0314
    lda #>$EABF
    sta $0315

    lda #$07
    sta PORTAVIA1d      ; PORTAVIA1d = $9113

    lda #8              ; Logical file number
    ldx #8              ; Device #8 = disk drive
    ldy #0              ; Secondary address 0 for read
    jsr $FFBA           ; KERNAL SETLFS

    ldx #NextFileNameLen
    lda #<($0300 + (StubFileName - LoaderStub))
    sta $B8
    lda #>($0300 + (StubFileName - LoaderStub))
    sta $B9
    jsr $FFBD           ; KERNAL SETNAM
    jsr $FFC0           ; KERNAL OPEN

    cli

    jsr $FFCF           ; Read low byte
    jsr $FFCF           ; Read high byte

    lda #<$8000
    sta $1A
    lda #>$8000
    sta $1B
@ReadLoop:
    jsr $FFCF
    pha
    lda $90
    and #%01000000
    bne @LoadDone
    
    pla
    ldy #0
    sta ($1A),y
    
    inc $1A
    bne @ReadLoop
    inc $1B
    jmp @ReadLoop
@LoadDone:
    pla
    lda #8
    jsr $FFC3
    jmp $8000

LoadCommand:
    .byte "LOAD", $22, "ROBOFRENZY", $22, ",8", $0D, 0
StubFileName:
    .byte "ROBOFRENZY",0
NextFileNameLen = *-StubFileName - 1
LoaderStubEnd:

; Data and configuration settings/tables.

; Music data. Much is loop-based, to reduce mem occupation.
; The code for a loop is as follows:
; 1 byte: 10xx xxxx where the xxx xxx represent the number of times the loop
;   should be repeated
; Special code: 1111 1111 repeat from start
; The code for a note is as follows:
;   01 zz zzzz where zz zzzz represents the distance in semitones from C
;   01 11 1111 is a silence
; Special codes for note durations:
;   00 ss ssss specify that the following notes should have the given
;      duration in 1/60's of seconds
;   it should be followed by a byte giving the duration of the silence in
;   the note
notecode = %10000000
silence  = %11111110
duracode = %00000000
repeatm  = %11111111
maskcode = %10000000
unmask   = %01111111

quaver = 31
quaverd = 20

semiquaver = 15
semiquaverd = 10

do0=128
dod0=134
re0=141
red0=147
mi0=153
fa0=159
fad0=164
sol0=170
sold0=174
la0=179
lad0=183
si0=187
do1=191
dod1=195
re1=198
red1=201
mi1=204
fa1=207
fad1=210
sol1=213
sold1=215
la1=217
lad1=219
si1=221
do2=223
dod2=225
re2=227
red2=229
mi2=230
fa2=231
fad2=232        ; Quite out of tune on higher pitches
sol2=234        ; ...
sold2=235       ;
la2=236
lad2=237
si2=238
do3=239
dod3=240

; Music data for J.S. Bach, Fantasia of Partita 3, BWV 827
Voice1data: 
	; Measures 1 - 6
   .byte duracode + semiquaver, semiquaverd
   .byte silence,la1,si1,sold1,la1,do2,re1,si1,mi1,re1,do1,si0
   .byte duracode + quaver, quaverd
   .byte do1,mi1,la1,la1,sold1,si1
   .byte duracode + semiquaver, semiquaverd
   .byte si1,la1,sold1,la1,si1,do2,re2,fa2,si1,fa2,mi2,re2

   ; Measures 7 - 12
   .byte duracode + quaver, quaverd
   .byte do2
   .byte duracode + semiquaver, semiquaverd
   .byte si1, la1
   .byte duracode + quaver, quaverd
   .byte fa2,fa2
   .byte duracode + semiquaver, semiquaverd
   .byte mi2,re2
   .byte duracode + quaver, quaverd
   .byte sol2
   .byte duracode + semiquaver, semiquaverd
   .byte mi2,do2,re2,si1,do2,mi2
   .byte sol2,si1,do2,la1,si1,re2
   .byte sol2,la1,si1,sold1,la1,do2
   .byte duracode + quaver, quaverd
   .byte fa2,re2,sol1

   ; Measures 13 - 18
   .byte mi2,do2,fa1
   .byte duracode + semiquaver, semiquaverd
   .byte re2,mi1,fa1,re1,mi1,sold1
   .byte duracode + quaver, quaverd
   .byte do2,si1,la1
   .byte duracode + semiquaver, semiquaverd
   .byte sold1,la1,si1,fa1,mi1,re1
   .byte do1,mi1,fa1,re1,mi1,la1
   .byte si1,mi1,fa1,re1,mi1,si1

   ; Measures 19-24
   .byte do2,mi1,fa1,re1,mi1,do2
   .byte si1,la1,sold1,si1
   .byte duracode +quaver, quaverd, mi1
   .byte duracode + semiquaver, semiquaverd
   .byte silence,mi2,fa2,re2,mi2,do2
   .byte si1,mi2,fa2,re2,mi2,si1
   .byte la1,mi2,fa2,re2,mi2,la1
   .byte sol1,si1,mi1,do2,re2,si1

   ; Measures 25-30
   .byte do2,mi2,la1,sold1,la1,do2
   .byte fad1,la1,re1,si1,do2,la1
   .byte si1,re2,sol1,fad1,sol1,si1
   .byte mi1,sol1,do1,la1,si1,sol1
   .byte la1,do2,fad1,mi1,fad1,la1
   .byte red1,fad1,si0,sol1,la1,fad1

   ; Measures 31-36
   .byte duracode +quaver, quaverd
   .byte sol1,si1,mi2,mi2,red2,fad2
   .byte duracode +semiquaver, semiquaverd
   .byte fad2,mi2,fad2,red2,mi2,sol2
   .byte la1,fad2,si1,la1,sol1,fad1
   .byte duracode +quaver, quaverd
   .byte sol1
   .byte duracode +semiquaver, semiquaverd
   .byte fad1,mi1
   .byte duracode +quaver, quaverd
   .byte do2
   .byte do2
   .byte duracode +semiquaver, semiquaverd
   .byte si1,la1
   .byte duracode +quaver, quaverd
   .byte re2

   ; Measures 37 - 42
   .byte duracode +semiquaver, semiquaverd
   .byte si1,do2,re2,mi2,fad2,sol2
   .byte la2,do3,fad2,do3,si2,la2
   .byte duracode +quaver, quaverd
   .byte sol2, si2,mi2,do2,la2,re2
   .byte si1,sol2,do2
   .byte repeatm

Voice2data: 
	; Measures 1 - 6
    .byte duracode +quaver, quaverd
    .byte la1,179,la1,la1,sold1,mi1
    .byte duracode+semiquaver,semiquaverd
    .byte silence, la1,si1,sold1,la1,do2,re1,si1,mi1,re1,do1,si0
    .byte duracode +quaver, quaverd
    .byte do1,mi1,la1,la1,sold1,si1
    ; Measures 7 - 12
    .byte duracode+semiquaver,semiquaverd
    .byte si1,la1,sold1,la1,si1,do2
    .byte re2,fa2,si1,fa2,mi2,re2
    .byte duracode +quaver, quaverd
    .byte do2,mi2,la1
    .byte fa2,re2,sol1
    .byte mi2,do2,207
    .byte duracode+semiquaver,semiquaverd
    .byte re2,si1,do2,la1,si1,re2

    ; Measures 13-18
    .byte sol2,la1,si1,sol1,la1,do2
    .byte fa2, sold1,la1,fad1,sold1,si1
    .byte mi2,la1,re2,mi1,fa1,re1
    .byte duracode +quaver, quaverd
    .byte mi1,sold1,mi1
    .byte la1,la0,la1
    .byte duracode+semiquaver,semiquaverd
    .byte la1,sold1,la1,fad1,sold1,mi1

    ; Measures 19 - 24
    .byte la1,sold1,la1,si1,do2,re2
    .byte mi2,fa2,mi2,re2,do2,si1
    .byte duracode +quaver, quaverd
    .byte la1,la0,la1
    .byte sol1,sol0,sol1
    .byte fa1,fa0,fa1
    .byte mi1,sold0,mi0

    ; Measures 25 - 30
    .byte la0,si0,do1
    .byte re1,fad1,re1
    .byte sol1,la1,si1
    .byte do2,mi1,do1
    .byte fad1,sol1,la1
    .byte si1,red1,si0

    ; Measures 31 - 36
    .byte duracode +semiquaver, semiquaverd
    .byte mi1,mi2,fad2,red2,mi2,sol2
    .byte la1,fad2,si1,la1,sol1,fad1
    .byte duracode +quaver, quaverd
    .byte sol1,si0,mi1
    .byte mi1,red1,fad1
    .byte duracode +semiquaver, semiquaverd
    .byte fad1,mi1,red1,mi1,fad1,sol1
    .byte la1,do2,fad1,do2,si1,la1

    ; Measures 37 - 42
    .byte duracode +quaver, quaverd
    .byte sol1
    .byte duracode +semiquaver, semiquaverd
    .byte fad1,mi1
    .byte duracode +quaver, quaverd
    .byte do2,do2
    .byte duracode +semiquaver, semiquaverd
    .byte si1,la1
    .byte duracode +quaver, quaverd
    .byte re2
    .byte duracode +semiquaver, semiquaverd
    .byte si1,sol1,la1,fad1,sol1,si1
    .byte mi2,fad1,sol1,mi1,fad1,la1
    .byte re2,mi1,fad1,re1,mi1,sol1
    .byte duracode +quaver, quaverd
    .byte do2,la1,re1
    .byte la1,la1,la1,la1

; Building off Davide's intro, using the same character printing rather than raw hex offsets.
TitleGame:  .byte (' '),(' '),(' '),(' '),(' '),(' '),('R'-'@'),('O'-'@'),('B'-'@'),('O'-'@'),(45+$80),('F'-'@'),('R'-'@'),('E'-'@'),('N'-'@'),('Z'-'@'),('Y'-'@'),'!',0

FAOGames: .byte (' '),(' '),('F'-'@'),('O'-'@'),('R'-'@'),(' '),('A'-'@'),('M'-'@'),('U'-'@'),('S'-'@'),('E'-'@'),('M'-'@'),('E'-'@'),('N'-'@'),('T'-'@'),(' '),0 

FAOGames2: .byte (' '),('O'-'@'),('N'-'@'),('L'-'@'),('Y'-'@'),(' '),('G'-'@'),('A'-'@'),('M'-'@'),('E'-'@'),('S'-'@'),(','),('L'-'@'),('L'-'@'),('C'-'@'),('.'), 0

Presents: .byte (' '),(' '),(' '),(' '),('P'-'@'),('R'-'@'),('E'-'@'),('S'-'@'),('E'-'@'),('N'-'@'),('T'-'@'),('S'-'@'), 0

Scoring: .byte (' '),(' '),(' '),(' '),('H'-'@'),('O'-'@'),('W'-'@'),(' '),('T'-'@'),('O'-'@'),(' '),('P'-'@'),('L'-'@'),('A'-'@'),('Y'-'@'),(':'), 0

LegScoreDesc: .byte '1','0','0', ('P'-'@'),('T'-'@'),0
BodyScoreDesc: .byte '2','0','0', ('P'-'@'),('T'-'@'),0
ArmScoreDesc: .byte '1','0','0', ('P'-'@'),('T'-'@'),0
HeadScoreDesc: .byte '5','0','0', ('P'-'@'),('T'-'@'),0
PlayerDisplay: .byte PLAYER,(61+$80),('Y'-'@'),('O'-'@'),('U'-'@'),0

GameDesc: .byte ('B'-'@'),('R'-'@'),('I'-'@'),('N'-'@'),('G'-'@')," ",GEAR,0
GameDesc2: .byte ('T'-'@'),('O'-'@')," ",('B'-'@'),('U'-'@'),('I'-'@'),('L'-'@'),('D'-'@')," ",('R'-'@'),('O'-'@'),('B'-'@'),('O'-'@'),('T'-'@'),('S'-'@'),0
GameDesc3: .byte ('A'-'@'),('V'-'@'),('O'-'@'),('I'-'@'),('D'-'@')," ",('T'-'@'),('E'-'@'),('N'-'@'),('T'-'@'),('A'-'@'),('C'-'@'),('L'-'@'),('E'-'@'),('S'-'@'),0
             
Keys: .byte 27+128,('Z'-'@'),29+128, " ",  ('L'-'@'),('E'-'@'),('F'-'@'),('T'-'@'),0
Keys2: .byte 27+128,('X'-'@'),29+128, " ",  ('R'-'@'),('I'-'@'),('G'-'@'),('H'-'@'),('T'-'@'),0
Keys3: .byte ('O'-'@'),('R'-'@')," ",('J'-'@'),('O'-'@'),('Y'-'@'),('S'-'@'),('T'-'@'),('I'-'@'),('C'-'@'),('K'-'@'),0
Keys4: .byte 27+128,('M'-'@'), 29+128," ", ('T'-'@'),('O'-'@'),('G'-'@'),('G'-'@'),('L'-'@'),('E'-'@'), " ", ('M'-'@'),('U'-'@'),('S'-'@'),('I'-'@'),('C'-'@'),0
Keys5: .byte 27+128,('R'-'@'),('E'-'@'),('T'-'@'),('U'-'@'),('R'-'@'),('N'-'@'),29+128," ",('O'-'@'),('R'-'@'),0
Keys6: .byte 27+128,('S'-'@'),('P'-'@'),('A'-'@'),('C'-'@'),('E'-'@'),29+128," ",('R'-'@'),('E'-'@'),('S'-'@'),('T'-'@'),('A'-'@'),('R'-'@'),('T'-'@'),0

BlankScoreLine: .byte "       ", 0 ; 7 spaces to cover point values during animation.

TentacleDrawData:
    ; Format is: X-coordinate, Y-coordinate, Character Code
    .byte 13, 17, TENTACLE3
    .byte 12, 16, 30
    .byte 11, 16, 0
    .byte 10, 16, 0
    .byte 9, 16, 0
    .byte 8, 16, 29
    .byte 7, 16, 29

RobotPartData:
    ; Format: X-coord, Y-coord, Character Code    
    .byte 6, 13, LLEG
    .byte 8, 13, RLEG
    .byte 7, 12, TORSO
    .byte 8, 12, RARM
    .byte 6, 12, LARM
    .byte 7, 11, HEAD

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

            
