;         V I C   A L I E N S - I N V A D E R S
;
;       by Davide Bucci, April 2018
;
; This program is a Space-Invaders clone that runs on an unexpanded VIC-20
; A certain amount of work has been done to ensure that the graphics is smooth
; enough so that the effect hopefully appears to be quite polished.
; It is my first attempt to write a complete game in 6502 assembly language,
; even if I learnt that language many years ago and I used it mainly as a
; complement for BASIC programs.
;
; The assembler used is ca65

; General-use addresses

        GRCHARS1 = $1C00     ; Address of user-defined characters


; VIC-chip addresses

        VICSCRHO = $9000    ; Horisontal position of the screen
        VICSCRVE = $9001    ; Vertical position of the screen
        VICCOLNC = $9002    ; Screen width in columns and video memory addr.
        VICROWNC = $9003    ; Screen height, 8x8 or 8x16 chars, scan line addr.
        VICCHGEN = $9005    ; Character gen. and video matrix addresses.
        VICCOLOR = $900F    ; Screen and border colours

        MEMSCR   = $1E00    ; Start address of the screen memory (unexp. VIC)
        MEMCLR   = $9600    ; Start address of the colour memory (unexp. VIC)

.export main
.segment "STARTUP"
.segment "LOWCODE"
.segment "INIT"
.segment "GRCHARS"
.segment "CODE"

; Main program here.

main:
            jsr Init
            lda #34
            sta AlienPosY   ; Initial position of aliens
            jsr DrawAliens
            rts

; INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT
;
; Initialization code: prepare the screen to the correct size, center it and
; load the graphic chars and configure the IRQ handler.
;
; INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT

Init:
            lda #$09        ; Define screen colour and background
            sta VICCOLOR
            lda #$90        ; Set a 16 column-wide screen
            sta VICCOLNC
            lda #$BE        ; Set a 31 row-high column
            sta VICROWNC
            lda #$16        ; Center the screen vertically...
            sta VICSCRVE
            lda #$12        ; ... and horisontally
            sta VICSCRHO
            lda #$FF        ; Move the character generator address to $1C00
            sta VICCHGEN    ; while leaving ch. 128-255 to their original pos.
            jsr MovCh       ; Load the graphic chars
            jsr CLS

            sei             ; Configure the interrupt handler
            lda #<IrqHandler
            sta $0314
            lda #>IrqHandler
            sta $0315
            cli

            rts
; Copy the graphic chars. They are subjected to be changed during the pixel-by
; pixel movement, so that routine gives only the initial situation.
MovCh:
            ldx #0
@loop:
            lda DefChars,x
            sta GRCHARS1,x
            inx
            cpx #5*8
            bne @loop
            rts

; IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ
;
; This is the interrupt handler, called 60 times each second when the VIC-20
; is working normally. It does the following things:
;
; 1 - Calculate positions of the aliens and, if necessary, redraw them
; 2 - Calculate positions of the falling bombs and fire shoots and draw them
; 3 - Update the position of the cannon and draw it
; 5 - Check for collisions and handle explosions
; 6 - Jump to the original IRQ handler (for scanning the keyboard, etc).
;
; The user interface is handled outside of the interrupt, in the main program
; loop and the communication with the IRQ handler is made by a set of
; appropriate flags. This approach has the following advantages:
;
; - The speed of the aliens and of the cannon is controlled very precisely as
;   the IRQ handler is called at a predictable and stable rate.
; - The code for the visualization and for the user interface (i.e. recognizing
;   the joystick movements) is kept separate.
;
; The main drawback is that the IRQ is supposed to do many things and it should
; be doing that VERY RAPIDLY in order not to mess with the calling order. As a
; rule of thumb, I would say that the IRQ should be completed in less than 5 ms
; at least most of times.
;
; IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ

IrqHandler:
            pha
            txa             ; Save registers
            pha
            tya
            pha

            lda IrqCn
            cmp #30         ; Increment the Y position of the aliens 
            bne @cont
            lda #0
            sta IrqCn
            inc AlienPosY
            lda AlienPosY
            cmp #25*8
            bne @draw
            lda #0
            sta AlienPosY
@draw:      jsr CLS
            jsr DrawAliens
@cont:
            inc IrqCn
            pla             ; Retrive registers
            tay
            pla
            tax
            pla

            jmp $EABF       ; Jump to the standard IRQ handling routine

; Draw the aliens on the screen. They are several lines with at most 8 aliens
; each. The presence of an alien in the first row is given by bits in the
; AliensR1 byte. An alien is present at the beginning of the game (or level)
; and can be destroyed when hit. In this case, the corresponding bit in the
; AliensR1 byte is set to 0. Same for AliensR2 and AliensR3.

DrawAliens:
            ldx #$10
            lda AlienPosY      ; The position is in pixel, divide by 8
            lsr                ; to obtain position in character
            lsr
            lsr
            sta AlienCurrY
@loop1:     dex
            ldy AlienCurrY
            lda AliensR1
            rol
            sta AliensR1
            bcs @drawAlien0
@ret1:      dex
            bne @loop1
            inc AlienCurrY
            inc AlienCurrY
            ldx #$10
@loop2:     dex
            ldy AlienCurrY
            lda AliensR2
            rol
            sta AliensR1
            bcs @drawAlien1
@ret2:      dex
            bne @loop2
            inc AlienCurrY
            rts

@drawAlien0:
            lda #2
            sta Colour
            lda #0
            jsr DrawChar
            jmp @ret1
@drawAlien1:
            lda #3
            sta Colour
            lda #1
            jsr DrawChar
            jmp @ret2

; Draw the character in A in the position given by the X and Y registers
; Since the screen is 16 characters wide, we need to shift the Y register
; to multiply times 16 and then add the X contents. Employs Dummy1 and Dummy2
; and Colour that indicates the color of the character.

DrawChar:
            sta Dummy1
            stx Dummy2
            tya
            asl             ; 16 columns per line
            asl
            asl
            asl
            clc
            adc Dummy2
            tay
            lda Dummy1
            sta MEMSCR,Y
            lda Colour
            sta MEMCLR,Y
            rts

; Clear the screen. This maybe is too slow to be used in an interrupt handler.
CLS:
            size=16*31/4
            ldx #size
@loop:      lda #5
            sta MEMSCR,X            ; A (small) degree of loop unrolling avoids
            sta MEMSCR+size,X       ; to mess with a 16-bit loop.
            sta MEMSCR+size*2,X
            sta MEMSCR+size*3,X
            lda #0
            sta MEMCLR,X
            sta MEMCLR+size,X
            sta MEMCLR+size*2,X
            sta MEMCLR+size*3,X
            dex
            bne @loop
            rts

; DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA
;
; Data and configuration settings.
;
; DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA

Dummy1:     .byte $00
Dummy2:     .byte $00
IrqCn:      .byte $00

Colour:     .byte $00           ; Colour to be used by the printing routines
AliensR1:   .byte $FF           ; Presence of aliens in row 1
AliensR2:   .byte $FF           ; Same for row 2
AliensR3:   .byte $FF           ; Same for row 3
AlienPosX:  .byte $00           ; Horisontal position of aliens (in pixels)
AlienPosY:  .byte $00           ; Vertical position of aliens (in pixels)
AlienCurrY: .byte $00           ; Vertical position of alien being drawn
CannonPos:  .byte $64           ; Horisontal position of the cannon (in pixels)

DefChars:
            .byte %00111100     ; Alien #1, associated to ch. 0 (normally @)
            .byte %01111110
            .byte %11011011
            .byte %11111111
            .byte %01100110
            .byte %00111100
            .byte %01000010
            .byte %10000001

            .byte %00111100     ; Alien #2, associated to ch. 1 (normally A)
            .byte %01111110
            .byte %11011011
            .byte %11111111
            .byte %01100110
            .byte %00111100
            .byte %01100110
            .byte %00000000
            
            .byte %10000001     ; Alien #3, associated to ch. 2 (normally B)
            .byte %01111110
            .byte %11011011
            .byte %11111111
            .byte %01100110
            .byte %00111100
            .byte %11000011
            .byte %00100100

            .byte %10000001     ; Alien #4, associated to ch. 3 (normally C)
            .byte %01111110
            .byte %11011011
            .byte %11111111
            .byte %01100110
            .byte %00111100
            .byte %01000010
            .byte %10000001

            .byte %00010000     ; Cannon, associated to ch. 4 (normally D)
            .byte %00111000
            .byte %00111000
            .byte %01111100
            .byte %01111100
            .byte %01111100
            .byte %11101110
            .byte %11000110

            .byte $0            ; Blank char, ch. 5 (E)
            .byte $0
            .byte $0
            .byte $0
            .byte $0
            .byte $0
            .byte $0
            .byte $0
            
            