# vic20-robo-frenzy

ROBO-FRENZY!

By Nick Baldridge of For Amusement Only Games, LLC.
Adapted from the game "Alien Invaders" by Davide Bucci

This program is a version of the arcade game "Robo-Frenzy" that runs on an unexpanded VIC-20.
No additional hardware is needed to play.

Written in machine language.

[Requirements]
You will need cc65 to compile, and xvic may be used to emulate the resulting game.

[Compiling and Running Game]
Compile with "make".

Run game wth the following command:

Cassette version contains both an intro and the game itself.  Use the command xvic intro.bin / game.bin.  
Cartridge has only the game image, and will autostart when run from address $A000, or use the command:
xvic -memory none -cartA robofrenzy.bin.

[How To Play]
You may use a joystick or the keyboard.  [Z] or [left arrow] moves Left, [X] or [right arrow] moves Right.

[Fire] or [Return] to begin and move to next stage.

[M] mutes/unmutes background music.

[Goal of Game]
Gather gears at the bottom of the screen, and bring back to the top to build a robot a piece at a time.
Legs and Arms score [100] points.
Torso scores [200] points.
Head scores [500] points.

Avoid tentacles, or a held gear will be dropped and you will be momentarily stopped.

High score begins at 2500 - build as many robots as you can within 2 minutes' time!