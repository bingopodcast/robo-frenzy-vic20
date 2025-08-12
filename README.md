# vic20-robo-frenzy

ROBO-FRENZY!

By Nick Baldridge of For Amusement Only Games, LLC.
Adapted from the game "Alien Invaders" by Davide Bucci

This program is a version of the arcade game "Robo-Frenzy" that runs on an unexpanded VIC-20.
No additional hardware is needed to play.

Written in machine language.

[Requirements]
You will need cc65 to compile, and xvic may be used to emulate the resulting game.

OR you may write to cassette or burn the resultant .bin to a cartridge ROM.

[Compiling and Running Game]
Compile with "make".  "make clean" removes all object files (.o files).

Cassette version contains both an intro and the game itself.  Use the command xvic intro.bin / game.bin.  
Cartridge has only the game image.  Boots to BASIC, and then launch using the command SYS 40999.

Use the following command-line arguments to emulate an unexpanded VIC-20 with a cartridge attached.
xvic -memory none -cartA robofrenzy.bin.

[How To Play]
You may use a joystick or the keyboard.  [Z] or [Joystick left] moves Left, [X] or [Joystick right] moves Right.

[Joystick Fire] or [Return] to begin and move to next stage.

[M] mutes/unmutes background music.  Sound effects are still played when music is muted.

[Goal of the Game]
Gather gears at the bottom of the screen, and bring back to the top to build a robot a piece at a time.
Legs and Arms score [100] points.
Torso scores [200] points.
Head scores [500] points.

Avoid tentacles, or a held gear will be dropped and you will be momentarily stopped.

High score begins at 2500 - build as many robots as you can within 2 minutes' time!

Cartridge box art by Ryan Claytor of Elephant Eater Comics (https://elephanteater.com)

Based on the arcade game Robo-Frenzy by Nick Baldridge and Ryan Claytor.  
Love Robo-Frenzy for the VIC-20?  Learn more about the arcade game at https://foramusementonlygames.com
