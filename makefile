all:
	cl65 robofrenzy-cassette.asm -t vic20 -C vic20robofrenzy.cfg -o game.bin
	cl65 robo-frenzy-intro-cassette.asm -t vic20 -C vic20robofrenzy.cfg -o intro.bin
	#rm robofrenzy.bin
	#rm robofrenzy-cartridge.o
	ca65 -v robofrenzy-cartridge.asm -o robofrenzy-cartridge.o
	ld65 -v -C vic20_cart.cfg --dbgfile robofrenzy.dbg -o robofrenzy.bin robofrenzy-cartridge.o

	#cl65 -v -t none -C vic20_cart.cfg robofrenzy-cartridge.asm -o robofrenzy.bin
