all:
	cl65 robofrenzy-cassette.asm -t vic20 -C vic20robofrenzy.cfg -o game.bin
	cl65 robo-frenzy-intro-cassette.asm -t vic20 -C vic20robofrenzy.cfg -o intro.bin
	cl65 -t none -C vic20_cart.cfg robofrenzy-cartridge.asm -o robofrenzy.bin
