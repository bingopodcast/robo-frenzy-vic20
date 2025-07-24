all:
	cl65 robo-frenzy.asm -t vic20 -C vic20robofrenzy.cfg -o robo-frenzy
	cl65 robo-frenzy-intro.asm -t vic20 -C vic20robofrenzy.cfg -o robo-frenzy-intro
	#cl65 -t vic20 -C vic-cart.cfg -o robofrenzy.rom cartheader.asm robo-frenzy-intro.asm robo-frenzy.asm
