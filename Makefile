
.PHONY: all clean

all: argon.png
	jekyll

argon.png: argon.svg
	svg2png argon.svg argon.png

clean:
	rm -rf argon.png _site

