
.PHONY: all clean

all: argon.png keysymdef.h.markdown
	jekyll

argon.png: argon.svg
	svg2png argon.svg argon.png

%.h.markdown: %.h
	echo '---' > $*.h.markdown
	echo 'layout: default' >> $*.h.markdown
	echo 'title: ' $*.h >> $*.h.markdown
	echo '---' >> $*.h.markdown
	echo >> $*.h.markdown
	echo "<p><a href='$*.h'>Original source</a></p>" >> $*.h.markdown
	cpp2html < $*.h >> $*.h.markdown

clean:
	rm -rf argon.png *.h.markdown _site

