
OPTIONS=-fglasgow-exts -XDisambiguateRecordFields -XRecordWildCards
.PHONY: all clean

all: kdwm

kdwm: *.hs */*.hs
	ghc -fglasgow-exts -o kdwm --make Main.hs

clean:
	-rm */*.hi */*.o *.hi *.o kdwm

