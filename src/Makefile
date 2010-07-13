
OPTIONS=                     \
  -fglasgow-exts             \
  -XDisambiguateRecordFields \
  -XRecordWildCards          \
  -XTupleSections

.PHONY: all clean

all: kdwm

kdwm: *.hs */*.hs
	ghc $(OPTIONS) -o kdwm --make Main.hs

clean:
	-rm */*.hi */*.o *.hi *.o kdwm

