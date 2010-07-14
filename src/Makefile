
OPTIONS=                     \
  -fglasgow-exts             \
  -XDisambiguateRecordFields \
  -XRecordWildCards          \
  -XTupleSections

WARNINGS=           \
  -Werror           \
  -Wall             \
  -fno-warn-orphans \
  -fno-warn-unused-do-bind

.PHONY: all clean

all: kdwm

kdwm: *.hs */*.hs
	ghc $(OPTIONS) $(WARNINGS) -o kdwm --make Main.hs

clean:
	-rm */*.hi */*.o *.hi *.o kdwm

