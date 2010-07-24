
OPTIONS=                     \
  -fglasgow-exts             \
  -XDisambiguateRecordFields \
  -XRecordWildCards          \
  -XTupleSections            \
  -XTemplateHaskell          \
  -XEmptyDataDecls           \
  -XLiberalTypeSynonyms      \
  -XFlexibleContexts

WARNINGS=                  \
  -Werror                  \
  -Wall                    \
  -fno-warn-orphans        \
  -fno-warn-unused-imports \
  -fno-warn-unused-binds   \
  -fno-warn-unused-matches \
  -fno-warn-unused-do-bind \
  -fno-warn-name-shadowing

.PHONY: all clean

all: kdwm

kdwm: *.hs */*.hs
	ghc $(OPTIONS) $(WARNINGS) -o kdwm --make Main.hs

clean:
	-rm */*.hi */*.o *.hi *.o kdwm

