#!/bin/sh

cp -ru ../dist/build/argon/argon-tmp/* .

# Make sure that ghci loads the targets in interpreted mode
# so that breakpoints can be set.
#
# The sleep is necessary because otherwise the touch time
# might be the same as the build-completion time.
sleep 1
touch $*

exec ghci \
  -XDisambiguateRecordFields \
  -XRecordWildCards \
  -XTemplateHaskell \
  -XEmptyDataDecls \
  -XLiberalTypeSynonyms \
  -XFlexibleContexts \
  -XNoImplicitPrelude \
  -fglasgow-exts \
  -XTupleSections \
  -Wall \
  -fno-warn-orphans \
  -fno-warn-unused-imports \
  -fno-warn-unused-binds \
  -fno-warn-unused-matches \
  -fno-warn-unused-do-bind \
  -fno-warn-name-shadowing \
  -fno-warn-type-defaults

