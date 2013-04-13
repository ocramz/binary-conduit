#!/bin/bash
if [ ! -f 'binary-conduit.cabal' ]; then
   echo -n "Can't find binary-conduit.cabal; please run this script as"
   echo -n " ./scripts/run-coverage from within the binary-conduit source"
   echo " directory"
   exit 1
fi

#CABAL_DEV=$(which cabal-dev)

#${CABAL_DEV} configure --enable-tests --flags="coverage"
#${CABAL_DEV} build || exit 1
#${CABAL_DEV} test  || exit 1

ghc -fhpc test/Main.hs
rm Main.tix
./test/Main

#dist/build/2pc-tests/2pc-tests
#rm -f system-fileio_tests.tix
#cabal-dev/bin/system-fileio_tests $@

#EXCLUDES="\
  #--exclude=Main \
  #--exclude=FilesystemTests.Posix \
  #--exclude=FilesystemTests.Util \
  #--exclude=FilesystemTests.Windows
#"

hpc markup --srcdir=. Main.tix --destdir=hpc-markup > /dev/null
hpc report --srcdir=. Main.tix
