#!/bin/bash -e

SOLUTION=PPrint.sln

if hash xbuild &> /dev/null ; then
  BUILD=xbuild
elif hash msbuild.exe &> /dev/null ; then
  BUILD=msbuild.exe
else
  echo "Couldn't find build command."
  exit 1
fi

if hash paket &> /dev/null ; then
  PAKET=paket
else
  PAKET=.paket/paket.exe
fi

function build() {
  $BUILD /nologo /verbosity:quiet /p:Configuration=$2 $1
  mono Tests/Examples/bin/$2/Examples.exe
}

for config in Debug Release ; do
  build $SOLUTION $config
done

for template in *.paket.template ; do
  $PAKET pack output . templatefile PPrint.paket.template
done
