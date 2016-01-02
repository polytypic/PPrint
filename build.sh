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

function run() {
  if hash mono &> /dev/null ; then
    mono "$1"
  else
    "$1"
  fi
}

function build() {
  $BUILD /nologo /verbosity:quiet /p:Configuration=$2 $1
}

for config in Debug Release ; do
  build $SOLUTION $config
  for exe in Tests/*/bin/$config/*.exe ; do
    run $exe
  done
done

for template in *.paket.template ; do
  $PAKET pack output . templatefile $template
done
