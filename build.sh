#!/bin/bash

set -e

nuget restore PPrint.sln -Verbosity quiet

function build () {
    xbuild /nologo /verbosity:quiet /p:Configuration=$2 $1
    mono Tests/Examples/bin/$2/Examples.exe
}

build PPrint.sln Debug
build PPrint.sln Release

paket pack output . templatefile PPrint.paket.template
