// Copyright (C) by Vesa Karvonen

namespace PPrint

open System

[<AutoOpen>]
module Console =
  let println maxCols doc =
    outputWithFun Console.Write maxCols doc
    Console.Write "\n"
