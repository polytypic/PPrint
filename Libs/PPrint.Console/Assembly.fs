// Copyright (C) by Vesa Karvonen

namespace PPrint

open System.Reflection
open System.Runtime.InteropServices

[<AutoOpen>]
module AssemblyInfo =
  [<Literal>]
  let Version = "1.0.0"

[<assembly: AssemblyTitle "PPrint.Console">]
[<assembly: AssemblyDescription "PPrint extensions for printing to console.">]
[<assembly: AssemblyConfiguration "">]
[<assembly: AssemblyCompany "">]
[<assembly: AssemblyProduct "PPrint.Console">]
[<assembly: AssemblyCopyright "© Vesa Karvonen">]
[<assembly: AssemblyTrademark "">]
[<assembly: AssemblyCulture "">]

[<assembly: AssemblyVersion(Version)>]
[<assembly: AssemblyFileVersion(Version)>]

()
