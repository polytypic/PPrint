// Copyright (C) by Vesa Karvonen

namespace PPrint

open System.Reflection
open System.Runtime.InteropServices

[<AutoOpen>]
module AssemblyInfo =
  [<Literal>]
  let Version = "1.4.4"

[<assembly: AssemblyTitle("PPrint")>]
[<assembly: AssemblyDescription("A pretty printing library for F# based on Wadler's and Leijen's work.")>]
[<assembly: AssemblyConfiguration("")>]
[<assembly: AssemblyCompany("")>]
[<assembly: AssemblyProduct("PPrint")>]
[<assembly: AssemblyCopyright("© Vesa Karvonen")>]
[<assembly: AssemblyTrademark("")>]
[<assembly: AssemblyCulture("")>]

[<assembly: ComVisible(false)>]

[<assembly: Guid("e7e47cf6-b20e-4ae8-aa62-898658deec04")>]

[<assembly: AssemblyVersion(Version)>]
[<assembly: AssemblyFileVersion(Version)>]

()
