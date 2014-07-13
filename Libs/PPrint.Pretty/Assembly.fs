// Copyright (C) by Vesa Karvonen

namespace PPrint.Pretty

open System.Reflection
open System.Runtime.InteropServices

[<AutoOpen>]
module AssemblyInfo =
  [<Literal>]
  let Version = "0.0.0.0"

[<assembly: AssemblyTitle("PPrint.Pretty")>]
[<assembly: AssemblyDescription("A generic pretty printer for F# types using PPrint and Infers.")>]
[<assembly: AssemblyConfiguration("")>]
[<assembly: AssemblyCompany("")>]
[<assembly: AssemblyProduct("PPrint.Pretty")>]
[<assembly: AssemblyCopyright("© Vesa Karvonen")>]
[<assembly: AssemblyTrademark("")>]
[<assembly: AssemblyCulture("")>]

[<assembly: ComVisible(false)>]

[<assembly: Guid("eb3481ce-c426-43f4-b8d9-acabbd2bf520")>]

[<assembly: AssemblyVersion(Version)>]
[<assembly: AssemblyFileVersion(Version)>]

()
