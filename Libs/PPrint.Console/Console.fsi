// Copyright (C) by Vesa Karvonen

namespace PPrint

[<AutoOpen>]
module Console =
  /// Writes the document to standard output with a newline.
  val println: maxCols: option<int> -> doc: Doc -> unit
