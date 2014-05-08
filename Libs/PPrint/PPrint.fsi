// Copyright (C) by Vesa Karvonen

/// A pretty printing library for F# based on Wadler's and Leijen's work.
///
/// Philip Wadler's article ''A prettier printer'' is a good introduction to the
/// motivation and basic usage of this library.  Daan Leijen's document
/// ''PPrint, a prettier printer'' provides motivation for the additional
/// primitives.
namespace PPrint

open System.IO

/// The abstract type of documents.
type [<Sealed; NoEquality; NoComparison>] Doc =
  /// `lhs <^> rhs` is the concatenation of the documents `lhs` and `rhs`.
#if DOC
  ///
  /// Note: This is the same as the operator `<>` used in the original Haskell
  /// libraries.  In F#, the `<>` operator is already used.
#endif
  static member (<^>): Doc * Doc -> Doc

  /// Concatenates the given documents with a `space` in between.  `lhs <+> rhs`
  /// is equivalent to `lhs <^> space <^> rhs`.
  static member (<+>): Doc * Doc -> Doc

  /// Concatenates the given documents with a `line` in between.  `lhs <.> rhs`
  /// is equivalent to `lhs <^> line <^> rhs`.
  static member (<.>): Doc * Doc -> Doc

  /// Concatenates the given documents with a `softline` in between.  `lhs </>
  /// rhs` is equivalent to `lhs <^> softline <^> rhs`.
  static member (</>): Doc * Doc -> Doc

  /// Concatenates the given documents with a `linebreak` in between.  `lhs <..>
  /// rhs` is equivalent to `lhs <^> linebreak <^> rhs`.
  static member (<..>): Doc * Doc -> Doc

  /// Concatenates the given documents with a `softbreak` in between.  `lhs <//>
  /// rhs` is equivalent to `lhs <^> softbreak <^> rhs`.
  static member (<//>): Doc * Doc -> Doc

/// Operations on pretty print documents.
[<AutoOpen>]
module PPrint =
  // == Rendering ==

  /// Outputs the document using the given output function.
  val outputWithFun: write: (string -> unit)
                  -> maxCols: option<int>
                  -> doc: Doc
                  -> unit

  /// Outputs the document using the given text writer.
  val outputToWriter: writer: TextWriter
                   -> maxCols: option<int>
                   -> doc: Doc
                   -> unit

  /// Renders the document as a string.
  val render: maxCols: option<int> -> doc: Doc -> string

  /// Writes the document to standard output with a newline.
  val println: maxCols: option<int> -> doc: Doc -> unit

  // == Basic Combinators ==

  /// The empty document is semantically equivalent to `txt ""`.
  val empty: Doc

  /// `chr c` renders to the character `c`.  The character shouldn't be a
  /// newline.
  val chr: char -> Doc

  /// `txt s` renders to the string `s`.  The string shouldn't contain any
  /// newline characters.
  val txt: string -> Doc

  /// `fmt ...` is equivalent to `txt (sprintf ...)`.  For example, to format an
  /// integer `i`, one could write `fmt "%d" i`.
  val fmt: Printf.StringFormat<'a, Doc> -> 'a

  /// `nest n doc` renders document `doc` indented by `n` more columns.  See
  /// also `gnest`.
#if DOC
  ///
  /// For example
  ///
  ///> nest 2 (txt "Hello" <.> txt "world") <.> txt "!"
  ///
  /// renders as
  ///
  ///> Hello
  ///>   world
  ///> !
  ///
  /// Note that in order for `nest n doc` to have any effect, you must have
  /// `line`s or `linebreak`s in the `doc`.
#endif
  val nest: numCols: int -> Doc -> Doc

  /// Advances to the next line and indents, unless undone by `group` in which
  /// case `line` behaves like `txt " "`.
  val line: Doc

  /// Advances to the next line and indents, unless undone by `group` in which
  /// case `linebreak` behaves like `empty`.
  val linebreak: Doc

  /// Used to specify alternative layouts.  `group doc` undoes all line breaks
  /// in document `doc`.  The resulting line of text is added to the current
  /// output if it fits.  Otherwise, the document is rendered without changes
  /// (with line breaks).
  val group: Doc -> Doc

  /// `gnest n doc` is equivalent to `nest n (group doc)` which is equivalent to
  /// `group (nest n doc)`.  This combinator can be useful, because `nest` is
  /// frequently combined with `group`.
  val gnest: numCols: int -> Doc -> Doc

  /// Used to specify alternative documents.  The wider document is added to the
  /// current output line if it fits.  Otherwise, the narrow document is
  /// rendered.
#if DOC
  ///
  /// Warning: This operation allows one to create documents whose rendering may
  /// not produce optimal or easily predictable results.
#endif
  val choice: wide: Doc -> narrow: Doc -> Doc

  /// Creates a lazily computed document.  `delay <| fun () -> doc` is
  /// equivalent to `doc` except that the expression `doc` may not be evaluated
  /// at all.
#if DOC
  ///
  /// Note: This is primarily useful for specifying the narrow alternative to
  /// `choice` - unless, of course, there is a chance that the whole document
  /// will not be rendered at all.
#endif
  val delay: (unit -> Doc) -> Doc

  /// Behaves like `space` if the resulting output fits, otherwise behaves like
  /// `line`.
  val softline: Doc

  /// Behaves like `empty` if the resulting output fits, otherwise behaves like
  /// `line`.
  val softbreak: Doc

  // == Alignment Combinators ==

  val column: (int -> Doc) -> Doc
  val nesting: (int -> Doc) -> Doc

  val indent: int -> Doc -> Doc
  val hang: int -> Doc -> Doc


  /// `align doc` renders `doc` with the nesting level set to the current
  /// column.
  ///
  /// For example
  ///
  ///> txt "Hi" <+> align (txt "nice" <.> txt "world!")
  ///
  /// renders as
  ///
  ///> Hi nice
  ///>    world!
  val align: Doc -> Doc

  val width: (int -> Doc) -> Doc -> Doc

  val fillBreak: int -> Doc -> Doc
  val fill: int -> Doc -> Doc

  // == Sequence Combinators ==

  /// `sep docs` is equivalent to `group (vsep docs)`.
  val sep: seq<Doc> -> Doc

  /// `cat docs` is equivalent to `group (vcat docs)`.
  val cat: seq<Doc> -> Doc

  /// `punctuate sep docs` concatenates `sep` to the right of each document in
  /// `docs` except the last one.
  val punctuate: sep: Doc -> docs: seq<Doc> -> seq<Doc>

  /// Concatenates the documents using `<+>`.
  val hsep: seq<Doc> -> Doc

  /// Concatenates the documents using `<.>`.
  val vsep: seq<Doc> -> Doc

  /// Concatenates the documents using `</>`.
  val fillSep: seq<Doc> -> Doc

  /// Concatenates the documents using `<^>`.
  val hcat: seq<Doc> -> Doc

  /// Concatenates the documents using `<..>`.
  val vcat: seq<Doc> -> Doc

  /// Concatenates the documents using `<//>`.
  val fillCat: seq<Doc> -> Doc

  // == Bracketing Combinators ==

  /// `enclose (lhs, rhs) doc` is equivalent to `lhs <^> doc <^> rhs`
  val enclose: Doc * Doc -> Doc -> Doc

  /// `squotes doc` is equivalent to `enclose (squote, squote) doc`.
  val squotes: Doc -> Doc

  /// `dquotes doc` is equivalent to `enclose (dquote, dquote) doc`.
  val dquotes: Doc -> Doc

  /// `parens doc` is equivalent to `enclose (lparen, rparen) doc`.
  val parens: Doc -> Doc

  /// `angles doc` is equivalent to `enclose (langle, rangle) doc`.
  val angles: Doc -> Doc

  /// `braces doc` is equivalent to `enclose (lbrace, rbrace) doc`.
  val braces: Doc -> Doc

  /// `brackets doc` is equivalent to `enclose (lbracket, rbracket) doc`.
  val brackets: Doc -> Doc

  // == Character Documents ==

  /// Equivalent to `txt "("`.
  val lparen: Doc

  /// Equivalent to `txt ")"`.
  val rparen: Doc

  /// Equivalent to `txt "<"`.
  val langle: Doc

  /// Equivalent to `txt ">"`.
  val rangle: Doc

  /// Equivalent to `txt "{"`.
  val lbrace: Doc

  /// Equivalent to `txt "}"`.
  val rbrace: Doc

  /// Equivalent to `txt "["`.
  val lbracket: Doc

  /// Equivalent to `txt "]"`.
  val rbracket: Doc

  /// Equivalent to `txt "'"`.
  val squote: Doc

  /// Equivalent to `txt "\""`.
  val dquote: Doc

  /// Equivalent to `txt ";"`.
  val semi: Doc

  /// Equivalent to `txt ":"`.
  val colon: Doc

  /// Equivalent to `txt ","`.
  val comma: Doc

  /// Equivalent to `txt " "`.
  val space: Doc

  /// Equivalent to `txt "."`.
  val dot: Doc

  /// Equivalent to `txt "\\"`.
  val backslash: Doc

  /// Equivalent to `txt "="`.
  val equals: Doc
