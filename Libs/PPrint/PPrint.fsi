// Copyright (C) by Vesa Karvonen

/// A pretty printing library for F# based on Wadler's and Leijen's work.
namespace PPrint

open System.IO

/// The abstract type of documents.
type [<NoEquality; NoComparison>] Doc

[<AutoOpen>]
module Combinators =
  /// Renders the document using the given output function.
  val renderer: option<int> -> (string -> unit) -> Doc -> unit

  /// Renders the document as a string.
  val render: option<int> -> Doc -> string

  /// Writes the document to the output stream.
  val output: TextWriter -> option<int> -> Doc -> unit

  /// Writes the document to standard output with a newline.
  val println: option<int> -> Doc -> unit

  // == Basic Combinators ==

  /// The empty document is semantically equivalent to `txt ""`.
  val empty: Doc

  /// `chr c` contains the character `c`.  The character shouldn't be a newline.
  val chr: char -> Doc

  /// `txt s` contains the string `s`.  The string shouldn't contain any newline
  /// characters.
  val txt: string -> Doc

  /// `l <^> r` is the concatenation of the documents `l` and `r`.
  ///
  /// Note: This is the same as the operator <> used in the original Haskell
  /// libraries.  In F#, <> is already used.
  val (<^>): Doc -> Doc -> Doc

  /// `nest n d` renders document `d` indented by `n` more columns.
  ///
  /// Note that in order for `nest` to have any effect, you must have line
  /// breaks in `group`s in `d`.
  val nest: int -> Doc -> Doc

  /// Advances to the next line and indents, unless undone by `group` in which
  /// case `line` behaves like `txt " "`.
  val line: Doc

  /// Advances to the next line and indents, unless undone by `group` in which
  /// case `linebreak` behaves like `empty`.
  val linebreak: Doc

  /// Used to specify alternative layouts.  `group d` undoes all line breaks in
  /// document `d`.  The resulting line of text is added to the current output
  /// line if it fits.  Otherwise, the document is rendered without changes
  /// (with line breaks).
  val group: Doc -> Doc

  /// Used to specify alternative documents.
  type Choice = {wide: Doc; narrow: Doc}

  /// Used to specify alternative documents.  The wider document is added to the
  /// current output line if it fits.  Otherwise, the narrow document is
  /// rendered.
  ///
  /// Warning: This operation allows one to create documents whose rendering may
  /// not produce optimal or easily predictable results.
  val choice: Choice -> Doc

  /// Creates a lazily computed document.  `delay <| fun () -> doc` is
  /// equivalent to `doc` except that the expression `doc` may not be evaluated
  /// at all.
  ///
  /// Note: This is primarily useful for specifying the narrow alternative to
  /// `choice` - unless, of course, there is a chance that the whole document
  /// will not be rendered at all.
  val delay: (unit -> Doc) -> Doc

  /// Behaves like a space if the resulting output fits, otherwise behaves like
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
  val align: Doc -> Doc

  val width: (int -> Doc) -> Doc -> Doc

  val fillBreak: int -> Doc -> Doc
  val fill: int -> Doc -> Doc

  // == Operators ==

  /// Concatenates with a `space`.
  val (<+>): Doc -> Doc -> Doc

  /// Concatenates with a `line`.
  val (<.>): Doc -> Doc -> Doc

  /// Concatenates with a `softline`.
  val (</>): Doc -> Doc -> Doc

  /// Concatenates with a `linebreak`.
  val (<..>): Doc -> Doc -> Doc

  /// Concatenates with a `softbreak`.
  val (<//>): Doc -> Doc -> Doc

  // == List Combinators ==

  /// `sep` is equivalent to `group o vsep`
  val sep: seq<Doc> -> Doc

  /// `cat` is equivalent to `group o vcat`
  val cat: seq<Doc> -> Doc

  /// `punctuate sep docs` concatenates `sep` to the right of each document in
  /// `docs` except the last one.
  val punctuate: Doc -> seq<Doc> -> seq<Doc>

  /// Concatenates with `<+>`.
  val hsep: seq<Doc> -> Doc

  /// Concatenates with `<$>`.
  val vsep: seq<Doc> -> Doc

  /// Concatenates with `</>`.
  val fillSep: seq<Doc> -> Doc

  /// Concatenates with `<^>`.
  val hcat: seq<Doc> -> Doc

  /// Concatenates with `<$$>`.
  val vcat: seq<Doc> -> Doc

  /// Concatenates with `<//>`.
  val fillCat: seq<Doc> -> Doc

  // == Bracketing Combinators ==

  /// `enclose (l, r) d` is equivalent to `l <^> d <^> r`
  val enclose: Doc * Doc -> Doc -> Doc

  /// `squotes` is equivalent to `enclose (squote, squote)`
  val squotes: Doc -> Doc

  /// `dquotes` is equivalent to `enclose (dquote, dquote)`
  val dquotes: Doc -> Doc

  /// `parens` is equivalent to `enclose (lparen, rparen)`
  val parens: Doc -> Doc

  /// `angles` is equivalent to `enclose (langle, rangle)`
  val angles: Doc -> Doc

  /// `braces` is equivalent to `enclose (lbrace, rbrace)`
  val braces: Doc -> Doc

  /// `brackets` is equivalent to `enclose (lbracket, rbracket)`
  val brackets: Doc -> Doc

  // == Character Documents ==

  /// `txt "("`
  val lparen: Doc

  /// `txt ")"`
  val rparen: Doc

  /// `txt "<"`
  val langle: Doc

  /// `txt ">"`
  val rangle: Doc

  /// `txt "{"`
  val lbrace: Doc

  /// `txt "}"`
  val rbrace: Doc

  /// `txt "["`
  val lbracket: Doc

  /// `txt "]"`
  val rbracket: Doc

  /// `txt "'"`
  val squote: Doc

  /// `txt "\""`
  val dquote: Doc

  /// `txt ";"`
  val semi: Doc

  /// `txt ":"`
  val colon: Doc

  /// `txt ","`
  val comma: Doc

  /// `txt " "`
  val space: Doc

  /// `txt "."`
  val dot: Doc

  /// `txt "\\"`
  val backslash: Doc

  /// `txt "="`
  val equals: Doc
