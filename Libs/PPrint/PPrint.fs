// Copyright (C) by Vesa Karvonen

namespace PPrint

open System
open System.IO

type Doc =
  | EMPTY
  | LAZY of Lazy<Doc>
  | LINE of bool
  | JOIN of Doc * Doc
  | NEST of string * Doc
  | TEXT of string
  | CHOICE of wide: Doc * narrow: Doc
  | COLUMN of (int -> Doc)
  | NESTING of (int -> Doc)

[<AutoOpen>]
module Consts =
  let space = TEXT " "
  let line = LINE false
  let linebreak = LINE true
  let softline = CHOICE (space, line)
  let softbreak = CHOICE (EMPTY, linebreak)

type Doc with
  static member (<^>) (l: Doc, r: Doc) = JOIN (l, r)
  static member (<+>) (l: Doc, r: Doc) = JOIN (l, JOIN (space, r))
  static member (<.>) (l: Doc, r: Doc) = JOIN (l, JOIN (line, r))
  static member (</>) (l: Doc, r: Doc) = JOIN (l, JOIN (softline, r))
  static member (<..>) (l: Doc, r: Doc) = JOIN (l, JOIN (linebreak, r))
  static member (<//>) (l: Doc, r: Doc) = JOIN (l, JOIN (softbreak, r))

[<AutoOpen>]
module Util =
  let inline K x _ = x
  let inline flip f x y = f y x
  let inline force (s: Lazy<_>) = s.Force ()

module List =
  let revAppend xs ys =
    List.fold (fun ys x -> x::ys) ys xs

module Seq =
  let revAppendToList xs ys =
    Seq.fold (fun ys x -> x::ys) ys xs

[<AutoOpen>]
module PPrint =
  let delay th = LAZY (Lazy.Create th)

  let empty = EMPTY
  let space = space
  let line = line
  let linebreak = linebreak
  let softline = softline
  let softbreak = softbreak

  let column i2d = COLUMN i2d
  let nesting i2d = NESTING i2d

  let txt s = TEXT s
  let fmt f = Printf.ksprintf TEXT f
  let chr (c: char) = TEXT (string c)

  let lparen = TEXT "("
  let rparen = TEXT ")"
  let parens' = (lparen, rparen)
  let langle = TEXT "<"
  let rangle = TEXT ">"
  let angles' = (langle, rangle)
  let lbrace = TEXT "{"
  let rbrace = TEXT "}"
  let braces' = (lbrace, rbrace)
  let lbracket = TEXT "["
  let rbracket = TEXT "]"
  let brackets' = (lbracket, rbracket)
  let squote = TEXT "'"
  let dquote = TEXT "\""
  let semi = TEXT ";"
  let colon = TEXT ":"
  let comma = TEXT ","
  let dot = TEXT "."
  let backslash = TEXT "\\"
  let equals = TEXT "="

  let punctuate sep (ds: seq<Doc>) = seq {
    use ds = ds.GetEnumerator ()
    if ds.MoveNext () then
      let d = ref ds.Current
      while ds.MoveNext () do
        yield !d <^> sep
        d := ds.Current
      yield !d
  }

  let spaces n = String.replicate n " "

  let nestBy s d = NEST (s, d)
  let nest n = nestBy (spaces n)

  let align d = column (fun k -> nesting (fun i -> nest (k-i) d))
  let hang i d = align (nest i d)
  let indent i d = hang i (txt (spaces i) <^> d)

  let width d f = column (fun l -> d <^> column (fun r -> f (r-l)))

  let mkFill p t f d =
    width d (fun w -> if p f w then t f else txt (spaces (f-w)))

  let fillBreak n d = mkFill (<) (flip nest linebreak) n d
  let fill n d = mkFill (<=) (K empty) n d

  let rec flatten doc = delay <| fun () ->
    match doc with
     | LAZY doc -> flatten (force doc)
     | EMPTY -> doc
     | JOIN (lhs, rhs) -> JOIN (flatten lhs, flatten rhs)
     | NEST (txt, doc) -> NEST (txt, flatten doc)
     | TEXT _ -> doc
     | LINE b -> if b then empty else space
     | CHOICE (wide, _) -> wide
     | COLUMN f -> COLUMN (flatten << f)
     | NESTING f -> NESTING (flatten << f)
  let choice wide narrow = CHOICE (flatten wide, narrow)
  let group doc = CHOICE (flatten doc, doc)

  let gnest n doc = nest n (group doc)

  let mkCat bop xs =
    match Seq.revAppendToList xs [] with
     | [] -> empty
     | x::xs -> List.fold bop x xs
  let hsep xs = mkCat (<+>) xs
  let vsep xs = mkCat (<.>) xs
  let fillSep xs = mkCat (</>) xs
  let hcat xs = mkCat (<^>) xs
  let vcat xs = mkCat (<..>) xs
  let fillCat xs = mkCat (<//>) xs

  let sep xs = group (vsep xs)
  let cat xs = group (vcat xs)

  let enclose (l, r) d = l <^> (d <^> r)
  let squotes d = enclose (squote, squote) d
  let dquotes d = enclose (dquote, dquote) d
  let parens d = enclose parens' d
  let angles d = enclose angles' d
  let braces d = enclose braces' d
  let brackets d = enclose brackets' d

  type t =
   | NIL
   | PRINT of text: string * Lazy<t>
   | LINEFEED of prefix: string * Lazy<t>

  let outputWithFun write maxCols doc =
    let rec layout t =
      match t with
       | NIL -> ()
       | PRINT (str, doc) ->
         write str
         layout (force doc)
       | LINEFEED (prefix, doc) ->
         write "\n"
         write prefix
         layout (force doc)

    let fits usedCols doc =
      Option.isNone maxCols ||
      (let rec lp usedCols doc =
         usedCols <= Option.get maxCols &&
         match force doc with
          | NIL | LINEFEED _ -> true
          | PRINT (str, doc) -> lp (usedCols + String.length str) doc
       lp usedCols (lazy doc))

    let rec best usedCols = function
      | [] -> NIL
      | (prefix, doc)::rest ->
        match doc with
         | LAZY doc ->
           best usedCols ((prefix, force doc)::rest)
         | EMPTY ->
           best usedCols rest
         | JOIN (lhs, rhs) ->
           best usedCols ((prefix, lhs)::(prefix, rhs)::rest)
         | NEST (txt, doc) ->
           best usedCols ((prefix + txt, doc)::rest)
         | TEXT str ->
           PRINT (str, lazy best (usedCols + String.length str) rest)
         | LINE _ ->
           LINEFEED (prefix, lazy best (String.length prefix) rest)
         | CHOICE (wide, narrow) ->
           let wide = best usedCols ((prefix, wide)::rest)
           if fits usedCols wide
           then wide
           else best usedCols ((prefix, narrow)::rest)
         | COLUMN f ->
           best usedCols ((prefix, f usedCols)::rest)
         | NESTING f ->
           best usedCols ((prefix, f (String.length prefix))::rest)
    layout (best 0 [("", doc)])

  let outputToWriter (tw: TextWriter) maxCols doc =
    outputWithFun (fun s -> tw.Write s) maxCols doc

  let render maxCols doc =
    use tw = new StringWriter ()
    outputToWriter tw maxCols doc
    tw.ToString ()

  let println maxCols doc =
    outputWithFun (fun s -> Console.Write s) maxCols doc
