// Copyright (C) by Vesa Karvonen

namespace PPrint

open System
open System.IO

type Doc =
  | EMPTY
  | LAZY of Lazy<Doc>
  | LINE of bool
  | JOIN of lhs: Doc * rhs: Doc
  | NEST of string * Doc
  | TEXT of string
  | CHOICE of wide: Doc * narrow: Doc
  | COLUMN of (int -> Doc)
  | NESTING of (int -> Doc)
  | USER of obj

[<AutoOpen>]
module Consts =
  let space = TEXT " "
  let line = LINE false
  let linebreak = LINE true
  let softline = CHOICE (space, line)
  let softbreak = CHOICE (EMPTY, linebreak)

  let inline joinWith d l r = JOIN (l, JOIN (d, r))

type Doc with
  static member (<^>) (l, r) = JOIN (l, r)
  static member (<+>) (l, r) = joinWith space l r
  static member (<.>) (l, r) = joinWith line l r
  static member (</>) (l, r) = joinWith softline l r
  static member (<..>) (l, r) = joinWith linebreak l r
  static member (<//>) (l, r) = joinWith softbreak l r

[<AutoOpen>]
module Util =
  let inline (^) x = x
  let inline K x _ = x
  let inline flip f x y = f y x
  let inline force (s: Lazy<_>) = s.Force ()

[<AutoOpen>]
module PPrint =
  let delay th = LAZY ^ Lazy.Create th

  let empty = EMPTY
  let space = space
  let line = line
  let linebreak = linebreak
  let softline = softline
  let softbreak = softbreak

  let column i2d = COLUMN i2d
  let nesting i2d = NESTING i2d

  let txt s = TEXT s
  let user (x: obj) = USER x
  let fmt f = Printf.ksprintf TEXT f
  let chr (c: char) = TEXT ^ string c

  let lparen = TEXT "("
  let rparen = TEXT ")"
  let lrparen = (lparen, rparen)
  let langle = TEXT "<"
  let rangle = TEXT ">"
  let lrangle = (langle, rangle)
  let lbrace = TEXT "{"
  let rbrace = TEXT "}"
  let lrbrace = (lbrace, rbrace)
  let lbracket = TEXT "["
  let rbracket = TEXT "]"
  let lrbracket = (lbracket, rbracket)
  let squote = TEXT "'"
  let lrsquote = (squote, squote)
  let dquote = TEXT "\""
  let lrdquote = (dquote, dquote)
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
  let nest n = nestBy ^ spaces n

  let align d = column ^ fun k -> nesting ^ fun i -> d |> nest ^ k-i
  let hang i d = align ^ nest i d
  let indent i d = txt ^ spaces i <^> d |> hang i

  let width d f = column ^ fun l -> d <^> column ^ fun r -> f ^ r-l

  let mkFill p t f d =
    width d ^ fun w -> if p f w then t f else txt ^ spaces ^ f-w

  let fillBreak n d = mkFill (<) (flip nest linebreak) n d
  let fill n d = mkFill (<=) (K empty) n d

  let rec flatten doc = delay ^ fun () ->
    match doc with
     | LAZY doc -> flatten ^ force doc
     | JOIN (lhs, rhs) -> JOIN (flatten lhs, flatten rhs)
     | NEST (txt, doc) -> NEST (txt, flatten doc)
     | EMPTY | TEXT _ | USER _ -> doc
     | LINE b -> if b then empty else space
     | CHOICE (wide, _) -> wide
     | COLUMN f -> COLUMN (flatten << f)
     | NESTING f -> NESTING (flatten << f)
  let choice wide narrow = CHOICE (flatten wide, narrow)
  let group doc = CHOICE (flatten doc, doc)

  let gnest n = group >> nest n

  let inline joinWith' bop xs =
    match Seq.fold (fun ys x -> x::ys) [] xs with
     | [] -> empty
     | x::xs -> List.fold (flip bop) x xs
  let joinWith bop xs = joinWith' bop xs
  let joinSep sep xs = xs |> joinWith' ^ fun l r -> l <^> (sep <^> r)
  let hsep xs = joinWith' (<+>) xs
  let hcat xs = joinWith' (<^>) xs
  let vsep xs = joinWith' (<.>) xs
  let vcat xs = joinWith' (<..>) xs
  let fillSep xs = joinWith' (</>) xs
  let fillCat xs = joinWith' (<//>) xs

  let sep xs = group ^ vsep xs
  let cat xs = group ^ vcat xs

  let inline enclose' (l: Doc, r: Doc) d = l <^> (d <^> r)
  let enclose (l, r) d = enclose' (l, r) d
  let squotes d = enclose' lrsquote d
  let dquotes d = enclose' lrdquote d
  let parens d = enclose' lrparen d
  let angles d = enclose' lrangle d
  let braces d = enclose' lrbrace d
  let brackets d = enclose' lrbracket d

  type t =
   | NIL
   | PRINT of text: string * Lazy<t>
   | LINEFEED of prefix: string * Lazy<t>
   | OBJ of obj: obj * Lazy<t>

  type [<AbstractClass>] Actions () =
    abstract Line: unit -> unit
    abstract Write: string -> unit
    abstract User: obj -> unit
    default t.Line () = t.Write "\n"
    default t.User _ = ()

  let rec output (actions: Actions) doc =
    match force doc with
     | NIL -> ()
     | PRINT (str, doc) ->
       actions.Write str
       output actions doc
     | OBJ (obj, doc) ->
       actions.User obj
       output actions doc
     | LINEFEED (prefix, doc) ->
       actions.Line ()
       actions.Write prefix
       output actions doc

  let rec fits maxColsOr0 usedCols doc =
    maxColsOr0 = 0 ||
    usedCols <= maxColsOr0 &&
    match force doc with
     | NIL | LINEFEED _ -> true
     | OBJ (_, doc) ->
       fits maxColsOr0 usedCols doc
     | PRINT (str, doc) ->
       fits maxColsOr0 (usedCols + str.Length) doc

  type Docs =
    | Done
    | Docs of string * Doc * Docs

  let rec layout maxColsOr0 usedCols = function
    | Done -> NIL
    | Docs (prefix, doc, rest) ->
      match doc with
       | LAZY doc ->
         layout maxColsOr0 usedCols ^ Docs (prefix, force doc, rest)
       | EMPTY ->
         layout maxColsOr0 usedCols rest
       | JOIN (lhs, rhs) ->
         layout maxColsOr0 usedCols ^
         Docs (prefix, lhs, Docs (prefix, rhs, rest))
       | NEST (txt, doc) ->
         layout maxColsOr0 usedCols ^ Docs (prefix + txt, doc, rest)
       | TEXT str ->
         PRINT (str, lazy layout maxColsOr0 (usedCols + str.Length) rest)
       | USER obj ->
         OBJ (obj, lazy layout maxColsOr0 usedCols rest)
       | LINE _ ->
         LINEFEED (prefix, lazy layout maxColsOr0 prefix.Length rest)
       | CHOICE (wide, narrow) ->
         let wide = layout maxColsOr0 usedCols ^ Docs (prefix, wide, rest)
         if fits maxColsOr0 usedCols ^ lazy wide
         then wide
         else layout maxColsOr0 usedCols ^ Docs (prefix, narrow, rest)
       | COLUMN f ->
         layout maxColsOr0 usedCols ^ Docs (prefix, f usedCols, rest)
       | NESTING f ->
         layout maxColsOr0 usedCols ^ Docs (prefix, f prefix.Length, rest)

  let outputWithActions actions maxCols doc =
    let maxColsOr0 =
      match maxCols with
       | None -> 0
       | Some n ->
         if n <= 0 then failwithf "maxCols: %d" n else n
    output actions ^ lazy layout maxColsOr0 0 (Docs ("", doc, Done))

  let inline outputWithFun write maxCols doc =
    outputWithActions
      {new Actions () with
        member this.Write s = write s}
      maxCols
      doc

  let outputToWriter (tw: TextWriter) maxCols doc =
    outputWithFun tw.Write maxCols doc

  let render maxCols doc =
    use tw = new StringWriter ()
    outputToWriter tw maxCols doc
    tw.ToString ()

  let println maxCols doc =
    outputWithFun Console.Write maxCols doc
    Console.Write "\n"
