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
  static member (<^>) (l: Doc, r: Doc) = JOIN (l, r)
  static member (<+>) (l: Doc, r: Doc) = joinWith space l r
  static member (<.>) (l: Doc, r: Doc) = joinWith line l r
  static member (</>) (l: Doc, r: Doc) = joinWith softline l r
  static member (<..>) (l: Doc, r: Doc) = joinWith linebreak l r
  static member (<//>) (l: Doc, r: Doc) = joinWith softbreak l r

[<AutoOpen>]
module Util =
  let inline K x _ = x
  let inline flip f x y = f y x
  let inline force (s: Lazy<_>) = s.Force ()

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
  let user (x: obj) = USER x
  let fmt f = Printf.ksprintf TEXT f
  let chr (c: char) = TEXT (string c)

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

  let joinWith bop xs =
    match Seq.revAppendToList xs [] with
     | [] -> empty
     | x::xs -> List.fold (flip bop) x xs
  let catWith bop xs = joinWith bop xs // XXX obsolete
  let joinSep sep xs = joinWith (fun l r -> l <^> (sep <^> r)) xs
  let hsep xs = joinWith (<+>) xs
  let hcat xs = joinWith (<^>) xs
  let vsep xs = joinWith (<.>) xs
  let vcat xs = joinWith (<..>) xs
  let fillSep xs = joinWith (</>) xs
  let fillCat xs = joinWith (<//>) xs

  let sep xs = group (vsep xs)
  let cat xs = group (vcat xs)

  let enclose (l, r) d = l <^> (d <^> r)
  let squotes d = enclose lrsquote d
  let dquotes d = enclose lrdquote d
  let parens d = enclose lrparen d
  let angles d = enclose lrangle d
  let braces d = enclose lrbrace d
  let brackets d = enclose lrbracket d

  type t =
   | NIL
   | PRINT of text: string * Lazy<t>
   | LINEFEED of prefix: string * Lazy<t>
   | OBJ of obj: obj * Lazy<t>

  type [<AbstractClass>] Actions () =
    abstract Line: unit -> unit
    abstract Write: string -> unit
    abstract User: obj -> unit
    default this.Line () = this.Write "\n"
    default this.User _ = ()

  let outputWithActions (actions: Actions) maxCols doc =
    let rec layout t =
      match t with
       | NIL -> ()
       | PRINT (str, doc) ->
         actions.Write str
         layout (force doc)
       | OBJ (obj, doc) ->
         actions.User obj
         layout (force doc)
       | LINEFEED (prefix, doc) ->
         actions.Line ()
         actions.Write prefix
         layout (force doc)

    let fits usedCols doc =
      match maxCols with
       | None -> true
       | Some maxCols ->
         let rec lp usedCols doc =
           usedCols <= maxCols &&
           match force doc with
            | NIL | LINEFEED _ -> true
            | OBJ (_, doc) -> lp usedCols doc
            | PRINT (str, doc) -> lp (usedCols + String.length str) doc
         lp usedCols (lazy doc)

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
         | USER obj ->
           OBJ (obj, lazy best usedCols rest)
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
