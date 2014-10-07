// Copyright (C) by Vesa Karvonen

namespace PPrint.Pretty

open Microsoft.FSharp.Reflection
open System
open System.Numerics
open System.Text
open Infers
open Infers.Rep
open PPrint

type Pretty<'t> = 't -> Doc

type Fixity =
  | Atomic
  | Part

type [<AbstractClass>] InternalPretty<'t> () =
  abstract Pretty: byref<'t> -> Fixity * Doc

type RecPretty<'x> () =
  inherit InternalPretty<'x> ()
  [<DefaultValue>] val mutable impl: InternalPretty<'x>
  override this.Pretty (x) = this.impl.Pretty (&x)

type OpenPretty<'t> = O of InternalPretty<'t>
type ProductPretty<'e, 'es, 't> = P of InternalPretty<'e>
type UnionPretty<'c, 'cs, 't> = U of list<InternalPretty<'t>>

[<AutoOpen>]
module Util =
  let inline atom (d: Doc) = (Atomic, d)
  let inline part (d: Doc) = (Part, d)

  let inline atxt x = atom <| txt x
  let inline con x _ = x
  let inline doc fn =
    {new InternalPretty<'x> () with
      override this.Pretty (x) =
       fn x}
  let inline str fn = doc (fn >> txt >> atom)
  let inline fmt fmt = str (sprintf fmt) |> O
  let inline atomize (f, x) =
    match f with
     | Atomic -> x
     | Part -> parens x
  let inline just (_, x) = x

  let inline hexc c =
    let c = uint32 c
    if c < 0x10000u
    then sprintf "\\u%04x" c
    else sprintf "\\U%08x" c

  let commaLine = comma <^> line
  let semiLine = choice (semi <^> line) line

  let inline seq (o: string)
                 (c: string)
                 (xP: InternalPretty<'x>)
                 (toSeq: 'xs -> seq<'x>) =
    let i = o.Length
    let o = txt o
    let c = txt c
    O {new InternalPretty<'xs> () with
        override this.Pretty (xs) =
         toSeq xs
         |> Seq.map (fun (x: 'x) ->
              let mutable x = x
              xP.Pretty (&x) |> snd)
         |> joinSep semiLine
         |> gnest i
         |> enclose (o, c)
         |> atom}

type [<InferenceRules (StaticMap = StaticMap.Results)>] Pretty () =
  static member MakePretty () : Pretty<'x> =
    match Engine.TryGenerate (Pretty ()) with
     | None -> failwithf "Pretty: Unsupported type %A" typeof<'x>
     | Some pu -> pu

  static member Get () = StaticMap<Pretty>.Memoize Pretty.MakePretty

  member this.toPretty (O p: OpenPretty<'t>) : Pretty<'t> = fun x ->
    let mutable x = x
    just <| p.Pretty (&x)

  member this.fix () : Rec<OpenPretty<'t>> =
    let r = RecPretty<'t> ()
    let o = O r
    {new Rec<OpenPretty<'t>> () with
      override this.Get () = o
      override this.Set (O x) = r.impl <- x}

  member this.unit: OpenPretty<unit> = O (doc (con (atxt "()")))

  member this.bool: OpenPretty<bool> =
    let t = atxt "true"
    let f = atxt "false"
    O (doc (fun b -> if b then t else f))

  member this.int8:  OpenPretty<int8>  = fmt "%dy"
  member this.int16: OpenPretty<int16> = fmt "%ds"
  member this.int32: OpenPretty<int32> = fmt "%d"
  member this.int64: OpenPretty<int64> = fmt "%dL"

  member this.uint8:  OpenPretty<uint8>  = fmt "%duy"
  member this.uint16: OpenPretty<uint16> = fmt "%dus"
  member this.uint32: OpenPretty<uint32> = fmt "%du"
  member this.uint64: OpenPretty<uint64> = fmt "%dUL"

  member this.float32: OpenPretty<float32> = fmt "%.9gf" 
  member this.float64: OpenPretty<float>   = fmt "%.17g"

  member this.char: OpenPretty<char> =
    let a = atxt "'\\''"
    let b = atxt "'\\b'"
    let n = atxt "'\\n'"
    let q = atxt "'\\\"'"
    let r = atxt "'\\r'"
    let s = atxt "'\\\\'"
    let t = atxt "'\\t'"
    O << doc <| function
     | '\'' -> a | '\b' -> b | '\n' -> n | '\"' -> q | '\r' -> r | '\\' -> s
     | '\t' -> t
     | c when Char.IsControl c -> hexc c |> txt |> squotes |> atom
     | c -> sprintf "'%c'" c |> atxt

  member this.string: OpenPretty<string> =
    O << doc <| fun s ->
      let mutable sawLF = false
      let mutable sawCR = false
      let wide =
        let sb = StringBuilder ()
        let inline S (s: string) = sb.Append s |> ignore
        let inline C (c: char) = sb.Append c |> ignore
        C '\"'
        for c in s do
          match c with
           | '\'' -> S"'"
           | '\b' -> S"\\b"
           | '\n' -> S"\\n" ; sawLF <- true
           | '\"' -> S"\\\""
           | '\r' -> S"\\r" ; sawCR <- true
           | '\\' -> S"\\\\"
           | '\t' -> S"\\t"
           | c when Char.IsControl c -> S (hexc c)
           | c -> C c
        C '\"'
        txt (sb.ToString ())
      let cutOnLF = sawLF
      let inline narrow () = 
        let parts = ResizeArray<_> ()
        let sb = StringBuilder ()
        let cut () =
          parts.Add (txt (sb.ToString ()))
          sb.Clear () |> ignore
        let inline S (s: string) = sb.Append s |> ignore
        let inline C (c: char) = sb.Append c |> ignore
        C '\"'
        for c in s do
          match c with
           | '\'' -> S"'"
           | '\b' -> S"\\b"
           | '\n' ->
             if cutOnLF then
               C '\\'
               cut ()
             S"\\n"
           | '\"' -> S"\\\""
           | '\r' ->
             if not cutOnLF then
               C '\\'
               cut ()
             S"\\r"
           | '\\' -> S"\\\\"
           | '\t' -> S"\\t"
           | c when Char.IsControl c -> S (hexc c)
           | c -> C c
        C '\"'
        cut ()
        nest 1 (vcat parts)
      atom (if sawLF || sawCR then choice wide (delay narrow) else wide)

  member this.option (O tP: OpenPretty<'t>) : OpenPretty<option<'t>> =
    let n = atxt "None"
    let s = txt "Some" <^> line
    O << doc <| function
     | None -> n
     | Some x ->
       let mutable x = x
       part (gnest 2 (s <^> atomize (tP.Pretty (&x))))

  member this.ref (O tP: OpenPretty<'t>) : OpenPretty<ref<'t>> =
    let con = txt "ref" <^> line
    O {new InternalPretty<ref<'t>> () with
        override this.Pretty (rx) =
         part (gnest 2 (con <^> just (tP.Pretty rx)))}

  member this.list (O tP: OpenPretty<'t>) : OpenPretty<list<'t>> =
    seq "[" "]" tP List.toSeq

  member this.array (O tP: OpenPretty<'t>) : OpenPretty<array<'t>> =
    seq "[|" "|]" tP Array.toSeq

  member this.case (case :        Case<Empty, 'cs, 't>)
                         : UnionPretty<Empty, 'cs, 't> =
    U [doc (con (atxt case.Name))]

  member this.case (case  :          Case<'ls,      'cs, 't>,
                    P lsP : ProductPretty<'ls, 'ls,      't>)
                          :   UnionPretty<'ls,      'cs, 't> =
    let con = txt case.Name <^> line
    U [doc <| fun x ->
       let mutable ls = Unchecked.defaultof<_>
       case.Extract (x, &ls)
       part (gnest 2 (con <^> atomize (lsP.Pretty (&ls))))]

  member this.plus (U c  : UnionPretty<       'c,       Choice<'c, 'cs>, 't>,
                    U cs : UnionPretty<           'cs ,            'cs , 't>)
                         : UnionPretty<Choice<'c, 'cs>, Choice<'c, 'cs>, 't> =
    U (c @ cs)

  member this.union (_     :         Rep,
                     union :       Union<          't>,
                     _     :    AsChoice<'cs,      't>,
                     U cs  : UnionPretty<'cs, 'cs, 't>)
                           :  OpenPretty<          't> =
    let cs = Array.ofList cs
    O {new InternalPretty<'t> () with
        override this.Pretty x = cs.[union.Tag x].Pretty (&x)}

  member this.item (_    :          Item<'e, 'es, 't>,
                    O eP :    OpenPretty<'e         >)
                         : ProductPretty<'e, 'es, 't> =
    P eP

  member this.labelled (l    :      Labelled<'e, 'es, 't>,
                        O eP :    OpenPretty<'e         >)
                             : ProductPretty<'e, 'es, 't> =
    let n = l.Name
    if n.StartsWith "Item" &&
       (let i = l.Index
        i = 0 && n.Length = 4 ||
        let suffix = string (i+1)
        n.Length = suffix.Length + 4 &&
        n.EndsWith suffix)
    then P eP
    else let label = txt n <+> (equals <^> line)
         P {new InternalPretty<'e> () with
             override this.Pretty e =
              part (gnest 2 (label <^> just (eP.Pretty (&e))))}

  member this.times (P eP  : ProductPretty<    'e,       And<'e, 'es>, 't>,
                     P esP : ProductPretty<        'es ,         'es , 't>)
                           : ProductPretty<And<'e, 'es>, And<'e, 'es>, 't> =
    let sep = if FSharpType.IsRecord typeof<'t> then semiLine else commaLine
    P {new InternalPretty<And<'e, 'es>> () with
        override this.Pretty ees =
         part (just (eP.Pretty (&ees.Elem)) <^>
               (sep <^> just (esP.Pretty (&ees.Rest))))}

  member this.product (_     :           Rep,
                       _     :       Product<          't>,
                       asPr  :     AsProduct<'es,      't>,
                       P esP : ProductPretty<'es, 'es, 't>)
                             :    OpenPretty<          't> =
    let lr = if FSharpType.IsRecord typeof<'t> then lrbrace else lrparen
    O << doc <| fun t ->
    let mutable es = Unchecked.defaultof<_>
    asPr.Extract (t, &es)
    atom (enclose lr (gnest 1 (just (esP.Pretty (&es)))))

[<AutoOpen>]
module TopLevel =
  let pretty x = Pretty.Get () x
  let show x = render None (pretty x)
