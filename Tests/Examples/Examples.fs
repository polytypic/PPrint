// Copyright (C) by Vesa Karvonen

open System.Diagnostics
open PPrint

[<EntryPoint>]
let main _ =
  let inline (^) x = x

  let result = ref 0

  let chk limit doc expected =
    let actual = render limit doc
    if actual <> expected then
      result := 1
      printf "Expected: %A\nActual: %A\n\n" expected actual

  let limit n doc expected = chk (Some n) doc expected
  let (===) doc expected = chk None doc expected

  empty === ""
  chr 'x' === "x"
  txt "example" === "example"
  fmt "%d" 101 === "101"
  nest 1 ^ parens ^ vsep ^ punctuate comma [txt "x"; txt "y"; txt "z"] ===
    "(x,\n y,\n z)"
  enclose (txt "(* ", line <^> txt " *)")
    ^ nestBy " * " ^ vsep [txt "a"; txt "b"; txt "c"] ===
    "(* a\n * b\n * c\n *)"
  nest 2 ^ hcat [txt "a"; line; txt "b"] === "a\n  b"
  group ^ nest 2 ^ hcat [txt "a"; line; txt "b"] === "a b"
  nest 2 ^ hcat [txt "a"; linebreak; txt "b"] === "a\n  b"
  group ^ nest 2 ^ hcat [txt "a"; linebreak; txt "b"] === "ab"
  txt "a" <.> txt "b" === "a\nb"
  txt "a" <.> txt "b" |> group === "a b"

  let wide = txt "\\\"a\\nb\\nc\\\""
  let narrow = vsep [txt "\"a\\n\""; txt "\"b\\n\""; txt "\"c\""]
  choice wide narrow === "\\\"a\\nb\\nc\\\""
  limit 3
   <| choice wide narrow
   <| "\"a\\n\"\n\"b\\n\"\n\"c\""

  hsep [txt "a"; txt "b"; txt "c"] === "a b c"
  vsep [txt "a"; txt "b"; txt "c"] === "a\nb\nc"
  hcat [txt "a"; txt "b"; txt "c"] === "abc"

  width <| txt "foo" <| fun n ->
       nest (n+1) ^
        parens ^
         vsep ^
          punctuate comma ^
           [txt "bar"; txt "baz"; txt "foobar"]
  === "foo(bar,\n    baz,\n    foobar)"

  txt "foo" <^>
  parens ^ align ^ vsep ^ punctuate comma [txt "bar"; txt "baz"; txt "foobar"]
  === "foo(bar,\n    baz,\n    foobar)"

  printfn "Done!"

  for i=1 to 9 do
    let start = Stopwatch.StartNew ()
    let x = txt "x"
    Seq.init (1 <<< i*2) ^ fun _ -> x
    |> fillCat
    |> PPrint.outputWithFun ignore (Some 40)
    printfn "Bench %d: %A" i start.Elapsed

  !result
