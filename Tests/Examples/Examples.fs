// Copyright (C) by Vesa Karvonen

open PPrint
open PPrint.Pretty

type Foo = Foo of Foo: int * Bar: bool * Baz: ref<unit>

type FoBaBaz = {Foo: int; Bar: bool; Baz: ref<unit>}

type Fuu = {Fuu: bool}
type Bar = Bar of int * float
         | Baz
         | Fuu of Fuu

[<EntryPoint>]
let main args =
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
  nest 1 (parens (vsep (punctuate comma [txt "x"; txt "y"; txt "z"]))) === "(x,\n y,\n z)"
  enclose (txt "(* ", line <^> txt " *)")
     (nestBy " * " (vsep [txt "a"; txt "b"; txt "c"])) === "(* a\n * b\n * c\n *)"
  nest 2 (hcat [txt "a"; line; txt "b"]) === "a\n  b"
  group (nest 2 (hcat [txt "a"; line; txt "b"])) === "a b"
  nest 2 (hcat [txt "a"; linebreak; txt "b"]) === "a\n  b"
  group (nest 2 (hcat [txt "a"; linebreak; txt "b"])) === "ab"
  txt "a" <.> txt "b" === "a\nb"
  group (txt "a" <.> txt "b") === "a b"

  let wide = txt "\\\"a\\nb\\nc\\\""
  let narrow = vsep [txt "\"a\\n\""; txt "\"b\\n\""; txt "\"c\""]
  choice wide narrow === "\\\"a\\nb\\nc\\\""
  limit 3
   <| choice wide narrow
   <| "\"a\\n\"\n\"b\\n\"\n\"c\""

  hsep [txt "a"; txt "b"; txt "c"] === "a b c"
  vsep [txt "a"; txt "b"; txt "c"] === "a\nb\nc"
  hcat [txt "a"; txt "b"; txt "c"] === "abc"

  width (txt "foo") (fun n -> 
   nest (n+1)
    (parens
      (vsep
        (punctuate comma
          [txt "bar"; txt "baz"; txt "foobar"])))) ===
  "foo(bar,\n    baz,\n    foobar)"

  txt "foo" <^>
  parens (align (vsep (punctuate comma [txt "bar"; txt "baz"; txt "foobar"]))) ===
  "foo(bar,\n    baz,\n    foobar)"

  pretty ((-1y, 2uy), (3s, 4us), (5, 6u), (-7L, 8UL), (-34.5f, 34.5)) ===
  "((-1y, 2uy), (3s, 4us), (5, 6u), (-7L, 8UL), (-34.5f, 34.5))"

  pretty ['a'; '\n'; ' '; '\u0002'; '-'] ===
  "['a'; '\\n'; ' '; '\\u0002'; '-']"

  pretty [|None; Some false|] ===
  "[|None; Some false|]"


  pretty (Foo (1, false, ref ())) ===
  "Foo (Foo = 1, Bar = false, Baz = ref ())"

  pretty [Bar (1, 1.0); Baz; Fuu {Fuu = false}] ===
  "[Bar (1, 1); Baz; Fuu {Fuu = false}]"

  pretty {Foo = 314; Bar = true; Baz = ref ()} ===
  "{Foo = 314; Bar = true; Baz = ref ()}"

  limit 14
   <| pretty {Foo = 314; Bar = true; Baz = ref ()}
   <| "{Foo = 314\n Bar = true\n Baz = ref ()}"

  pretty "'Hello,\nworld!'" ===
  "\"'Hello,\\nworld!'\""

  limit 8
   <| pretty "'Hello,\nworld!'"
   <| "\"'Hello,\\\n \\nworld!'\""

  !result
