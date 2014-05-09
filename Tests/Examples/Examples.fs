// Copyright (C) by Vesa Karvonen

open PPrint

let chk limit doc expected =
  let actual = render limit doc
  if actual <> expected then
    printf "Expected: %A\nActual: %A\n\n" expected actual

let limit n doc expected = chk (Some n) doc expected
let (===) doc expected = chk None doc expected

do empty === ""
do chr 'x' === "x"
do txt "example" === "example"
do fmt "%d" 101 === "101"
do nest 1 (parens (vsep (punctuate comma [txt "x"; txt "y"; txt "z"]))) === "(x,\n y,\n z)"
do enclose (txt "(* ", line <^> txt " *)")
     (nestBy " * " (vsep [txt "a"; txt "b"; txt "c"])) === "(* a\n * b\n * c\n *)"
do nest 2 (hcat [txt "a"; line; txt "b"]) === "a\n  b"
do group (nest 2 (hcat [txt "a"; line; txt "b"])) === "a b"
do nest 2 (hcat [txt "a"; linebreak; txt "b"]) === "a\n  b"
do group (nest 2 (hcat [txt "a"; linebreak; txt "b"])) === "ab"
do txt "a" <.> txt "b" === "a\nb"
do group (txt "a" <.> txt "b") === "a b"

do let wide = txt "\\\"a\\nb\\nc\\\""
   let narrow = vsep [txt "\"a\\n\""; txt "\"b\\n\""; txt "\"c\""]
   choice wide narrow === "\\\"a\\nb\\nc\\\""
   limit 3
    <| choice wide narrow
    <| "\"a\\n\"\n\"b\\n\"\n\"c\""

do hsep [txt "a"; txt "b"; txt "c"] === "a b c"
do vsep [txt "a"; txt "b"; txt "c"] === "a\nb\nc"
do hcat [txt "a"; txt "b"; txt "c"] === "abc"

do width (txt "foo") (fun n -> 
    nest (n+1)
     (parens
       (vsep
         (punctuate comma
           [txt "bar"; txt "baz"; txt "foobar"])))) ===
   "foo(bar,\n    baz,\n    foobar)"

do txt "foo" <^>
   parens (align (vsep (punctuate comma [txt "bar"; txt "baz"; txt "foobar"]))) ===
   "foo(bar,\n    baz,\n    foobar)"
