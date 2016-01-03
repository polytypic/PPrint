PPrint is a pretty printing combinator library for text documents that can be
rendered to a desired maximum width.

### Docs

* [PPrint reference](http://polytypic.github.io/PPrint/PPrint.html)
  * [![NuGet](https://img.shields.io/nuget/v/PPrint.svg)](https://www.nuget.org/packages/PPrint/) [![NuGet total](https://img.shields.io/nuget/dt/PPrint.svg)](https://www.nuget.org/packages/PPrint/)
* [PPrint.Console reference](http://polytypic.github.io/PPrint/PPrint.Console.html)
  * [![NuGet](https://img.shields.io/nuget/v/PPrint.Console.svg)](https://www.nuget.org/packages/PPrint.Console/) [![NuGet total](https://img.shields.io/nuget/dt/PPrint.Console.svg)](https://www.nuget.org/packages/PPrint.Console/)

### Background

* [A prettier printer](http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf)
* [PPrint, a prettier printer](http://research.microsoft.com/en-us/um/people/daan/download/pprint/pprint.html)

### Example

Here is an example of how one could define the function

```fsharp
module Json =
  val pretty: JsonValue -> Doc
```

for pretty printing JSON values:

```fsharp
open System.Json
open PPrint

let inline (^) x = x

module Json =
  let private prettySeq (l, r) toDoc xs =
    if Seq.isEmpty xs
    then l <^> r
    else let ds = xs |> Seq.map toDoc |> punctuate comma |> vsep
         l <..> ds |> nest 2 <..> r |> group

  let rec pretty (json: JsonValue) =
    match json with
     | :? JsonObject as json ->
       json
       |> prettySeq lrbrace ^ fun kv ->
            pretty ^ JsonPrimitive kv.Key <^> colon <+> pretty kv.Value
     | :? JsonArray as json ->
       json
       |> prettySeq lrbracket pretty
     | null ->
       txt "null"
     | _ ->
       json
       |> string
       |> txt
```

Given a JSON value

```fsharp
let json = JsonValue.Parse """
{
  "firstName": "John",
  "lastName": "Smith",
  "isAlive": true,
  "age": 25,
  "address": {
    "streetAddress": "21 2nd Street",
    "city": "New York",
    "state": "NY",
    "postalCode": "10021-3100"
  },
  "phoneNumbers": [
    {
      "type": "home",
      "number": "212 555-1234"
    },
    {
      "type": "office",
      "number": "646 555-4567"
    }
  ],
  "children": [],
  "spouse": null
}
"""
```

We can render it to different desired maximum widths.  For example,

```fsharp
json |> Json.pretty |> println ^ Some 40
```

produces

```
{
  "address": {
    "city": "New York",
    "postalCode": "10021-3100",
    "state": "NY",
    "streetAddress": "21 2nd Street"
  },
  "age": 25,
  "children": [],
  "firstName": "John",
  "isAlive": true,
  "lastName": "Smith",
  "phoneNumbers": [
    {
      "number": "212 555-1234",
      "type": "home"
    },
    {
      "number": "646 555-4567",
      "type": "office"
    }
  ],
  "spouse": null
}
```

and

```fsharp
json |> Json.pretty |> println ^ Some 80
```

produces

```
{
  "address": {
    "city": "New York",
    "postalCode": "10021-3100",
    "state": "NY",
    "streetAddress": "21 2nd Street"
  },
  "age": 25,
  "children": [],
  "firstName": "John",
  "isAlive": true,
  "lastName": "Smith",
  "phoneNumbers": [
    {"number": "212 555-1234", "type": "home"},
    {"number": "646 555-4567", "type": "office"}
  ],
  "spouse": null
}
```
