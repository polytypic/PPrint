// Copyright (C) by Vesa Karvonen

/// A generic pretty printer for F# types using PPrint and Infers.
namespace PPrint.Pretty

open System
open System.Numerics
open Infers
open Infers.Rep
open PPrint

type Pretty<'t> = 't -> Doc

type OpenPretty<'t>
type ProductPretty<'e, 'es, 't>
type UnionPretty<'c, 'cs, 't>

[<AutoOpen>]
module TopLevel =
  val pretty: 'x -> Doc
  val show: 'x -> string

type [<InferenceRules (StaticMap = StaticMap.Results)>] Pretty =
  static member Get: unit -> Pretty<'t>

  // ---------------------------------------------------------------------------

  new: unit -> Pretty

  // ---------------------------------------------------------------------------

  member toPretty: OpenPretty<'t> -> Pretty<'t>

  // Rec -----------------------------------------------------------------------

  member fix: unit -> Rec<OpenPretty<'t>>

  // Base Types ----------------------------------------------------------------

  member unit: OpenPretty<unit>

  member bool: OpenPretty<bool>

  member int8: OpenPretty<int8>
  member int16: OpenPretty<int16>
  member int32: OpenPretty<int32>
  member int64: OpenPretty<int64>

  member uint8: OpenPretty<uint8>
  member uint16: OpenPretty<uint16>
  member uint32: OpenPretty<uint32>
  member uint64: OpenPretty<uint64>

  member float32: OpenPretty<float32>
  member float64: OpenPretty<float>

  member char: OpenPretty<char>
  member string: OpenPretty<string>

  // Special -------------------------------------------------------------------

  member option: OpenPretty<'t> -> OpenPretty<option<'t>>

  member ref: OpenPretty<'t> -> OpenPretty<ref<'t>>

  member list: OpenPretty<'t> -> OpenPretty<list<'t>>

  member array: OpenPretty<'t> -> OpenPretty<array<'t>>

  // Discriminated Unions ------------------------------------------------------

  member case: Case<Empty, 'cs, 't>
     -> UnionPretty<Empty, 'cs, 't>

  member case: Case<'ls,      'cs, 't>
    * ProductPretty<'ls, 'ls,      't>
     -> UnionPretty<'ls,      'cs, 't>

  member plus: UnionPretty<       'c      , Choice<'c, 'cs>, 't>
             * UnionPretty<           'cs ,            'cs , 't>
            -> UnionPretty<Choice<'c, 'cs>, Choice<'c, 'cs>, 't>

  member union: Rep
            * Union<          't>
         * AsChoice<'cs,      't>
      * UnionPretty<'cs, 'cs, 't>
      -> OpenPretty<          't>

  // Tuples and Records --------------------------------------------------------

  member item: Item<'e, 'es, 't>
       * OpenPretty<'e         >
   -> ProductPretty<'e, 'es, 't>

  member labelled: Labelled<'e, 'es, 't>
               * OpenPretty<'e         >
           -> ProductPretty<'e, 'es, 't>

  member times: ProductPretty<    'e      , And<'e, 'es>, 't>
              * ProductPretty<        'es ,         'es , 't>
             -> ProductPretty<And<'e, 'es>, And<'e, 'es>, 't>

  member product: Rep
            * Product<          't>
          * AsProduct<'es,      't>
      * ProductPretty<'es, 'es, 't>
        -> OpenPretty<          't>
