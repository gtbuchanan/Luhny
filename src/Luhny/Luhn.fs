namespace Luhny.FSharp

open System.Text
open System.Diagnostics.CodeAnalysis

[<ExcludeFromCodeCoverage>]
type LuhnLengthError = OutOfRange

[<ExcludeFromCodeCoverage>]
type LuhnLength = internal LuhnLength of int

module LuhnLength =
  let create n =
    if n <= 1 then Error OutOfRange
    else LuhnLength n |> Ok

  let value (LuhnLength n) = n

[<ExcludeFromCodeCoverage>]
type LuhnInt64Length = internal LuhnInt64Length of int

module LuhnInt64Length =
  let create n =
    if n <= 1 || n >= 19 then Error OutOfRange
    else LuhnInt64Length n |> Ok

  let value (LuhnInt64Length n) = n

module Luhn =
  let private checksumDigit rightIndex digit =
    // Double odd position digits starting after the check digit (right-most digit).
    // If the product has more than one digit, sum the individual digits.
    let d = if rightIndex % 2 = 0 then digit else digit * 2
    if d > 9 then d - 9 else d

  let private reduceCheckDigit n = (10 - n % 10) % 10

  let private create' length (prefix: string option) =
    let trimPrefix (s: string) =
      if isNull s || s.Length = 0 then None
      elif s.Length >= length then s.Substring(0, length - 1) |> Some
      else Some s
    let tryParseDigit =
      (+) -1
      >> String.tryItem
      >> second (Option.bind Char.decimalDigitValue)
    let getOrCreateDigit =
      flip tuple2 <| Option.bind trimPrefix prefix
      >> mapItem1 tryParseDigit
      >> (<||) Option.bind
      >> Option.defaultWith (fun () -> Random.next 0 9)
    let foldDigits ((sb: StringBuilder), acc) ir i =
      let digit = getOrCreateDigit i
      sb.Insert(0, digit), acc + checksumDigit (ir + 1) digit
    let appendCheckDigit ((sb: StringBuilder), n) =
      sb.Append(reduceCheckDigit n).ToString()

    Seq.foldi foldDigits (StringBuilder(), 0) { length - 1..-1..1 }
    |> appendCheckDigit

  let create length prefix = create' (LuhnLength.value length) prefix

  let private createInt64' length =
    let pow10 = (+) 1 >> pown 10L // +1 to skip check digit
    let normalizePrefix n =
      let digits = Int64.countDigits n
      if digits = length then
        Int64.floor 1 n, 0 // Make room for the check digit
      else
        let rem = length - 1 - digits |> abs // Remaining digits to generate
        let adj = pow10 rem // Adjustment to meet length with check digit
        if digits > length then n / adj, 0 else n * adj, rem
    let random n =
      if n = length - 2 then 1 else 0 // Prevent leading zeros
      |> flip Random.next 9 |> int64
    let initDigit = zip random pow10 >> (<||) (*)
    let seedRemainder = flip Seq.init initDigit >> Seq.sum >> (+)
    let checksum =
      Int64.unfoldDigits
      >> Seq.indexed
      >> Seq.sumBy ((<||) checksumDigit)
      >> reduceCheckDigit
      >> int64

    // Zero for prefix equates to None, signs ignored
    Option.bind (noneIf ((=) 0L) >> Option.map (abs >> normalizePrefix))
    >> Option.defaultValue (0L, length - 1)
    >> ifElse (snd >> (=) 0) fst (swap >> (<||) seedRemainder)
    >> zip checksum (+)
    >> (<||) (|>)

  let createInt64 length prefix = createInt64' (LuhnInt64Length.value length) prefix

  let private verify' =
    let generator (ri, i, s: string) =
      let result = ri, s.Chars i |> Char.decimalDigitValue, ri = s.Length - 1
      let state = ri + 1, i - 1, s
      result, state
    let folder (_, acc) (i, maybeDigit, complete) =
      match maybeDigit with
      | None -> false, 0
      | Some d -> complete, acc + checksumDigit i d

    (fun (s: string) -> 0, s.Length - 1, s)
    >> Seq.unfold (item2 >> (>) 0 |> noneIf >> Option.map generator)
    >> Seq.takeWhile (item2 >> Option.isSome) // Short-circuit on non-numeric
    >> Seq.fold folder (false, 0)
    >> (fun (complete, n) -> complete && n % 10 = 0)

  let verify number = not <| isNull number && String.length number > 1 && verify' number

  let private verifyInt64' =
    Int64.unfoldDigits
    >> Seq.indexed
    >> Seq.sumBy ((<||) checksumDigit)
    >> (fun n -> n <> 0 && n % 10 = 0)

  let verifyInt64 number = verifyInt64' number

namespace Luhny

open Luhny.FSharp
open System

[<AbstractClass; Sealed>]
type Luhn =
  static member private CreateInternal length prefix =
    match LuhnLength.create length with
    | Ok l -> Luhn.create l <| Option.ofObj prefix
    | Error e ->
      match e with
      | OutOfRange -> outOfRangeArg "length" length "Must be greater than 1"

  static member Create(length, prefix) =
    if isNull prefix then nullArg "prefix"
    else Luhn.CreateInternal length prefix

  static member Create length = Luhn.CreateInternal length null

  static member private CreateInt64Internal length prefix =
    match LuhnInt64Length.create length with
    | Ok l -> Luhn.createInt64 l <| Option.ofNullable prefix
    | Error e ->
      match e with
      | OutOfRange -> outOfRangeArg "length" length "Must be between 2 and 18"

  static member CreateInt64(length, prefix) =
    Luhn.CreateInt64Internal length <| new Nullable<int64>(prefix)

  static member CreateInt64 length =
    Luhn.CreateInt64Internal length <| new Nullable<int64>()

  static member Verify number =
    if isNull number then nullArg "number" else Luhn.verify number

  static member Verify number = Luhn.verifyInt64 number
