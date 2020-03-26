namespace Luhny.FSharp

open System
open System.Globalization
open System.Threading

[<AutoOpen>]
module internal Internals =
  let flip f x y = f y x

  let ifElse f g h x = if f x then g x else h x

  // Bifunctors
  let second f g = g >> f

  // Tuples
  let tuple2 x y = x, y

  let item2 (_, y, _) = y

  let mapItem1 f (x, y) = f x, y

  let swap (x, y) = y, x

  let zip f g x = f x, g x

  // Options
  let noneIf f x = if f x then None else Some x

  // Math
  let inline divRem a b = a / b, a % b

  // Exceptions
  let outOfRangeArg name value msg = raise (new ArgumentOutOfRangeException(name, value, msg))

module internal Char =
  let decimalDigitValue =
    CharUnicodeInfo.GetDecimalDigitValue >> noneIf ((=) -1)

module internal String =
  let tryItem i (s: string) = if s.Length > i then s.Chars i |> Some else None

module internal Int64 =
  let floor digits n =
    let b = pown 10L digits
    n / b * b

  let countDigits : int64 -> int =
    float >> log10 >> int >> (+) 1

  let unfoldDigits =
    let mapper = flip divRem 10L >> swap >> mapItem1 int
    Seq.unfold (noneIf ((>=) 0L) >> Option.map mapper)

module internal Seq =
  let foldi folder state =
    let inner (p, i) t = folder p i t, i + 1
    Seq.fold inner (state, 0) >> fst

// https://stackoverflow.com/a/7792667/1409101
module internal Random =
  let private random = new Random()
  let private localRandom = new ThreadLocal<Random>(fun _ ->
    lock random (ignore >> random.Next) |> Random)

  let next minValue maxValue = localRandom.Value.Next(minValue, maxValue)
