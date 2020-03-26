module LuhnTest

open Xunit
open FsCheck
open FsCheck.Xunit
open Luhny
open Luhny.FSharp
open Swensen.Unquote
open System

let stringVectors = [
  "", false
  " ", false
  "A", false
  "123A456B789C", false
  "1234567890", false
  "1234-5678-9012-3456", false
  "378282246310005", true
  "371449635398431", true
  "378734493671000", true
  "5610591081018250", true
  "30569309025904", true
  "38520000023237", true
  "6011111111111117", true
  "6011000990139424", true
  "3530111333300000", true
  "3566002020360505", true
  "5555555555554444", true
  "5105105105105100", true
  "4111111111111111", true
  "4111-1111-1111-1111", false
  "4012888888881881", true
  "4222222222222", true
  "5019717010103742", true
  "6331101999990016", true
]

let intVectors = [
  -1234567890L, false
  -1L, false
  0L, false
  9L, false
  1234567890L, false
  378282246310005L, true
  371449635398431L, true
  378734493671000L, true
  5610591081018250L, true
  30569309025904L, true
  38520000023237L, true
  6011111111111117L, true
  6011000990139424L, true
  3530111333300000L, true
  3566002020360505L, true
  5555555555554444L, true
  5105105105105100L, true
  4111111111111111L, true
  4012888888881881L, true
  4222222222222L, true
  5019717010103742L, true
  6331101999990016L, true
]

type VerifyStringData() as this =
  inherit TheoryData<string, bool>()
  do Seq.iter this.Add stringVectors

type VerifyIntData() as this =
  inherit TheoryData<int64, bool>()
  do Seq.iter this.Add intVectors

module private Gen =
  let private intFilter = flip Gen.filter Arb.generate<int>

  let private int64Filter = flip Gen.filter Arb.generate<int64>

  let luhnLength = intFilter ((<) 1)

  let invalidLuhnLength = intFilter ((>) 2)

  let luhnInt64Length = intFilter <| fun n -> n > 1 && n < 19

  let invalidLuhnInt64Length = intFilter <| fun n -> n < 2 || n > 18

  let luhnInt64Prefix = int64Filter ((<) 0L)

  let luhnPrefix = luhnInt64Prefix |> Gen.map (fun n -> n.ToString())

[<Property>]
let ``LuhnLength.create.should return Ok when valid`` () =
  Gen.luhnLength |> Arb.fromGen |> Prop.forAll <| fun length ->
    LuhnLength.create length =! Ok (LuhnLength length)

[<Property>]
let ``LuhnLength.create.should return Error when out of range`` () =
  Gen.invalidLuhnLength |> Arb.fromGen |> Prop.forAll <| fun length ->
    LuhnLength.create length =! Error OutOfRange

[<Property>]
let ``LuhnLength.value.should return raw value`` () =
  Gen.luhnLength |> Arb.fromGen |> Prop.forAll <| fun length ->
    LuhnLength.value (LuhnLength length) =! length

[<Property>]
let ``LuhnInt64Length.create.should return Ok when valid`` () =
  Gen.luhnInt64Length |> Arb.fromGen |> Prop.forAll <| fun length ->
    LuhnInt64Length.create length =! Ok (LuhnInt64Length length)

[<Property>]
let ``LuhnInt64Length.create.should return Error when out of range`` () =
  Gen.invalidLuhnInt64Length |> Arb.fromGen |> Prop.forAll <| fun length ->
    LuhnInt64Length.create length =! Error OutOfRange

[<Property>]
let ``LuhnInt64Length.value.should return raw value`` () =
  Gen.luhnInt64Length |> Arb.fromGen |> Prop.forAll <| fun length ->
    LuhnInt64Length.value (LuhnInt64Length length) =! length

[<Property>]
let ``Luhn.create.should return valid Luhn string`` (prefix) =
  Gen.luhnLength |> Arb.fromGen |> Prop.forAll <| fun length ->
    test <@ Luhn.create (LuhnLength length) prefix |> Luhn.verify @>

[<Property>]
let ``Luhn.create.should return string of length`` (prefix) =
  Gen.luhnLength |> Arb.fromGen |> Prop.forAll <| fun length ->
    test <@ Luhn.create (LuhnLength length) prefix |> String.length |> (=) length @>

[<Property>]
let ``Luhn.create.should return string with prefix`` () =
  Gen.zip Gen.luhnLength Gen.luhnPrefix |> Arb.fromGen |> Prop.forAll <| fun (length, prefix) ->
    let trimmed = if prefix.Length < length then prefix else prefix.Substring(0, length - 1)
    let hasPrefix (s: string) = s.StartsWith trimmed
    test <@ Luhn.create (LuhnLength length) (Some prefix) |> hasPrefix @>

[<Property>]
let ``Luhn.createInt64.should return valid Luhn integer`` (prefix) =
  Gen.luhnInt64Length |> Arb.fromGen |> Prop.forAll <| fun length ->
    test <@ Luhn.createInt64 (LuhnInt64Length length) prefix |> Luhn.verifyInt64 @>

[<Property>]
let ``Luhn.createInt64.should return integer of length`` (prefix) =
  Gen.luhnInt64Length |> Arb.fromGen |> Prop.forAll <| fun length ->
    test <@ Luhn.createInt64 (LuhnInt64Length length) prefix |> Int64.countDigits |> (=) length @>

[<Property>]
let ``Luhn.createInt64.should return integer with prefix`` () =
  Gen.zip Gen.luhnInt64Length Gen.luhnInt64Prefix |> Arb.fromGen |> Prop.forAll <| fun (length, prefix) ->
    let digits = Int64.countDigits prefix
    let trimmed = if digits < length then prefix else prefix / pown 10L (max (digits - length) 1)
    let hasPrefix (n: int64) = n.ToString().StartsWith(trimmed.ToString())
    test <@ Luhn.createInt64 (LuhnInt64Length length) (Some prefix) |> hasPrefix @>

[<Theory; ClassData(typeof<VerifyStringData>)>]
let ``Luhn.verify.should return expected result`` (value, expected) =
  Luhn.verify value =! expected

[<Theory; ClassData(typeof<VerifyIntData>)>]
let ``Luhn.verifyInt64.should return expected result`` (value, expected) =
  Luhn.verifyInt64 value =! expected

[<Fact>]
let ``Luhn.should be a static class`` () =
  let t = typeof<Luhn>
  test <@ t.IsAbstract && t.IsSealed @>

[<Fact>]
let ``Luhn.should not have delegate members`` () =
  let members = typeof<Luhn>.GetMembers(Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Static)
  let returnType (mi: Reflection.MemberInfo) =
    match mi with
    | :? Reflection.PropertyInfo as p -> p.PropertyType
    | :? Reflection.MethodInfo as m -> m.ReturnType
    | _ -> typeof<Object>
  let delegateNames = ["Action"; "Func"]
  let isDelegateType (t: Type) = t.IsConstructedGenericType && List.contains (t.GetGenericTypeDefinition().Name) delegateNames
  <@ not <| Seq.exists (returnType >> isDelegateType) members @>

[<Property>]
let ``Luhn.Create(int).should throw ArgumentOutOfRangeException when length is less than 2`` () =
  Gen.invalidLuhnLength |> Arb.fromGen |> Prop.forAll <| fun length ->
    raises<ArgumentOutOfRangeException> <@ Luhn.Create(length) @>

[<Property>]
let ``Luhn.Create(int).should return valid Luhn string`` () =
  Gen.luhnLength |> Arb.fromGen |> Prop.forAll <| fun length ->
    test <@ Luhn.Create(length) |> Luhn.verify @>

[<Property>]
let ``Luhn.Create(int).should return string of length`` () =
  Gen.luhnLength |> Arb.fromGen |> Prop.forAll <| fun length ->
    test <@ Luhn.Create(length) |> String.length |> (=) length @>

[<Property>]
let ``Luhn.Create(int, string).should throw ArgumentOutOfRangeException when length is less than 2`` (prefix: NonNull<string>) =
  Gen.invalidLuhnLength |> Arb.fromGen |> Prop.forAll <| fun length ->
    raises<ArgumentOutOfRangeException> <@ Luhn.Create(length, prefix.Get) @>

[<Property>]
let ``Luhn.Create(int, string).should throw ArgumentNullException when prefix is null`` () =
  Gen.luhnLength |> Arb.fromGen |> Prop.forAll <| fun length ->
    raises<ArgumentNullException> <@ Luhn.Create(length, null) @>

[<Property>]
let ``Luhn.Create(int, string).should return valid Luhn string`` (prefix: NonNull<string>) =
  Gen.luhnLength |> Arb.fromGen |> Prop.forAll <| fun length ->
    test <@ Luhn.Create(length, prefix.Get) |> Luhn.verify @>

[<Property>]
let ``Luhn.Create(int, string).should return string of length`` (prefix: NonNull<string>) =
  Gen.luhnLength |> Arb.fromGen |> Prop.forAll <| fun length ->
    test <@ Luhn.Create(length, prefix.Get) |> String.length |> (=) length @>

[<Property>]
let ``Luhn.Create(int, string).should return string with prefix`` () =
  Gen.zip Gen.luhnLength Gen.luhnPrefix |> Arb.fromGen |> Prop.forAll <| fun (length, prefix) ->
    let trimmed = if prefix.Length < length then prefix else prefix.Substring(0, length - 1)
    let hasPrefix (s: string) = s.StartsWith trimmed
    test <@ Luhn.Create(length, prefix) |> hasPrefix @>

[<Property>]
let ``Luhn.CreateInt64(int).should throw ArgumentOutOfRangeException when length is not between 2 and 18`` () =
  Gen.invalidLuhnInt64Length |> Arb.fromGen |> Prop.forAll <| fun length ->
    raises<ArgumentOutOfRangeException> <@ Luhn.CreateInt64(length) @>

[<Property>]
let ``Luhn.CreateInt64(int).should return valid Luhn integer`` () =
  Gen.luhnInt64Length |> Arb.fromGen |> Prop.forAll <| fun length ->
    test <@ Luhn.CreateInt64(length) |> Luhn.verifyInt64 @>

[<Property>]
let ``Luhn.CreateInt64(int).should return integer of length`` () =
  Gen.luhnInt64Length |> Arb.fromGen |> Prop.forAll <| fun length ->
    test <@ Luhn.CreateInt64(length) |> Int64.countDigits |> (=) length @>

[<Property>]
let ``Luhn.CreateInt64(int, int64).should throw ArgumentOutOfRangeException when length is not between 2 and 18`` (prefix) =
  Gen.invalidLuhnInt64Length |> Arb.fromGen |> Prop.forAll <| fun length ->
    raises<ArgumentOutOfRangeException> <@ Luhn.CreateInt64(length, prefix) @>

[<Property>]
let ``Luhn.CreateInt64(int, int64).should return valid Luhn integer`` (prefix) =
  Gen.luhnInt64Length |> Arb.fromGen |> Prop.forAll <| fun length ->
    test <@ Luhn.CreateInt64(length, prefix) |> Luhn.verifyInt64 @>

[<Property>]
let ``Luhn.CreateInt64(int, int64).should return integer of length`` (prefix) =
  Gen.luhnInt64Length |> Arb.fromGen |> Prop.forAll <| fun length ->
    test <@ Luhn.CreateInt64(length, prefix) |> Int64.countDigits |> (=) length @>

[<Property>]
let ``Luhn.CreateInt64(int, int64).should return integer with prefix`` () =
  Gen.zip Gen.luhnInt64Length Gen.luhnInt64Prefix |> Arb.fromGen |> Prop.forAll <| fun (length, prefix) ->
    let digits = Int64.countDigits prefix
    let trimmed = if digits < length then prefix else prefix / pown 10L (max (digits - length) 1)
    let hasPrefix (n: int64) = n.ToString().StartsWith(trimmed.ToString())
    test <@ Luhn.createInt64 (LuhnInt64Length length) (Some prefix) |> hasPrefix @>

[<Fact>]
let ``Luhn.Verify(string).should throw ArgumentNullException when number is null`` () =
  raises<ArgumentNullException> <@ Luhn.Verify(null) @>

[<Theory; ClassData(typeof<VerifyStringData>)>]
let ``Luhn.Verify(string).should return expected result`` (value: string, expected) =
  Luhn.Verify(value) =! expected

[<Theory; ClassData(typeof<VerifyIntData>)>]
let ``Luhn.Verify(int64).should return expected result`` (value: int64, expected) =
  Luhn.Verify(value) =! expected
