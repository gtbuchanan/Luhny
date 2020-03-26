# Luhny

![CI](https://github.com/gtbuchanan/Luhny/workflows/CI/badge.svg)

A .NET Library to create and verify values using the Luhn algorithm. F# and C# friendly.

## Usage

### Import

```fsharp
// F#
open Luhny.FSharp
```

```csharp
// C#
using Luhny;
```

### Create String

```fsharp
// F#
let luhnString =
  match LuhnLength.create 16 with
  | Ok length -> Luhn.create length None
  | _ -> failwith "Invalid Length"
```

```csharp
// C#
var luhnString = Luhn.Create(16);
```

### Create String w/Prefix

```fsharp
// F#
let luhnString =
  match LuhnLength.create 15 with
  | Ok length -> Luhn.create length (Some "34")
  | _ -> failwith "Invalid Length"
```

```csharp
// C#
var luhnString = Luhn.Create(15, "34");
```

### Create Int64

```fsharp
// F#
let luhnNumber =
  match LuhnInt64Length.create 16 with
  | Ok length -> Luhn.createInt64 length None
  | _ -> failwith "Invalid Length"
```

```csharp
// C#
var luhnNumber = Luhn.CreateInt64(16);
```

### Create Int64 w/ Prefix

```fsharp
// F#
let luhnNumber =
  match LuhnInt64Length.create 15 with
  | Ok length -> Luhn.createInt64 length (Some 34L)
  | _ -> failwith "Invalid Length"
```

```csharp
// C#
var luhnNumber = Luhn.CreateInt64(15, 34L);
```

### Verify String

```fsharp
// F#
let isValid = Luhn.verify "378282246310005"
```

```csharp
// C#
var isValid = Luhn.Verify("378282246310005");
```

### Verify Int64

```fsharp
// F#
let isValid = Luhn.verifyInt64 378282246310005L
```

```csharp
// C#
var isValid = Luhn.Verify(378282246310005L);
```
