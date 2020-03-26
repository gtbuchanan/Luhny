namespace Luhny.FSharp

/// Represents an error that occurred when constructing a Luhn length.
type LuhnLengthError = OutOfRange

/// Represents the length of a numeric string verifiable by the Luhn algorithm
type LuhnLength = internal LuhnLength of int

/// Basic operations on Luhn string lengths.
module LuhnLength =
  /// <summary>
  ///   Creates a new instance of the <see cref="LuhnLength" /> type.
  /// </summary>
  /// <param name="n">The desired value.</param>
  /// <returns>
  ///   <see cref="Result.Ok" /> if the creation was successful, otherwise a
  ///   <see cref="Result.Error" /> with the specific error.
  /// </returns>
  val create : n:int -> Result<LuhnLength, LuhnLengthError>

  /// <summary>
  ///   Gets the underlying value of the <see cref="LuhnLength" />.
  /// </summary>
  /// <param name="n">The Luhn length.</param>
  /// <returns>The underlying value.</returns>
  val value : n:LuhnLength -> int

/// Represents the length of a numeric value verifiable by the Luhn algorithm
type LuhnInt64Length = internal LuhnInt64Length of int

/// Basic operations on Luhn integer lengths.
module LuhnInt64Length =
  /// <summary>
  ///   Creates a new instance of the <see cref="LuhnLength" /> type.
  /// </summary>
  /// <param name="n">The desired value.</param>
  /// <returns>
  ///   <see cref="Result.Ok" /> if the creation was successful, otherwise a
  ///   <see cref="Result.Error" /> with the specific error.
  /// </returns>
  val create : n:int -> Result<LuhnInt64Length, LuhnLengthError>

  /// <summary>
  ///   Gets the underlying value of the <see cref="LuhnLength" />.
  /// </summary>
  /// <param name="n">The Luhn length.</param>
  /// <returns>The underlying value.</returns>
  val value : n:LuhnInt64Length -> int

/// <summary>
///   A simple checksum formula used to verify a variety of identification numbers, 
///   such as credit card numbers, IMEI numbers, National Provider Identifier numbers
///   in the United States, Canadian Social Insurance Numbers, Israel ID Numbers and
///   Greek Social Security Numbers (ΑΜΚΑ).
/// </summary>
/// <seealso href="https://en.wikipedia.org/wiki/Luhn_algorithm">
///   Wikipedia: Luhn Algorithm
/// </seealso>
[<RequireQualifiedAccess>]
module Luhn =
  /// <summary>
  ///   Generates a numeric string of the specified length verifiable by the Luhn algorithm.
  /// </summary>
  /// <param name="length">The length of the number to generate.</param>
  /// <param name="prefix">
  ///   The prefix for the numeric string. If longer than the length, it will be truncated.
  ///   Non-numeric characters will be replaced.
  /// </param>
  /// <returns>A numeric string of the specified length verifiable by the Luhn algorithm.</returns>
  val create : (LuhnLength -> string option -> string)

  /// <summary>
  ///   Generates an <see cref="Int64" /> of the specified length verifiable by the Luhn algorithm.
  /// </summary>
  /// <param name="length">The length of the number to generate.</param>
  /// <param name="prefix">
  ///   The prefix for the number. If longer than the length, it will be truncated. Signs are ignored.
  /// </param>
  /// <returns>A numeric string of the specified length verifiable by the Luhn algorithm.</returns>
  val createInt64 : (LuhnInt64Length -> int64 option -> int64)

  /// <summary>
  ///   Verifies a number against a checksum generated with the Luhn algorithm.
  /// </summary>
  /// <param name="number">Numeric string to validate.</param>
  /// <returns><c>true</c> if valid, otherwise <c>false</c>.</returns>
  val verify : number:string -> bool

  /// <summary>
  ///   Verifies a number against a checksum generated with the Luhn algorithm.
  /// </summary>
  /// <param name="number">Number to validate.</param>
  /// <returns><c>true</c> if valid, otherwise <c>false</c>.</returns>
  val verifyInt64 : (int64 -> bool)

namespace Luhny

/// <summary>
///   A simple checksum formula used to verify a variety of identification numbers, 
///   such as credit card numbers, IMEI numbers, National Provider Identifier numbers
///   in the United States, Canadian Social Insurance Numbers, Israel ID Numbers and
///   Greek Social Security Numbers (ΑΜΚΑ).
/// </summary>
/// <seealso href="https://en.wikipedia.org/wiki/Luhn_algorithm">
///   Wikipedia: Luhn Algorithm
/// </seealso>
[<AbstractClass; Sealed>]
type Luhn =
  /// <summary>
  ///   Generates a numeric string of the specified length verifiable by the Luhn algorithm.
  /// </summary>
  /// <param name="length">The length of the number to generate.</param>
  /// <returns>A numeric string of the specified length verifiable by the Luhn algorithm.</returns>
  /// <exception cref="ArgumentOutOfRangeException"><paramref cref="length" /> is less than 2.</exception>
  static member Create : length:int -> string

  /// <summary>
  ///   Generates a numeric string of the specified length verifiable by the Luhn algorithm.
  /// </summary>
  /// <param name="length">The length of the number to generate.</param>
  /// <param name="prefix">
  ///   The prefix for the numeric string. If longer than the length, it will be truncated.
  ///   Non-numeric characters will be replaced.
  /// </param>
  /// <returns>A numeric string of the specified length verifiable by the Luhn algorithm.</returns>
  /// <exception cref="ArgumentNullException"><paramref cref="prefix" /> is <c>null</c>.</exception>
  /// <exception cref="ArgumentOutOfRangeException"><paramref cref="length" /> is less than 2.</exception>
  static member Create : length:int * prefix:string -> string

  /// <summary>
  ///   Generates an <see cref="Int64" /> of the specified length verifiable by the Luhn algorithm.
  /// </summary>
  /// <param name="length">The length of the number to generate.</param>
  /// <returns>A numeric string of the specified length verifiable by the Luhn algorithm.</returns>
  /// <exception cref="ArgumentOutOfRangeException"><paramref cref="length" /> is less than 2 or greater than 18.</exception>
  static member CreateInt64 : length:int -> int64

  /// <summary>
  ///   Generates an <see cref="Int64" /> of the specified length verifiable by the Luhn algorithm.
  /// </summary>
  /// <param name="length">The length of the number to generate.</param>
  /// <param name="prefix">
  ///   The prefix for the number. If longer than the length, it will be truncated. Signs are ignored.
  /// </param>
  /// <returns>A numeric string of the specified length verifiable by the Luhn algorithm.</returns>
  /// <exception cref="ArgumentOutOfRangeException"><paramref cref="length" /> is less than 2 or greater than 18.</exception>
  static member CreateInt64 : length:int * prefix:int64 -> int64

  /// <summary>
  ///   Verifies a number against a checksum generated with the Luhn algorithm.
  /// </summary>
  /// <param name="number">Numeric string to validate.</param>
  /// <returns><c>true</c> if valid, otherwise <c>false</c>.</returns>
  /// <exception cref="ArgumentNullException"><paramref cref="number" /> is less than 2 or greater than 18.</exception>
  static member Verify : number:string -> bool

  /// <summary>
  ///   Verifies a number against a checksum generated with the Luhn algorithm.
  /// </summary>
  /// <param name="number">Number to validate.</param>
  /// <returns><c>true</c> if valid, otherwise <c>false</c>.</returns>
  static member Verify : number:int64 -> bool
