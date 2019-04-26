module TokenTests

open System
open Xunit
open Tokenizer
open Expecto
open System.IO
open Tests

[<Theory>]
[<InlineData("""letters""")>]
[<InlineData("''")>]
[<InlineData("""""")>]
let ``string token`` (str : string) =
    match tokenStream str with
    | [Token.String(a)] -> true
    | _ -> false
[<Theory>]
[<InlineData("/* */")>]
[<InlineData(" ")>]
[<InlineData("  ")>]
let ``whitespace token`` (str : string) =
    match tokenStream str with
    | [Token.Whitespace(a)] -> true
    | _ -> false

[<Tests>]
let tests =
    testList "token tests" [
        test "string token" {
            "\"letters\"" |> tokenStream  |> (function | [Token.String(_)] -> true | _ -> false) |> (fun x -> Expect.isTrue x "string of letters")
        }
    ]