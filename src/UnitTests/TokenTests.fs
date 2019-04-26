module TokenTests

open System
open Xunit
open Tokenizer
open Expecto
open System.IO
open Tests

[<Theory>]
[<InlineData("""letters""")>]
[<InlineData("'letters'")>]
[<InlineData("""""")>]
let ``string token`` (str : string) =
    match tokenStream str with
    | [Token.String(a)] -> true
    | _ -> false

[<Theory>]
[<InlineData("/* comment */")>]
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
            """letters""" |> tokenStream  |> (function | [Token.String(_)] -> true | _ -> false) |> (fun x -> Expect.isTrue x "string of letters")
        }
    ]


[<Tests>]
let AccepectenceTests =
    testList "Acceptence Tests" [
        test "tailwind" {
            let testText = File.ReadAllText("../../../../../test/tailwind.css")
            let tstream = tokenStream testText
            Expect.isTrue (tstream.Length > 0) (sprintf "token length: %i"  tstream.Length)
        };

        test "bulma" {
            let testText = File.ReadAllText("../../../../../test/bulma.css")
            let tstream = tokenStream testText
            Expect.isTrue (tstream.Length > 0) (sprintf "token length: %i"  tstream.Length)
        }

        test "bootstrap" {
            let testText = File.ReadAllText("../../../../../test/bootstrap.css")
            let tstream = tokenStream testText
            Expect.isTrue (tstream.Length > 0) (sprintf "token length: %i"  tstream.Length)
        }


    ]