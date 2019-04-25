module TokenTests

open System
open Xunit
open Tokenizer
open System.IO
open Tests

[<Theory>]
[<InlineData("\"letters\"")>]
[<InlineData("\'\'")>]
[<InlineData("\"\"")>]
let ``string token`` (str : string) =
    match tokenStream str with
    | [Token.String(a)] -> true
    | _ -> false


