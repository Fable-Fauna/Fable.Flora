module TokenTests

open System
open Xunit
open Tokenizer
open System.IO
open Tests

[<Theory>]
[<InlineData("*")>]
[<InlineData("")>]
[<InlineData("|*")>]
[<InlineData("*|*")>]
[<InlineData("juice|*")>]
[<InlineData("*|div")>]
[<InlineData("juice|div")>]
[<InlineData("|div")>]
[<InlineData("div")>]
let ``type selector`` (str : string) =
    let result = run parseTypeSelector str
    assert_success result

