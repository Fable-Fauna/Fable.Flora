module Tests

open System
open Xunit
open FParsec
open CssProvider
open Parser
open System.IO

let assert_success result =
  let success =
      match result with
      | Success(z,_,_) -> true
      | Failure(_,_,_) -> false
  Assert.True success
    

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

[<Theory>]
[<InlineData("*.class")>]
[<InlineData(".class")>]
[<InlineData("|*#prop")>]
[<InlineData("*|*")>]
[<InlineData("juice|*")>]
[<InlineData("*|div.class")>]
[<InlineData("juice|div#prop")>]
[<InlineData("|div.class.class")>]
[<InlineData("div")>]
let ``simple selector seq`` (str : string) =
    let result = run parseTypeSelector str
    assert_success result

[<Theory>]
[<InlineData("*.class")>]
[<InlineData(".class")>]
[<InlineData("juice|div + *")>]
[<InlineData("|div * juice|div")>]
[<InlineData("div > div")>]
let ``selector with cominators`` (str : string) =
    let result = run parseSelector str
    assert_success result

[<Fact>]
let ``parse sample`` () =
    let str = 
        " code, kbd, samp {
          font-family: monospace, monospace; /* 1 */
          font-size: 1em; /* 2 */
        } "
    let result = run parseDefinition str
    assert_success result

[<Fact>]
let ``parse sample 2`` () =
    let str = 
        " /*
         * Add the correct display in Edge, IE 10+, and Firefox.
         */

        details {
          display: block;
        }"
    let result = run parseDefinition str
    assert_success result

[<Fact>]
let ``tailwind`` () =
    let testText = File.ReadAllText("../../../../test/tailwind.css")
    let result = run parseCss testText 
    assert_success result


[<Fact>]
let ``bootstrap`` () =
    let testText = File.ReadAllText("../../../../test/bootstrap.css")
    let result = run parseCss testText 
    assert_success result