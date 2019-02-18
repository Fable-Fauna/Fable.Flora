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


[<Theory>]
[<InlineData("*.class[dude = 'balh']")>]
[<InlineData("*.class[dude = \"balh\"]")>]
[<InlineData("div[blah|id ~= id]")>]
let ``selector with attributes`` (str : string) =
    let result = run parseSelector str
    assert_success result

[<Theory>]
[<InlineData(":-moz-focusring")>]
[<InlineData("::-moz-focus-inner")>]
let ``parse psudo elements`` (str : string) =
    let result = run parsePseudoElement str
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
let ``parse sample 3`` () =
    let str = "/*! normalize.css v8.0.1 | MIT License | github.com/necolas/normalize.css */
    
    /* Document
       ========================================================================== */
    
    /**
     * 1. Correct the line height in all browsers.
     * 2. Prevent adjustments of font size after orientation changes in iOS.
     */
    
    html {
      line-height: 1.15; /* 1 */
      -webkit-text-size-adjust: 100%; /* 2 */
    }"
    let result = run parseDefinition str
    match result with
    | Success(z,_,_) -> 
        let (ts,ssl) = z.SelectorGroups.Head.Head
        Assert.Equal("html",(match ts.Element with Name(str) -> str))

[<Fact>]
let ``parse sample 4`` () =
    let str = "
    
    html {
      line-height: 1.15; /* 1 */
      -webkit-text-size-adjust: 100%; /* 2 */
    }
    
    details {
      display: block;
    }"
    let result = run parseCss str
    match result with
    | Success(z,_,_) -> 
        match z with
        | [l1;l2] -> 
             Assert.Equal("html",(match (fst l1.SelectorGroups.Head.Head).Element with Name(str) -> str))
             Assert.Equal("details",(match (fst l2.SelectorGroups.Head.Head).Element with Name(str) -> str))


[<Fact>]
let ``parse sample 5`` () =
    let str = """button:-moz-focusring,
    [type="button"]:-moz-focusring,
    [type="reset"]:-moz-focusring,
    [type="submit"]:-moz-focusring {
      outline: 1px dotted ButtonText;
    }"""
    let result = run parseDefinition str
    assert_success result

[<Fact>]
let  ``parse sample 6`` () =
    let str = "
        @media (min-width: 576px) {
          .container {
            max-width: 576px;
          }
        }"
    let result = run parseMediaSection str
    assert_success result

[<Fact>]
let ``tailwind`` () =
    let testText = File.ReadAllText("../../../../test/tailwind.css")
    let result = run parseCss testText 
    match result with
    | Success(z,_,_) -> Assert.True true
    | Failure(x,y,z) -> Assert.True false


[<Fact>]
let ``bootstrap`` () =
    let testText = File.ReadAllText("../../../../test/bootstrap.css")
    let result = run parseCss testText 
    assert_success result

[<Fact>]
let ``tailwind graphs`` () =
    //let testText = File.ReadAllText("../../../../test/tailwind.css")
    let result = CssProcesser.makeGraphFromCss "../../../../test/tailwind.css"
    Assert.NotEmpty result