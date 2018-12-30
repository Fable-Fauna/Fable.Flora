module Tests

open System
open Xunit
open FParsec
open CssProvider
open Parser
open System.IO

[<Fact>]
let ``stuff`` () =
    let testText = "div .class #id p { margin: 0 auto; } * .btn #foo { background-color: red; }"
    let result = run parseCss testText 
    let success =
      match result with
      | Success(z,_,_) -> true
      | Failure(_,_,_) -> false
    Assert.True success


[<Fact>]
let ``tailwind`` () =
    let testText = File.ReadAllText("../../../../test/tailwind.css")
    let result = run parseCss testText 
    let success =
      match result with
      | Success(z,_,_) -> true
      | Failure(_,_,_) -> false
    Assert.True success


[<Fact>]
let ``bootstrap`` () =
    let testText = File.ReadAllText("../../../../test/bootstrap.css")
    let result = run parseCss testText 
    let success =
      match result with
      | Success(z,_,_) -> true
      | Failure(_,_,_) -> false
    Assert.True success