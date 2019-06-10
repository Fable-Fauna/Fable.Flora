module StreamTests

open System
open Xunit
open Tokenizer
open Expecto
open System.IO
open Tests
open Stream

let stream str =
    let model = ref (String.op_Implicit(str).ToArray())
    Stream<char>(model,10) :> IStream<char>

[<Tests>]
let tests =
    testList "stream tests" [


        test "pchar" {
            match stream "sa" with
            | PChar 's' left -> Expect.equal ((snd left).Head()) (Some('a')) "pchar leaves rest"
        }

        test "pstring" {
            let result = (|PString|_|) "sa" (stream "sa")
            Expect.equal ((snd result.Value).Head()) (None) "pstring can take two"
        }

        test "subsearch" {
          match (stream "a").SubSearch ((=),"a".ToCharArray()) with
          | Some(position) -> Expect.equal position 0 "finds 0"
        }

        test "subsearch 1" {
          match (stream "sa").SubSearch ((=),"a".ToCharArray()) with
          | Some(position) -> Expect.equal position 1 "finds 1"
        }

        test "subsearch 2" {
          match (stream "saa").SubSearch ((=),"aa".ToCharArray()) with
          | Some(position) -> Expect.equal position 1 "finds 1"
        }


        test "between" {
            match stream "asa" with
            | Between "a" (result,left) -> Expect.equal result [|'s'|] "between takes inside"; Expect.equal (left.Head()) None "nothing left"
        }
        test "between 2" {
            match stream "assas" with
            | Between "a" (result,left) -> Expect.equal result [|'s'; 's'|] "between takes inside"; Expect.equal (left.Head()) (Some('s')) "nothing left"
        }

        test "whitespace" {
          let result = (|Whitespace|_|) (stream "  sa")

          Expect.isTrue result.IsSome "takes stream"
          Expect.equal ((snd result.Value).Head().Value) 's' "whitespace consumes correctly"
        }

        

        test "whitespace 2" {
          let str = (stream "/*! normalize.css v8.0.1 | MIT License | github.com/necolas/normalize.css */")
          match str with
          | Whitespace(Token.Whitespace(cmt),left) -> 
            Expect.equal cmt ["! normalize.css v8.0.1 | MIT License | github.com/necolas/normalize.css "] "correct comment"
            Expect.isTrue (left.Head().IsNone) "empty stream"

        }

        test "whitespace 3" {
          let str = (stream "/*!
          * Bootstrap v4.2.1 (https://getbootstrap.com/)
          * Copyright 2011-2018 The Bootstrap Authors
          * Copyright 2011-2018 Twitter, Inc.
          * Licensed under MIT (https://github.com/twbs/bootstrap/blob/master/LICENSE)
          */ ")
          match str with
          | Whitespace(Token.Whitespace(cmt),left) -> 
            //Expect.equal cmt ["!
            //* Bootstrap v4.2.1 (https://getbootstrap.com/)
            //* Copyright 2011-2018 The Bootstrap Authors
            //* Copyright 2011-2018 Twitter, Inc.
            //* Licensed under MIT (https://github.com/twbs/bootstrap/blob/master/LICENSE)
            //"] "correct comment"
            Expect.isTrue (left.Head().IsNone) "empty stream"

        }

    ]