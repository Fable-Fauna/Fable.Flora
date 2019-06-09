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
    Stream<char>(model,10)

[<Tests>]
let tests =
    ftestList "stream tests" [


        test "pchar" {
            match stream "sa" with
            | PChar 's' left -> Expect.equal (left.Head()) (Some('a')) "pchar leaves rest"
        }

        test "pstring" {
            let result = (|PString|_|) "sa" (stream "sa")
            Expect.equal (result.Value.Head()) (None) "pstring can take two"
        }

        test "between" {
            match stream "asa" with
            | Between 'a' (result,left) -> Expect.equal result [|'s'|] "between takes inside"; Expect.equal (left.Head()) None "nothing left"
        }
        test "between 2" {
            match stream "assas" with
            | Between 'a' (result,left) -> Expect.equal result [|'s'; 's'|] "between takes inside"; Expect.equal (left.Head()) (Some('s')) "nothing left"
        }

        test "whitespace" {
          let result = (|Whitespace|_|) (stream "  sa")

          Expect.isTrue result.IsSome "takes stream"
          Expect.equal ((snd result.Value).Head().Value) 's' "whitespace consumes correctly"
        }

    ]