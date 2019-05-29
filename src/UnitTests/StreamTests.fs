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
    testList "stream tests" [


        test "pchar" {
            match stream "sa" with
            | PChar 's' left -> Expect.equal (left.Head()) (Some('a')) "pchar leaves rest"
        }

        test "between" {
            match stream "asa" with
            | Between 'a' (result,left) -> Expect.equal result (Some([|'s'|])) "between takes inside"; Expect.equal (left.Head()) None "nothing left"
        }
    ]