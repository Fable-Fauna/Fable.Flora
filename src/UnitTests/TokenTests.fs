module TokenTests

open System
open Xunit
open Tokenizer
open Expecto
open System.IO
open Tests
open CssProvider.ParseShaper
open CssProvider.SelectorsParser
open Stream

//[<Theory>]
//[<InlineData("\"letters\"")>]
//[<InlineData("'letters'")>]
////[<InlineData("""""")>]
////[<InlineData("""sbc""a""")>]
//let ``string token`` (str : string) =
//    match tokenStream str with
//    | [Token.String(a)] -> true
//    | _ -> false

//[<Theory>]
//[<InlineData("/* comment */")>]
//[<InlineData(" ")>]
//[<InlineData("  ")>]
//let ``whitespace token`` (str : string) =
//    match tokenStream str with
//    | [Token.Whitespace(a)] -> true
//    | _ -> false

//[<Tests>]
//let tests =
//    testList "token tests" [
//        test "string token" {
//            match "\"letters\"" with
//            | StringToken (str,left) -> Expect.equal str "letters" "string of letters"
//        };

//        test "pchar" {
//            match "sa" with
//            | PChar 's' left -> Expect.equal left "a" "pchar leaves rest"
//        }

//        test "between" {
//            match "asa" with
//            | Between 'a' (result,left) -> Expect.equal result "s" "between takes inside"; Expect.equal left "" "nothing left"
//        }
//    ]


[<Tests>]
let AccepectenceTests =
    testList "Acceptence Tests" [
        test "tailwind" {
            let testText = File.ReadAllText("../../../../../test/tailwind.css")
           
            let ptree = parseCss testText
            Expect.isTrue (ptree.Length > 1000) (sprintf "token length: %i"  ptree.Length)
        };

        test "bulma" {
            let testText = File.ReadAllText("../../../../../test/bulma.css")
            let ptree = parseCss testText
            Expect.isTrue (ptree.Length > 1000) (sprintf "token length: %i"  ptree.Length)
        }

        test "bootstrap" {
            let testText = File.ReadAllText("../../../../../test/bootstrap.css")
            let ptree = parseCss testText
            Expect.isTrue (ptree.Length > 1000) (sprintf "token length: %i"  ptree.Length)
        }


    ]

//[<Tests>]
//let ShapeTests =
//    testList "Shape Tests" [
//        test "tailwind" {
//            let testText = ".select-none {
//            -webkit-user-select: none;
//               -moz-user-select: none;
//                -ms-user-select: none;
//                    user-select: none;
//}

//.select-text {
//            -webkit-user-select: text;
//               -moz-user-select: text;
//                -ms-user-select: text;
//                    user-select: text;
//}
//"
//            let tref = ref (testText.ToCharArray())
//            let stream = Stream(tref,20) :> IStream<char>
//            let tstream = tokenStream stream
//            let pshape = parseShape tstream
//            Expect.isTrue (pshape.Length = 2) (sprintf "token length: %i"  tstream.Length)
//        };

       
//    ]

[<Tests>]
let ParseTests =
    testList "Parse Tests" [
        test "tailwind" {
            let testText = ".select-none {
            -webkit-user-select: none;
               -moz-user-select: none;
                -ms-user-select: none;
                    user-select: none;
}

.select-text {
            -webkit-user-select: text;
               -moz-user-select: text;
                -ms-user-select: text;
                    user-select: text;
}
"
            let ptree = parseCss testText
            Expect.isTrue (ptree.Length = 2) (sprintf "token length: %i"  ptree.Length)
        };

       
    ]