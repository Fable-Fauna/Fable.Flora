module GraphTests

open CssProcesser
open Expecto
open System.IO

[<Tests>]
let AccepectenceTests =
    testList "Graph Tests" [
        test "tailwind" {

            let g = makeGraphFromCss "../../../../../test/tailwind.css"
            Expect.isTrue (g.Length > 1000) (sprintf "token length: %i"  g.Length)
        };

        test "bulma" {
            let g = makeGraphFromCss "../../../../../test/bulma.css"
           
            Expect.isTrue (g.Length > 1000) (sprintf "token length: %i"  g.Length)
        }

        test "bootstrap" {
            let g = makeGraphFromCss "../../../../../test/bootstrap.css"
            
            Expect.isTrue (g.Length > 1000) (sprintf "token length: %i"  g.Length)
        }


    ]