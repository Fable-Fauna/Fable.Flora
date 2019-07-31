module GraphTests

open CssProcesser
open Expecto
open System.IO
open CssProvider.SelectorsParser

let issue14_highlight = ".is-large.delete, .is-large.modal-close {
  height: 32px;
  max-height: 32px;
  max-width: 32px;
  min-height: 32px;
  min-width: 32px;
  width: 32px;
  }

  .button.is-loading::after, .select.is-loading::after, .control.is-loading::after, .loader {
  -webkit-animation: spinAround 500ms infinite linear;
          animation: spinAround 500ms infinite linear;
  border: 2px solid #dbdbdb;
  border-radius: 290486px;
  border-right-color: transparent;
  border-top-color: transparent;
  content: \"\";
  display: block;
  height: 1em;
  position: relative;
  width: 1em;
  }

  .is-overlay, .image.is-square img,
  .image.is-square .has-ratio, .image.is-1by1 img,
  .image.is-1by1 .has-ratio, .image.is-5by4 img,
  .image.is-5by4 .has-ratio, .image.is-4by3 img,
  .image.is-4by3 .has-ratio, .image.is-3by2 img,
  .image.is-3by2 .has-ratio, .image.is-5by3 img,
  .image.is-5by3 .has-ratio, .image.is-16by9 img,
  .image.is-16by9 .has-ratio, .image.is-2by1 img,
  .image.is-2by1 .has-ratio, .image.is-3by1 img,
  .image.is-3by1 .has-ratio, .image.is-4by5 img,
  .image.is-4by5 .has-ratio, .image.is-3by4 img,
  .image.is-3by4 .has-ratio, .image.is-2by3 img,
  .image.is-2by3 .has-ratio, .image.is-3by5 img,
  .image.is-3by5 .has-ratio, .image.is-9by16 img,
  .image.is-9by16 .has-ratio, .image.is-1by2 img,
  .image.is-1by2 .has-ratio, .image.is-1by3 img,
  .image.is-1by3 .has-ratio, .modal, .modal-background, .hero-video {
  bottom: 0;
  left: 0;
  position: absolute;
  right: 0;
  top: 0;
  }

  .button, .input,
  .textarea, .select select, .file-cta,
  .file-name, .pagination-previous,
  .pagination-next,
  .pagination-link,
  .pagination-ellipsis {
  -moz-appearance: none;
  -webkit-appearance: none;
  align-items: center;
  border: 1px solid transparent;
  border-radius: 4px;
  box-shadow: none;
  display: inline-flex;
  font-size: 1rem;
  height: 2.25em;
  justify-content: flex-start;
  line-height: 1.5;
  padding-bottom: calc(0.375em - 1px);
  padding-left: calc(0.625em - 1px);
  padding-right: calc(0.625em - 1px);
  padding-top: calc(0.375em - 1px);
  position: relative;
  vertical-align: top;
  }"

[<Tests>]
let AccepectenceTests =
    testList "Graph Tests" [
        test "tailwind" {

            let g = makeGraphFromCss "../../../../../test/tailwind.css"
            Expect.isTrue (g.Length = 1) (sprintf "token length: %i"  g.Length)
        };

        test "bulma" {
            let g = makeGraphFromCss "../../../../../test/bulma.css"
           
            Expect.isTrue (g.Length = 7) (sprintf "token length: %i"  g.Length)
        }

        test "bootstrap" {
            let g = makeGraphFromCss "../../../../../test/bootstrap.css"
            
            Expect.isTrue (g.Length = 7) (sprintf "token length: %i"  g.Length)
        }

        test "bulma_1" {
            let defs = parseCss issue14_highlight
            let a = processCss defs
            let g = a |> Array.map (fun (x,y) -> 
                { Leaf = None
                  Name = x
                  Children = produceGraph y
                })
            
            Expect.isTrue (g.Length = 1) (sprintf "token length: %i"  g.Length)
        }


    ]