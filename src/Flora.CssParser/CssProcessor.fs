module CssProcesser

open CssProvider.SelectorsParser
open System.IO
open FParsec

let procCss (css : Rule list) = 
    let q = seq {
        for x in css do
          match x with
          | Rule.Qualified(sgl,blk) ->
            for y in sgl do
              match y with
              | SelectorGroup.Single(sseq) ->
                for s in sseq.Selectors do
                  match s with
                  | SimpleSelector.Class(cls) -> 
                      yield sseq.Type.Element, (cls.Split('-') |> Array.toList, cls)
                  | _ -> ()

          | Rule.At(name,sgl,blk) ->
            for y in sgl do
              match y with
              | SelectorGroup.Single(sseq) ->
                for s in sseq.Selectors do
                  match s with
                  | SimpleSelector.Class(cls) -> 
                      yield sseq.Type.Element, (cls.Split('-') |> Array.toList, cls)
                  | _ -> ()
    }
    let n = 
        q   |> Seq.groupBy (fst >> (function | ElementSelector.Name(e) -> e | _ -> "Any"))
            |> Seq.map (fun (x,y) -> x, y |> Seq.map snd |> Seq.toArray) 
            |> Seq.toArray
    n

type Graph =
    { Leaf : string option
      Name : string
      Children : Graph [] }



let rec produceGraph (classes :(string list * string) []) : Graph [] =
    classes 
    |> Seq.groupBy (fst >> List.head)
    |> Seq.map (fun (x,y) -> 
        let mutable leaf = None
        let flex = 
            y 
            |> Seq.choose (fun (z,lf) -> 
                match z.Tail with
                | [] -> leaf <- Some(lf); None
                | tail -> Some(tail,lf) )
            |> Seq.toArray
        { Name = x
          Leaf = leaf
          Children = produceGraph flex })
    |> Seq.toArray

let makeGraphFromCss filename =
    let testText = File.ReadAllText(filename,System.Text.Encoding.UTF8)
    let defs = parseCss testText
    let a = procCss defs
    a |> Array.map (fun (x,y) -> 
        { Leaf = None
          Name = x
          Children = produceGraph y
        })

