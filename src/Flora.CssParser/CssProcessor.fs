module CssProcesser

open CssProvider.SelectorsParser
open System.IO

let processCss (css : Rule list) =
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
              | SelectorGroup.Multiple(group) ->
                for sseq in group.Head :: (group.Ls |> Array.map (fun (comb,sseq) -> sseq) |> Array.toList ) do
                  for s in sseq.Selectors do
                    match s with
                    | SimpleSelector.Class(cls) ->
                        yield sseq.Type.Element, (cls.Split('-') |> Array.toList, cls)
                    | _ -> ()

          | Rule.At(name,sgl,blk) -> //need at rules name
            for y in sgl do
              match y with
              | SelectorGroup.Single(sseq) ->
                for s in sseq.Selectors do
                  match s with
                  | SimpleSelector.Class(cls) ->
                      yield sseq.Type.Element, (name :: (cls.Split('-') |> Array.toList), cls)
                  | _ -> ()
              | SelectorGroup.Multiple(group) ->
                for sseq in group.Head :: (group.Ls |> Array.map (fun (comb,sseq) -> sseq) |> Array.toList ) do
                  for s in sseq.Selectors do
                    match s with
                    | SimpleSelector.Class(cls) ->
                        yield sseq.Type.Element, (name :: (cls.Split('-') |> Array.toList), cls)
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


/// Creates the CSS graphs from a string containing the css definitions
let makeGraphFromCssContent content =
    content
    |> parseCss
    |> processCss
    |> Array.map (fun (x,y) ->
        { Leaf = None
          Name = x
          Children = produceGraph y
        })

/// Creates the CSS graphs from a local file containing the css definitions after reading the contents of the file
let makeGraphFromCss filename =
    let cssContent = File.ReadAllText(filename,System.Text.Encoding.UTF8)
    makeGraphFromCssContent cssContent