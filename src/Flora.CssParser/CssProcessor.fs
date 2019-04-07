module CssProcesser

open CssProvider.Parser
open System.IO
open FParsec

let procCss (css : Definition list) = 
    let q = seq {
        for x in css do
            for y in x.SelectorGroups do
                for (typ,ls) in y do
                    for l in ls do
                        match l with
                        | SimpleSelector.Class(cls) -> 
                            yield typ.Element, (cls.Split('-') |> Array.toList, cls)
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
    let result = run parseCss testText
    match result with
    | Success(defs,_,_) ->
        let a = procCss defs
        a |> Array.map (fun (x,y) -> 
            { Leaf = None
              Name = x
              Children = produceGraph y
            })
    | Failure(err,perr,_) -> failwith err
