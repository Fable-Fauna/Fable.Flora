module CssProcesser

open CssProvider
open CssProvider.SelectorsParser
open System.IO

let processCssGraph (css : Rule list) =
    let q = seq {
        for x in css do
          match x with
          | Rule.Qualified(sgl,blk) ->
            let helpText = ParseShaper.printBlockShape blk
            for y in sgl do
              match y with
              | SelectorGroup.Single(sseq) ->
                for s in sseq.Selectors do
                  match s with
                  | SimpleSelector.Class(cls) ->
                      yield sseq.Type.Element, (cls.Split('-') |> Array.toList, cls, helpText)
                  | _ -> ()
              | SelectorGroup.Multiple(group) ->
                for sseq in group.Head :: (group.List |> Array.map (fun (comb,sseq) -> sseq) |> Array.toList ) do
                  for s in sseq.Selectors do
                    match s with
                    | SimpleSelector.Class(cls) ->
                        yield sseq.Type.Element, (cls.Split('-') |> Array.toList, cls, helpText)
                    | _ -> ()

          | Rule.At(name,sgl,blk) -> //need at rules name
            let helpText = ParseShaper.printBlockShape blk
            for y in sgl do
              match y with
              | SelectorGroup.Single(sseq) ->
                for s in sseq.Selectors do
                  match s with
                  | SimpleSelector.Class(cls) ->
                      yield sseq.Type.Element, (name :: (cls.Split('-') |> Array.toList), cls, helpText)
                  | _ -> ()
              | SelectorGroup.Multiple(group) ->
                for sseq in group.Head :: (group.List |> Array.map (fun (comb,sseq) -> sseq) |> Array.toList ) do
                  for s in sseq.Selectors do
                    match s with
                    | SimpleSelector.Class(cls) ->
                        yield sseq.Type.Element, (name :: (cls.Split('-') |> Array.toList), cls, helpText)
                    | _ -> ()
    }
    let n =
        q   |> Seq.groupBy (fst >> (function | ElementSelector.Name(e) -> e | _ -> "Any"))
            |> Seq.map (fun (x,y) -> x, y |> Seq.map snd |> Seq.toArray)
            |> Seq.toArray
    n


type Strategy =
  | Verbatim = 0
  | SnakeCase = 1
  | DirectedGraph = 2

type Class =
    { Name : string
      ClassName : string
      ToolTip : string option
      Body : string
    }

type Graph =
    | Node of string * Graph[]
    | Class of Class

type StyleSheetResult =
  { Graphs : Graph []
    Variables : string [] }

let rec produceGraph (classes :(string list * string * string) []) : Graph [] =
    classes
    |> Seq.groupBy (fun (x,_,_) -> List.head x)
    |> Seq.map (fun (x,y) ->
        let mutable leaf = None
        let flex =
            y
            |> Seq.choose (fun (z,lf,bdy) ->
                match z.Tail with
                | [] -> leaf <- Some(lf,bdy); None
                | tail -> Some(tail,lf,bdy) )
            |> Seq.toArray
        match leaf with
        | Some(n,bdy) ->
            Graph.Class {Name = x; ClassName = n; ToolTip = Some(n); Body = bdy }
        | None -> Graph.Node(x, produceGraph flex)
       )
    |> Seq.toArray
    
let distinctGraph = Seq.distinctBy (function | Node(str,g) -> str | Class(cls) -> cls.ClassName)


let processCss (strategy : Strategy) (content : Rule list) : Graph seq =

  let GraphClass body tooltip name_transform =
    function 
    | SimpleSelector.Class(cls) -> 
      let cls_name = name_transform cls
      Graph.Class { Name = cls_name; ClassName = cls; ToolTip = tooltip; Body = body } |> Option.Some
    | _ -> None

  let GraphClasses sgl fn =
    sgl
    |> Seq.collect (function 
      | SelectorGroup.Single(sseq) -> 
          sseq.Selectors |> Seq.choose fn
      | SelectorGroup.Multiple(group) ->
        group.Head :: (group.List |> Array.map (fun (comb,sseq) -> sseq) |> Array.toList ) 
        |> Seq.collect (fun sseq -> sseq.Selectors |> Seq.choose fn)
      | _ -> Seq.empty<Graph>)
    |> distinctGraph

  match strategy with
  | Strategy.Verbatim ->
    content
    |> Seq.collect (function
      | Rule.Qualified(sgl,blk) ->
        let body = ParseShaper.printBlockShape blk
        GraphClasses sgl (GraphClass body None (fun x -> x))
      | Rule.At(name,sgl,blk) -> //need at rules name
        let body = ParseShaper.printBlockShape blk
        let classes = GraphClasses sgl (GraphClass body None (fun x -> x)) |> Seq.toArray
        seq { yield Graph.Node(name, classes) })
    |> distinctGraph
  | Strategy.SnakeCase ->
    content
    |> Seq.collect (function
      | Rule.Qualified(sgl,blk) ->
        let body = ParseShaper.printBlockShape blk
        GraphClasses sgl (GraphClass body None (fun x -> x.Replace("-","_")))
      | Rule.At(name,sgl,blk) -> //need at rules name
        let body = ParseShaper.printBlockShape blk
        let classes = GraphClasses sgl (GraphClass body None (fun x -> x.Replace("-","_"))) |> Seq.toArray
        seq { yield Graph.Node("@"+name, classes) })
    |> distinctGraph
      
  | Strategy.DirectedGraph ->
    processCssGraph content
    |> Seq.map (fun (y,x) -> Graph.Node(y,produceGraph x))
    |> distinctGraph


/// Creates the CSS graphs from a string containing the css definitions
let makeGraphFromCssContent content strategy =
    let styl = parseCss content
    let graph = processCss strategy styl.Rules |> Seq.toArray
    { Graphs = graph
      Variables = styl.Variables |> Seq.toArray }

/// Creates the CSS graphs from a local file containing the css definitions after reading the contents of the file
let makeGraphFromCss filename strategy =
    let cssContent = File.ReadAllText(filename,System.Text.Encoding.UTF8)
    makeGraphFromCssContent cssContent strategy