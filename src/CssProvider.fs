// ts2fable 0.6.1
namespace Fable.CssClassProvider
open System
open Fable.Core
open Fable.Import.JS
open Fable.Import.Browser
open Microsoft.FSharp.Core.CompilerServices
open System.IO
open FParsec
open ProviderImplementation.ProvidedTypes

open CssProvider.Parser

module CssProcesser =
    let procCss (css : Definition list) = 
        let q = seq {
            for x in css do
                for y in x do
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
          Children : Graph []
        }



    let rec produceGraph (classes :(string list * string) seq) : Graph [] =
        classes 
        |> Seq.groupBy (fun x -> List.head (fst x) )
        |> Seq.map (fun (x,y) -> 
            let mutable leaf = None
            let flex = 
                seq { 
                    for (z,zz) in y do
                        match z with
                        | h::[] -> leaf <- Some(zz)
                        | h::tail -> yield tail,zz
                }
            {Name = x
             Leaf = leaf
             Children = produceGraph flex })
        |> Seq.toArray

    let makeGraphFromCss filename =
        let testText = File.ReadAllText(filename)
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

//module CssProviderHelpers =



[<TypeProvider>]
type public Css (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)
    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "Fable.CssClassProvider"

    let staticParams = [ProvidedStaticParameter("file",typeof<string>)]
    let generator = ProvidedTypeDefinition(asm, ns, "Css", Some typeof<obj>)

    do generator.DefineStaticParameters(
        parameters = staticParams,
        instantiationFunction = 
            (fun typeName pVals ->
                match pVals with 
                | [| :? string as file|] -> 
                    let graphs = CssProcesser.makeGraphFromCss file

                    
                    let root = 
                        ProvidedTypeDefinition(typeName, baseType = Some typeof<obj>)

                    for graph in graphs do
                        //make type recursively

                        let staticProp = 
                            ProvidedProperty(propertyName = graph.Name, 
                                          propertyType = typeof<string>, //generated static type
                                          isStatic = true,
                                          getterCode = (fun args -> <@@ "Hello!" @@>))  
                        root.AddMember(staticProp)                   
                    root
            )
        )
        
        
       
        // let staticProp = ProvidedProperty(propertyName = "StaticProperty", 
        //                                   propertyType = typeof<string>, 
        //                                   isStatic = true,
        //                                   getterCode = (fun args -> <@@ "Hello!" @@>))
        // generator.AddMember staticProp
            
    do this.AddNamespace(ns, [generator])



[<assembly:TypeProviderAssembly>]
do ()
