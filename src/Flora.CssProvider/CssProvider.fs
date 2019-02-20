// ts2fable 0.6.1
namespace Flora.CssProvider
open System
open Fable.Core
open Fable.Import.JS
open Fable.Import.Browser
open Microsoft.FSharp.Core.CompilerServices
open System.IO
open FParsec
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open ProviderImplementation


module CssProviderHelpers =
    open CssProcesser

    let (|Singleton|) = function [l] -> l | _ -> failwith "Parameter mismatch"

    let rec makeType (g : Graph) =
        let t = ProvidedTypeDefinition(g.Name, baseType = Some typeof<obj>, hideObjectMethods = true, isErased = true)
        if g.Leaf.IsSome then
                let valueProp = 
                    ProvidedProperty(propertyName = "Value", 
                                  propertyType = typeof<string>,
                                  isStatic = true,
                                  getterCode = (fun _ -> Expr.Value g.Leaf.Value))  
                t.AddMember(valueProp)       

        for child in g.Children do
            if child.Leaf.IsSome && Array.isEmpty child.Children then
                let valueProp = 
                    ProvidedProperty(propertyName = child.Name, 
                                  propertyType = typeof<string>,
                                  isStatic = true,
                                  getterCode = (fun _ -> Expr.Value child.Leaf.Value ))  
                t.AddMember(valueProp)
            else 
                let subtype = makeType child
                t.AddMember(subtype)       

        t               

open CssProviderHelpers

[<TypeProvider>]
type public CssProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)
    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "Flora"

    let staticParams = [ProvidedStaticParameter("file",typeof<string>)]
    let generator = ProvidedTypeDefinition(asm, ns, "Stylesheet", Some typeof<obj>, isErased = true)

    do generator.DefineStaticParameters(
        parameters = staticParams,
        instantiationFunction = 
            (fun typeName pVals ->
                match pVals with 
                | [| :? string as file|] -> 
                    let graphs = CssProcesser.makeGraphFromCss file
                    //failwith (sprintf "graphs") 
                    
                    let root = 
                        ProvidedTypeDefinition(asm, ns, typeName, baseType = Some typeof<obj>, hideObjectMethods = true, isErased = true)

                      

                    for graph in graphs do
                        let t = makeType graph
                        root.AddMember(t)
                    root
                | _ -> failwith "unexpected parameter values"                
            )
        )

    do this.AddNamespace(ns, [generator])

[<assembly:TypeProviderAssembly>]
do ()
