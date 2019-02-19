// ts2fable 0.6.1
namespace Fable.CssProvider
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
        let t = ProvidedTypeDefinition(g.Name, baseType = Some typeof<obj>, hideObjectMethods = true, isErased = false)
        if g.Leaf.IsSome then
                let valueProp = 
                    ProvidedProperty(propertyName = "Value", 
                                  propertyType = typeof<string>, //generated static type
                                  isStatic = true,
                                  getterCode = (fun args -> <@@ g.Leaf.Value @@>))  
                t.AddMember(valueProp)       

        for child in g.Children do
            if child.Leaf.IsSome && Array.isEmpty child.Children then
                let valueProp = 
                    ProvidedProperty(propertyName = child.Name, 
                                  propertyType = typeof<string>, //generated static type
                                  isStatic = true,
                                  getterCode = (fun args -> <@@ child.Leaf.Value @@>))  
                t.AddMember(valueProp)
            else 
                let subtype = makeType child
                
                let prop = 
                    ProvidedProperty( propertyName = child.Name, 
                                      propertyType = subtype,
                                      isStatic = true,
                                      getterCode = (fun (Singleton doc) -> doc ))  
                t.AddMember(subtype)       

        t               

open CssProviderHelpers

[<TypeProvider>]
type public CssProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)
    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "Proto"

    let staticParams = [ProvidedStaticParameter("file",typeof<string>)]
    let generator = ProvidedTypeDefinition(asm, ns, "Css", Some typeof<obj>, isErased = false)

    do generator.DefineStaticParameters(
        parameters = staticParams,
        instantiationFunction = 
            (fun typeName pVals ->
                match pVals with 
                | [| :? string as file|] -> 
                    let graphs = CssProcesser.makeGraphFromCss file
                    //failwith (sprintf "%O" graphs) its empty
                    
                    let root = 
                        ProvidedTypeDefinition(asm, ns, typeName, baseType = Some typeof<obj>, hideObjectMethods = true, isErased = false)

                      

                    for graph in graphs do
                        let t = makeType graph

                        let staticProp = 
                            ProvidedProperty(propertyName = graph.Name, 
                                          propertyType = t, //generated static type
                                          isStatic = true,
                                          getterCode = (fun (Singleton doc) -> doc))  
                        //root.AddMember(staticProp)     
                        root.AddMember(t)
                    root
                | _ -> failwith "unexpected parameter values"                
            )
        )

    do this.AddNamespace(ns, [generator])



[<assembly:TypeProviderAssembly>]
do ()
