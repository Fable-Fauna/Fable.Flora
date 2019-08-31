// ts2fable 0.6.1

namespace Fable.Core

type EmitAttribute(macro: string) =
    inherit System.Attribute()

module Util =
  let inline jsNative<'T> : 'T =
      try failwith "JS only" // try/catch is just for padding so it doesn't get optimized
      with ex -> raise ex


namespace Flora.CssProvider
open System
open Microsoft.FSharp.Core.CompilerServices
open System.IO
open ProviderImplementation.ProvidedTypes
open FSharp.Quotations
open ProviderImplementation
open System.Net
open System.Net.Http

module internal Internal =
  type Key =
    { File : string
      Mode : CssProcesser.Strategy
      Fable : bool
    }
  let cache = System.Collections.Concurrent.ConcurrentDictionary<Key,ProvidedTypeDefinition>()
  let fileWatcher = new FileSystemWatcher()

module CssProviderHelpers =
    open CssProcesser

    let (|Singleton|) = function [l] -> l | _ -> failwith "Parameter mismatch"

    let rec makeType (g : Graph) (t : ProvidedTypeDefinition) =
        match g with
        | Class(cls) ->
          let valueProp =
              ProvidedProperty(propertyName = cls.Name,
                            propertyType = typeof<string>,
                            isStatic = true,
                            getterCode = (fun _ -> Expr.Value cls.ClassName ))
          valueProp.AddXmlDoc cls.ClassName
          t.AddMember(valueProp)


        | Node(name,graphs) ->
          let nt = ProvidedTypeDefinition(name, baseType = Some typeof<obj>, hideObjectMethods = true, isErased = true)
          for child in graphs do
              makeType child nt
              t.AddMember(nt)

    open Fable.Core

    [<Emit("document.documentElement.style.setProperty('$0', '$1');")>]
    let setCssVariable (name : string) (value : string) : unit = Util.jsNative

    [<Emit("window.getComputedStyle(document.documentElement).getPropertyValue('$0');")>]
    let getCssVariable (name : string) : string = Util.jsNative

    let getterCode name =
      fun (args: Expr list) -> <@@ getCssVariable name @@>

    let setterCode name =
      fun (args: Expr list) -> <@@ setCssVariable name %%args.[0] @@>


    let makeVariables (v : string []) (t : ProvidedTypeDefinition) =
       let nt = ProvidedTypeDefinition("Variables", baseType = Some typeof<obj>, hideObjectMethods = true, isErased = true)

       for var in v do
          let name = var.Remove(0,2)
          let valueProp =
            ProvidedProperty(propertyName = name,
                          propertyType = typeof<string>,
                          isStatic = true,
                          getterCode = getterCode var,
                          setterCode = setterCode var
            )
          valueProp.AddXmlDoc var
          nt.AddMember(valueProp)
       t.AddMember(nt)

type NamingMode = 
  | Verbatim = 0
  | SnakeCase = 1
  | DirectedGraph = 2

open CssProviderHelpers

[<TypeProvider>]
type public CssProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)
    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "Flora"

    let staticParams = 
      [ ProvidedStaticParameter("file",typeof<string>);
        ProvidedStaticParameter("naming", typeof<NamingMode>, parameterDefaultValue = NamingMode.Verbatim ) 
        ProvidedStaticParameter("fable", typeof<bool>, parameterDefaultValue = true)]
    let generator = ProvidedTypeDefinition(asm, ns, "Stylesheet", Some typeof<obj>, isErased = true)
    //TODO add xml doc to generator
    do generator.DefineStaticParameters(
        parameters = staticParams,
        instantiationFunction =
            (fun typeName args ->
                try
                  let key : Internal.Key = { 
                    File = args.[0] :?> string; 
                    Mode = args.[1] :?> CssProcesser.Strategy //lazy casting
                    Fable = args.[2] :?> bool}
                  Internal.cache.GetOrAdd(key, fun key ->
                    let file = key.File
                    let strategy = key.Mode
                    let fable = key.Fable
                    let styl =
                        if file.StartsWith "http://" || file.StartsWith "https://"
                        then
                            // load content using http
                            let content, statusCode =
                                async {
                                    use httpClient = new HttpClient()
                                    match! Async.Catch(Async.AwaitTask (httpClient.GetAsync(file))) with
                                    | Choice1Of2 httpMessage ->
                                        let! content = Async.AwaitTask(httpMessage.Content.ReadAsStringAsync())
                                        return content, int httpMessage.StatusCode
                                    | Choice2Of2 error ->
                                        return error.Message, 0
                                }
                                |> Async.RunSynchronously

                            if statusCode = 200
                            then CssProcesser.makeGraphFromCssContent content strategy
                            else failwithf "Error (%d) while retreiving the external stylesheet from %s\n%s" statusCode file content
                        else

                            Internal.fileWatcher.Path <- Path.GetDirectoryName(file)
                            Internal.fileWatcher.Filter <- Path.GetFileName(file)
                            Internal.fileWatcher.Changed.Add(fun x -> Internal.cache.TryRemove key |> ignore)
                            Internal.fileWatcher.EnableRaisingEvents <- true
                            // just (try to) read locally from file
                            CssProcesser.makeGraphFromCss file strategy

                    //failwith (sprintf "graphs")

                    let root =
                        ProvidedTypeDefinition(asm, ns, typeName, baseType = Some typeof<obj>, hideObjectMethods = true, isErased = true)

                    for graph in styl.Graphs do
                        makeType graph root

                    makeVariables styl.Variables root
                    root)
                with
                | exn -> failwith (sprintf "%s ||| %s ||| %s" exn.Message exn.StackTrace exn.Source)
        )
    )

    do this.AddNamespace(ns, [generator])

[<assembly:TypeProviderAssembly>]
do ()
