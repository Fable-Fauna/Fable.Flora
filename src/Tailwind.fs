// ts2fable 0.6.1
namespace Fable.Tailwind
open System
open Fable.Core
open Fable.Import.JS
open Fable.Import.Browser
open Microsoft.FSharp.Core.CompilerServices

open ProviderImplementation.ProvidedTypes


[<TypeProvider>]
type public TailWind (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let asm = System.Reflection.Assembly.GetExecutingAssembly()

    do
        let ns = "Fable.Tailwind"
        let generator = ProvidedTypeDefinition(asm, ns, "TailWind", Some typeof<obj>)
       
        let staticProp = ProvidedProperty(propertyName = "StaticProperty", 
                                  propertyType = typeof<string>, 
                                  isStatic = true,
                                  getterCode = (fun args -> <@@ "Hello!" @@>))
        generator.AddMember staticProp
            
        this.AddNamespace(ns, [generator])

[<assembly:TypeProviderAssembly>]
do ()
