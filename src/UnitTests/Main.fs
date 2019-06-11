module Main

open Expecto

[<EntryPoint>]
let main argv =
  let exit = Tests.runTestsInAssembly defaultConfig argv
  System.Console.ReadLine() |> ignore
  exit
