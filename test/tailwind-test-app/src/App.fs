module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Emish architecture and samples at https://elmish.github.io/
*)

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props



// MODEL
type Tailwind = Flora.Stylesheet<"C:/Users/Orlando/Desktop/Projects2019/Fable-Fauna/Fable.Flora/test/tailwind.css", naming = Flora.CssProvider.NamingMode.DirectedGraph>

type Bulma = Flora.Stylesheet<"C:/Users/Orlando/Desktop/Projects2019/Fable-Fauna/Fable.Flora/test/bulma.css", naming = Flora.CssProvider.NamingMode.DirectedGraph>

type Tailwind2 = Flora.Stylesheet<"C:/Users/Orlando/Desktop/Projects2019/Fable-Fauna/Fable.Flora/test/tailwind.css">

//type Bulma2 = Flora.Stylesheet<"C:/Users/Orlando/Desktop/Projects2019/Fable-Fauna/Fable.Flora/test/bulma.css"> //failing do to overloads

type Vars = Flora.Stylesheet<"C:/Users/Orlando/Desktop/Projects2019/Fable-Fauna/Fable.Flora/test/varablestest.css">

let c1 = Bulma.Any.tile

type Model = int

type Msg =
| Increment
| Decrement
| ChangeColor

let init() : Model = 0

// UPDATE

let update (msg:Msg) (model:Model) =
    match msg with
    | Increment -> model + 1
    | Decrement -> model - 1
    | ChangeColor -> Vars.Variables.``main-bg-color`` <- "blue"; model

// VIEW (rendered with React)

let view (model:Model) dispatch =

  div [ ]
      [ button [ OnClick (fun _ -> dispatch Increment); Class Tailwind2.``bg-green-darkest`` ] [ str "+" ]
        div [] [ str (string model) ]
        button [ OnClick (fun _ -> dispatch Decrement) ; Class Tailwind.Any.``hover:capitalize`` ] [ str "-" ]
        button [ OnClick (fun _ -> dispatch ChangeColor) ; Class Tailwind.Any.``hover:capitalize`` ] [ str ("ChangeColor:" +  Vars.Variables.``main-bg-color``) ]]

  

// App
Program.mkSimple init update view
|> Program.withReact "elmish-app"
|> Program.withConsoleTrace
|> Program.run
