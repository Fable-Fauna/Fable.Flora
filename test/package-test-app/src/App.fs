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
type Tailwind = Flora.Stylesheet<"C:/Users/Orlando/Desktop/Projects2019/Fable-Fauna/Fable.Flora/test/bulma.css">

type Model = int

type Msg =
| Increment
| Decrement

let init() : Model = 0

// UPDATE

let update (msg:Msg) (model:Model) =
    match msg with
    | Increment -> model + 1
    | Decrement -> model - 1

// VIEW (rendered with React)

let view (model:Model) dispatch =

  div [ ]
      [ button [ OnClick (fun _ -> dispatch Increment); Class Tailwind.Any.list.item  ] [ str "+" ]
        div [] [ str (string model) ]
        button [ OnClick (fun _ -> dispatch Decrement) ; Class Tailwind.Any.list.Value ] [ str "-" ] ]

// App
Program.mkSimple init update view
|> Program.withReact "elmish-app"
|> Program.withConsoleTrace
|> Program.run
