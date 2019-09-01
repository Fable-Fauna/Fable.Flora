# Flora.CssParser
A CSS parser for .NET.
Spec Module Level 3 https://www.w3.org/TR/css-syntax-3/

# Flora.CssProvider
A type provider for consumption in both .NET and Fable runtimes to provide class names found in css style sheets. And themeing support via auto injecting css varibles.

## Classes

The default naming scheme is exact. 

Note: Not all overloading issuses have been solved, it works with tailwind though.

``` fsharp
type Tailwind = Flora.Stylesheet<"../tailwind.css">

let view model dispatch =
  div [ Class Tailwind.``bg-green-darkest``]
      [ str "hello world" ]

```

The original naming scheme now called `DirectedGraph` will nest deeper by putting global classes in an any directory. Ex. `Tailwind.Any.``hover:capitalize`` `  Element specific classes are placed in corrsponding directories. Ex.  `Tailwind.div.``hover:capitalize`` `  Can parse any css without name overloading issues.

``` fsharp
type Tailwind = Flora.Stylesheet<"../tailwind.css", naming = NamingMode.DirectedGraph>

let view model dispatch =
  div [ Class Tailwind.Any.``bg-green-darkest``]
      [ str "hello world" ]

```

## Css Variables

When using the type provider with fable, the type provider will create mutable properties with will read and set css varibles to the root document. Calls `document.documentElement.style.setProperty(...)` and `window.getComputedStyle(document.documentElement).getPropertyValue(...)` behind the curtain.


``` fsharp
type VariableTest = Flora.Stylesheet<"../variabletest.css">

type Msg =
| ChangeColor

let update (msg:Msg) (model:Model) =
    match msg with
    | ChangeColor -> Vars.Variables.``main-bg-color`` <- "blue"; model //Set Css Variable found in in variabletest.css

let view (model:Model) dispatch =
  button [ OnClick (fun _ -> dispatch ChangeColor) ; Class Tailwind.Any.``hover:capitalize`` ] 
         [ str ("ChangeColor: " +  Vars.Variables.``main-bg-color``) ] //Retrive Css Variable set in document element.

```


# Nuget 
https://www.nuget.org/packages/Flora.CssProvider/0.7.0

# Getting Started With Test App

Run `fake build`

Change directory to `test\tailwind-test-app`

Run `npm install`

Run `npm start`

Make some changes to `test\tailwind-test-app\src\App.fs`

Have fun
