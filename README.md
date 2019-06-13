# Flora.CssParser
A to spec css parser for .NET.

# Flora.CssProvider
A type provider for consumption in both .NET and Fable runtimes to provide class names. And themeing support via auto injecting css varibles.

# Supported Css 
* Tailwind (tested)
* Bootstrap (tested)
* Pure 
* Bulma (tested)
* Milligram
* Semantic-UI
* Foundation
* Material UI
* UIKit

# How to Use

``` fsharp
open Flora.CssProvider

type Bulma = Flora.Stylesheet<"../bulma.css">

let view model dispatch =
  div [ Class Bulma.Any.radio]
      [ str "hello world" ]

```

# Nuget 
https://www.nuget.org/packages/Flora.CssProvider/0.6.0

# Getting Started With Test App

Run `fake build`

Change directory to `test\tailwind-test-app`

Run `npm install`

Run `npm start`

Make some changes to `test\tailwind-test-app\src\App.fs`

Have fun
