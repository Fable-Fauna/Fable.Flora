namespace CssProvider
open System
open FParsec


module Parser =

    type NamespaceSelector =
        | All
        | Empty
        | Name of string

    type ElementSelector =
        | All
        | Name of string

    type Combinator =
        | Desendent //*
        | Child // >
        | Next // +

    and SelectorGroup =
        SelectorSeq list

    and TypeSelector =
        { Element : ElementSelector
          Namespace : NamespaceSelector}


    and SimpleSelector =
        | Class of string
        | Id of string
        | Attribute of string //[]
        | PsudoClass of string //ident
        | PsudoElement of string //ident    

    and SelectorSeq =
        TypeSelector * SimpleSelector list    

    and Rule =
        string * string

    and Definition =
        SelectorGroup list * Rule list    


    //OTHER

    let parseComment =
        skipString "/*" >>. skipCharsTillStringCI "*/" true Int32.MaxValue

    let whitespace =
        spaces 
        <|> parseComment >>. spaces


    //SELECTORS
    let identifier = (satisfy isLetter <|> pchar '-') |> many1Chars

    let parseClass = pchar '.' >>. identifier |>> Class

    let parseId = pchar '#' >>. identifier |>> Id

    let parseSimpleSelector = attempt parseClass <|> attempt parseId

    let parseNamespaceSelector =
        attempt identifier |>> NamespaceSelector.Name
        <|> (attempt (pchar '*') >>% NamespaceSelector.All)
        <|> (preturn NamespaceSelector.Empty)

    let parseElementSelector = 
        attempt identifier |>> ElementSelector.Name
        <|> (pchar '*' >>% ElementSelector.All)


    let parseTypeSelector = 
        attempt ((parseNamespaceSelector .>> pchar '|') .>>. parseElementSelector |>> (fun (x,y) -> { Element = y; Namespace = x}))
        <|> (parseElementSelector |>> (fun x -> { Namespace = NamespaceSelector.All; Element = x}))
        <|> (preturn { Namespace = NamespaceSelector.All; Element = ElementSelector.All } )

        

    let parseSelectorSeq = 
        parseTypeSelector .>>. many parseSimpleSelector |>> SelectorSeq


    let parseCombinator =
        (satisfy (isAnyOf ['>';'*';'+'])) |>> (function | '>' -> Child | '*' -> Desendent | '+' -> Next )


    let parseSelector =     //combinator selectorSeq
        sepBy1 (whitespace >>. parseSelectorSeq .>> whitespace) parseCombinator

    let parseSelectorGroup = 
        sepBy1 parseSelector (pchar ',' .>> whitespace)
    



    //RULES

    let ruleValue =  (satisfy <| fun c -> c <> ';') |> many1Chars

    let parseRule =
        (identifier .>> spaces) .>>. (pchar ':' >>. spaces >>. ruleValue .>> spaces .>> pchar ';') |>> Rule

    let parseDefinition =
        (parseSelectorGroup .>> spaces) .>>. (pchar '{' >>. spaces >>. many parseRule .>> spaces .>> pchar '}') |>> Definition
        
    let parseCss : Parser<Definition list, unit>= 
        many1 (attempt parseDefinition)

