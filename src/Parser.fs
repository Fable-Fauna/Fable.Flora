namespace CssProvider
open System
open FParsec


module Parser =



    type Combinator =
        | Desendent //*
        | Child // >
        | Next // +

    and SelectorGroup =
        SelectorSeq list

    and TypeSelector =
        | Type of string option * string
        | Universial of string option

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



    let identifier = (satisfy isLetter <|> pchar '-') |> many1Chars

    let parseClass = spaces >>. pchar '.' >>. identifier |>> Class

    let parseId = spaces >>. pchar '#' >>. identifier |>> Id

    let parseSimpleSelector = attempt parseClass <|> attempt parseId

    let parseUniversial = 
        (attempt (identifier .>> pchar '|') .>> (pchar '*') |>> Some
        <|> (spaces >>. pchar '*' >>% None)) |>> Universial

    let parseType =
        ((attempt identifier) |>> Some) <|> (pchar '*' >>% None) .>> pchar '|' .>>. identifier
        |>> Type

    let parseSelectorSeq = 
        (attempt parseType <|> parseUniversial) .>>. many parseSimpleSelector |>> SelectorSeq


    let parseCombinator =
        (satisfy (isAnyOf ['>';'*';'+'])) .>> spaces |>> (function | '>' -> Child | '*' -> Desendent | '+' -> Next )


    let parseSelector =     //combinator selectorSeq
        sepBy1 parseSelectorSeq parseCombinator

    let parseSelectorGroup = 
        sepBy1 parseSelector (pchar ',' .>> spaces)
    

    let ruleValue =  (satisfy <| fun c -> c <> ';') |> many1Chars

    let parseRule =
        (identifier .>> spaces) .>>. (pchar ':' >>. spaces >>. ruleValue .>> spaces .>> pchar ';') |>> Rule

    let parseDefinition =
        (parseSelectorGroup .>> spaces) .>>. (pchar '{' >>. spaces >>. many parseRule .>> spaces .>> pchar '}') |>> Definition
        
    let parseCss : Parser<Definition list, unit>= 
        many1 (attempt parseDefinition)

