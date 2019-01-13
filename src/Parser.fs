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

    and Match =
        | Includes // ~=
        | Dash // |=
        | Prefix // ^=
        | Suffix // $=
        | Substring // *=
        | Equal // =

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
        SelectorGroup list  


    //OTHER

    let parseComment =
        skipString "/*" >>. skipCharsTillStringCI "*/" true Int32.MaxValue

    let whitespace =
        attempt (spaces .>> parseComment >>. spaces)
        <|> 
        spaces


    //SELECTORS
    let identifier = (satisfy isLetter <|> pchar '-') |> many1Chars

    let parseClass = pchar '.' >>. identifier |>> Class

    let parseId = pchar '#' >>. identifier |>> Id


    let parseNamespaceSelector =
        ((identifier |>> NamespaceSelector.Name 
        <|> (pchar '*' >>% NamespaceSelector.All)
        <|> preturn NamespaceSelector.Empty)
        .>> pchar '|') 
        <|> preturn NamespaceSelector.All

    let parseElementSelector = 
        identifier |>> ElementSelector.Name
        <|> (pchar '*' >>% ElementSelector.All)
        <|> preturn ElementSelector.All

    let parseTypeSelector = 
        parseNamespaceSelector .>>. parseElementSelector 
        |>> (fun (x,y) -> { Element = y; Namespace = x})
        

    let parseMatch =
        (pstring "~=" >>% Match.Includes)
        <|> (pstring "|=" >>% Match.Dash)
        <|> (pstring "^=" >>% Match.Prefix)
        <|> (pstring "$=" >>% Match.Suffix)
        <|> (pstring "*=" >>% Match.Substring)
        <|> (pchar '=' >>% Match.Equal)

   
    let parseAttribute =
        pchar '[' >>. whitespace >>. parseNamespaceSelector .>>. identifier .>> whitespace
        .>>. opt (parseMatch .>> whitespace  .>>. identifier (* or string*) ) .>> whitespace .>> pchar ']'
    //parse pseudo

    //parse negation


    let parseSimpleSelector = attempt parseClass <|> attempt parseId

        

    let parseSelectorSeq = 
        parseTypeSelector .>>. many parseSimpleSelector |>> SelectorSeq


    let parseCombinator =
        (pchar '>' >>% Child)
        <|> (pchar '*' >>% Desendent)
        <|> (pchar '+' >>% Next)
        

    let parseSelector :Parser<SelectorGroup,unit> =     //combinator selectorSeq
        sepBy1 (whitespace >>. parseSelectorSeq .>> whitespace) parseCombinator

    let parseSelectorGroup  = 
        sepBy1 parseSelector (pchar ',' .>> whitespace) 
    



    //RULES

 
    let parseDefinition :Parser<Definition,unit> =
        (parseSelectorGroup .>> whitespace) .>> (pchar '{' >>. skipCharsTillStringCI "}" true Int32.MaxValue) 
        
    let parseCss : Parser<Definition list, unit>= 
        many1 (attempt (whitespace >>. parseDefinition))

