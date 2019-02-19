namespace CssProvider
open System
open FParsec


module Tokens =

    let comment :Parser<unit,unit> =
        skipString "/*" >>. skipCharsTillStringCI "*/" true Int32.MaxValue

    let newLine :Parser<unit,unit> =
        newline >>% ()
        <|>
        skipChar '\f'

        

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
        | Attribute of (NamespaceSelector * string) * (Match * string) option //[]
        | PsudoClass of string //ident
        | PsudoElement of string //ident 
        | Negation of SimpleSelector
        | TypeSelector of TypeSelector

    and SelectorSeq =
        TypeSelector * SimpleSelector list    

    and Rule =
        string * string

    and Definition =
        { SelectorGroups : SelectorGroup list
          Rules : Rule list}

    and CssRoot =
        | Definition of Definition
        | Media

    //OTHER

    let parseComment =
        pstring "/*" >>. charsTillString "*/" true Int32.MaxValue |>> ignore

    let whitespace =
        spaces .>> opt (skipMany (parseComment .>> spaces))



    //SELECTORS
    let identifier = 
        ((satisfy isLetter <|> pchar '-' <|> satisfy isDigit) |> many1Chars 
        <|> (pstring "\:" >>% ":") <|> (pstring "\/" >>% "/")) |> many1Strings

    let istring = 
        (pchar '"' >>. identifier .>> pchar '"')
        <|>
        (pchar ''' >>. identifier .>> pchar ''')

    let parseClass = pchar '.' >>. identifier |>> Class

    let parseId = pchar '#' >>. identifier |>> Id


    let parseNamespaceSelector =
        attempt (
            (identifier |>> NamespaceSelector.Name 
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
        .>>. opt (parseMatch .>> whitespace  .>>. (identifier <|> istring) ) .>> whitespace .>> pchar ']'
        |>> SimpleSelector.Attribute

    let parsePseudoElement =
        pchar ':' >>. (optional (pchar ':')) >>. identifier |>> SimpleSelector.PsudoElement


    let parseNegation =
        pchar ':' 
        .>> pstringCI "NOT"
        .>> pchar '('
        .>> whitespace
        >>. (
            attempt parseTypeSelector |>> SimpleSelector.TypeSelector
            <|> attempt parseClass
            <|> attempt parseId
            <|> attempt parseAttribute
            <|> attempt parsePseudoElement )
        .>> whitespace
        .>> pchar ')'
        |>> SimpleSelector.Negation


    let parseSimpleSelector = 
        attempt parseClass 
        <|> attempt parseId
        <|> attempt parseAttribute
        <|> attempt parsePseudoElement
        <|> attempt parseNegation

        

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
    
    let parseDefinition :Parser<Definition,unit> =
        (parseSelectorGroup .>> whitespace) 
        .>> (pchar '{' >>. skipCharsTillStringCI "}" true Int32.MaxValue) 
        |>> (fun x -> { SelectorGroups = x; Rules = []})

    //@Media

    let parseCss2 = attempt (many1 (attempt (whitespace >>. parseDefinition)))

    let parseMediaQuery =
        (pchar '(' >>. (identifier .>> pchar ':' >>. skipCharsTillStringCI ")" true 200))

    let parseMediaQueryList  =
        sepBy1 parseMediaQuery (pchar ',' .>> whitespace) 

    let parseMediaSection =
        whitespace >>. pstringCI "@media" >>. whitespace >>. parseMediaQueryList >>. whitespace >>.
        between (pchar '{') (whitespace >>. pchar '}') parseCss2 |>> ignore

    
    //let parseMediaFeature = //plain, boolean, range
        
         
    //RULES

    //let parseRule :Parser<Rule,unit> =
    //    (whitespace >>. identifier .>> pchar ':') 
    //    .>>. (whitespace >>. skipCharsTillStringCI ';')
    
 

        
    let parseCss = 
        many1 (whitespace >>. choice [(parseDefinition |>> CssRoot.Definition); (parseMediaSection >>% CssRoot.Media)] .>> whitespace)
        |>> (fun ls -> ls |> List.choose (function | CssRoot.Definition(x) -> Some(x) | _ -> None))

