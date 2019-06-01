namespace CssProvider
open System
open FParsec



        

//module Parser =

//    type NamespaceSelector =
//           | All
//           | Empty
//           | Name of string

//       type ElementSelector =
//           | All
//           | Name of string

//       type Combinator =
//           | Desendent //*
//           | Child // >
//           | Next // +

//       and Match =
//           | Includes // ~=
//           | Dash // |=
//           | Prefix // ^=
//           | Suffix // $=
//           | Substring // *=
//           | Equal // =



//       and TypeSelector =
//           { Element : ElementSelector
//             Namespace : NamespaceSelector}


//       and SimpleSelector =
//           | Class of string
//           | Id of string
//           | Attribute of (NamespaceSelector * string) * (Match * string) option //[]
//           | PsudoClass of string //ident
//           | PsudoElement of string //ident 
//           | Negation of SimpleSelector
//           | TypeSelector of TypeSelector

//       and SelectorSeq =
//           TypeSelector * SimpleSelector list   

//    let parseClass = pchar '.' >>. identifier |>> Class

//    let parseId = pchar '#' >>. identifier |>> Id


//    let parseNamespaceSelector =
//        attempt (
//            (identifier |>> NamespaceSelector.Name 
//            <|> (pchar '*' >>% NamespaceSelector.All)
//            <|> preturn NamespaceSelector.Empty)
//            .>> pchar '|') 
//        <|> preturn NamespaceSelector.All

//    let parseElementSelector = 
//        identifier |>> ElementSelector.Name
//        <|> (pchar '*' >>% ElementSelector.All)
//        <|> preturn ElementSelector.All

//    let parseTypeSelector = 
//        parseNamespaceSelector .>>. parseElementSelector 
//        |>> (fun (x,y) -> { Element = y; Namespace = x})
        
         
//    let parseMatch =
//        (pstring "~=" >>% Match.Includes)
//        <|> (pstring "|=" >>% Match.Dash)
//        <|> (pstring "^=" >>% Match.Prefix)
//        <|> (pstring "$=" >>% Match.Suffix)
//        <|> (pstring "*=" >>% Match.Substring)
//        <|> (pchar '=' >>% Match.Equal)

   
//    let parseAttribute =
//        pchar '[' >>. whitespace >>. parseNamespaceSelector .>>. identifier .>> whitespace
//        .>>. opt (parseMatch .>> whitespace  .>>. (identifier <|> istring) ) .>> whitespace .>> pchar ']'
//        |>> SimpleSelector.Attribute

//    let parsePseudoElement =
//        pchar ':' >>. (optional (pchar ':')) >>. identifier |>> SimpleSelector.PsudoElement


//    let parseNegation =
//        pchar ':' 
//        .>> pstringCI "NOT"
//        .>> pchar '('
//        .>> whitespace
//        >>. (
//            attempt parseTypeSelector |>> SimpleSelector.TypeSelector
//            <|> attempt parseClass
//            <|> attempt parseId
//            <|> attempt parseAttribute
//            <|> attempt parsePseudoElement )
//        .>> whitespace
//        .>> pchar ')'
//        |>> SimpleSelector.Negation


//    let parseSimpleSelector = 
//        attempt parseClass 
//        <|> attempt parseId
//        <|> attempt parseAttribute
//        <|> attempt parsePseudoElement
//        <|> attempt parseNegation

        

//    let parseSelectorSeq = 
//        parseTypeSelector .>>. many parseSimpleSelector |>> SelectorSeq


//    let parseCombinator =
//        (pchar '>' >>% Child)
//        <|> (pchar '*' >>% Desendent)
//        <|> (pchar '+' >>% Next)
        

//    let parseSelector :Parser<SelectorGroup,unit> =     //combinator selectorSeq
//        sepBy1 (whitespace >>. parseSelectorSeq .>> whitespace) parseCombinator

//    let parseSelectorGroup  = 
//        sepBy1 parseSelector (pchar ',' .>> whitespace) 
    



module ParseShaper =
    open Tokenizer

    type StylesheetShape =
        RuleShape list
    
    and RuleShape =
        | Qualified of ComponentShape list * BlockShape
        | At of string * ComponentShape list * BlockShape
    
    and ComponentShape =
        | Preserved of Token
        | SwiggleBlock of BlockShape
        | ParenBlock of BlockShape
        | SquareBlock of BlockShape
        | Function of string * BlockShape
    
    and BlockShape = ComponentShape list    
    


    let looper fn init =
        let mutable loop = true
        let mutable state = init
        while loop do
            match fn state with
            | Some(st),continue -> 
                state <- st
                loop <- continue
            | _ -> loop <- false
        state

    let rec (|CompShape|_|) (input ) =
        match input with
        | Token.Function(name) :: CompShapeList Token.ParenEnd (inner,left) -> Some(ComponentShape.Function(name,inner),left)
        | Token.SquareStart :: CompShapeList Token.SquareEnd (inner,left) -> Some(ComponentShape.SquareBlock(inner),left)
        | Token.SwiggleStart :: CompShapeList Token.SwiggleEnd (inner,left) -> Some(ComponentShape.SwiggleBlock(inner),left)
        | Token.ParenStart :: CompShapeList Token.ParenEnd (inner,left) -> Some(ComponentShape.ParenBlock(inner),left)
        | a :: left -> Some(ComponentShape.Preserved a, left)

    and (|CompShapeList|_|) (terminal : Token) (input : Token list) =
        looper (fun x -> 
            match fst x with 
            | [] -> None,false
            | [a] when a = terminal -> Some([],snd x),false
            | a :: left when a = terminal -> Some(left,snd x),false
            | CompShape(shape,left) -> Some(left, (snd x) @ [shape]),true
            | _ -> failwith "broken shaper") (input,[])
        |> (function | (x,[]) -> None | (x,y) -> Some(y,x))    

    and (|RuleShape|_|) (input) =
        match input with
        | CompShapeList Token.SwiggleStart (inner,CompShapeList Token.SwiggleEnd (inner2, left)) ->
            Some(RuleShape.Qualified(inner,inner2),left)
        | Token.At(name) :: CompShapeList Token.SwiggleStart (inner,CompShapeList Token.SwiggleEnd (inner2, left)) ->
            Some(RuleShape.At(name,inner,inner2),left)
        | _ -> None
            

    let parseShape input : StylesheetShape =
        looper (fun x ->
            match fst x with
            | [] -> None, false
            | RuleShape(r,left) -> Some(left,(snd x) @ [r]),true
            | a :: left -> Some(left,snd x),true //do not preserve tokens not matching rules

            ) (input,[])
        |> snd

module SelectorsParser =
    open Tokenizer
    open ParseShaper

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

    type Stylesheet =
        Rule list
    
    and SelectorGroup =
        SelectorSeq list

    and Rule =
        | Qualified of SelectorGroup list * BlockShape
        | At of string * SelectorGroup list * BlockShape


        //    let parseNamespaceSelector =
        //        attempt (
        //            (identifier |>> NamespaceSelector.Name 
        //            <|> (pchar '*' >>% NamespaceSelector.All)
        //            <|> preturn NamespaceSelector.Empty)
        //            .>> pchar '|') 
        //        <|> preturn NamespaceSelector.All
        
        //    let parseElementSelector = 
        //        identifier |>> ElementSelector.Name
        //        <|> (pchar '*' >>% ElementSelector.All)
        //        <|> preturn ElementSelector.All
        
        //    let parseTypeSelector = 
        //        parseNamespaceSelector .>>. parseElementSelector 
        //        |>> (fun (x,y) -> { Element = y; Namespace = x})
                
                 
        //    let parseMatch =
        //        (pstring "~=" >>% Match.Includes)
        //        <|> (pstring "|=" >>% Match.Dash)
        //        <|> (pstring "^=" >>% Match.Prefix)
        //        <|> (pstring "$=" >>% Match.Suffix)
        //        <|> (pstring "*=" >>% Match.Substring)
        //        <|> (pchar '=' >>% Match.Equal)
        
           
        //    let parseAttribute =
        //        pchar '[' >>. whitespace >>. parseNamespaceSelector .>>. identifier .>> whitespace
        //        .>>. opt (parseMatch .>> whitespace  .>>. (identifier <|> istring) ) .>> whitespace .>> pchar ']'
        //        |>> SimpleSelector.Attribute
        
        //    let parsePseudoElement =
        //        pchar ':' >>. (optional (pchar ':')) >>. identifier |>> SimpleSelector.PsudoElement
        
        
        //    let parseNegation =
        //        pchar ':' 
        //        .>> pstringCI "NOT"
        //        .>> pchar '('
        //        .>> whitespace
        //        >>. (
        //            attempt parseTypeSelector |>> SimpleSelector.TypeSelector
        //            <|> attempt parseClass
        //            <|> attempt parseId
        //            <|> attempt parseAttribute
        //            <|> attempt parsePseudoElement )
        //        .>> whitespace
        //        .>> pchar ')'
        //        |>> SimpleSelector.Negation
        
        
        //    let parseSimpleSelector = 
        //        attempt parseClass 
        //        <|> attempt parseId
        //        <|> attempt parseAttribute
        //        <|> attempt parsePseudoElement
        //        <|> attempt parseNegation
        
                
        
        //    let parseSelectorSeq = 
        //        parseTypeSelector .>>. many parseSimpleSelector |>> SelectorSeq

        
    let (|Combinator|_|) input =
      match input with
      | ComponentShape.Preserved(l)::ls ->
        match l with
        | Token.Delim('>') -> Some(Child,ls)
        | Token.Delim('*') -> Some(Desendent,ls)
        | Token.Delim('+') -> Some(Next,ls)
        | _ -> None
      | _ -> None
                        
        //    let parseSelector :Parser<SelectorGroup,unit> =     //combinator selectorSeq
        //        sepBy1 (whitespace >>. parseSelectorSeq .>> whitespace) parseCombinator
        


    let (|TypeSelector|_|) (input : ComponentShape list) =
        match input with
        | 

    let (|SelectorSeq|_|) (input : ComponentShape list) : SelectorSeq = 
        match input with
        | TypeSelector ts left ->
        | left ->

    let (|Selector|_|) input =


    let (|NextSelectorGroup|_|) = function
      | ComponentShape.Preserved(Token.Comma) :: ComponentShape.Preserved(Token.Whitespace(_)) :: left -> Some(left)
      | ComponentShape.Preserved(Token.Comma) :: left -> Some(left)
      | _ -> None

    let parseSelectorGroup (input : ComponentShape list) : SelectorGroup list = 
      Stream.sepBy1 Selector NextSelectorGroup input |> Option.get

        //([],input)
        //|> looper (fun (results,x) -> 
        //    match x with
        //    | Selector selseq left -> 
        //      match left with
        //      | NextSelectorGroup left2 -> Some(results,left),true
        //      | _ -> Some(results,left),false
        //    | _ -> None, false
        //    ) 
        //|> fst

    let parseRule (r : RuleShape) : Rule =
        match r with
        | RuleShape.Qualified(ls,bs) -> Rule.Qualified(parseSelectorGroup ls, bs)
        | RuleShape.At(s,ls,bs) -> Rule.At(s, (parseSelectorGroup ls), bs)

    let parseStylesheet (shape : StylesheetShape) : Stylesheet =
        shape |> List.map parseRule 