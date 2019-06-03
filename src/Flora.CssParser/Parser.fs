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
    open Stream

    type NamespaceSelector =
        | AllNamespaces
        | DefultNamespace
        | NoNamespace
        | Namespace of string

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

    and PsudoIdent =
        | Function of string
        | Ident of string

    and Psudo = 
        | Class of PsudoIdent
        | Element of PsudoIdent

    and TypeSelector =
        { Element : ElementSelector
          Namespace : NamespaceSelector}


    and SimpleSelector =
        | Class of string
        | Id of string
        | Attribute of (NamespaceSelector * string) * (Match * string) option //[]
        | Psudo of Psudo 
        | Negation of SimpleSelector


    and NegationArg =
        | Class of string
        | Id of string
        | Attribute of (NamespaceSelector * string) * (Match * string) option //[]
        | Psudo of Psudo 
        | TypeSelector of TypeSelector

    and SelectorSeq =
        { Type : TypeSelector 
          Selectors : SimpleSelector list }


    type Stylesheet =
        Rule list
    
    and SelectorGroup =
        | Single of SelectorSeq
        | Multiple of {| Head : SelectorSeq; Ls : (Combinator * SelectorSeq) [] |}



    and Rule =
        | Qualified of SelectorGroup list * BlockShape
        | At of string * SelectorGroup list * BlockShape



                 
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
        


        

                        

        
    let (|MaybeWhitespace|_|) = function
        | Fst (ComponentShape.Preserved(Token.Whitespace(_))) left -> Some(left)
        | input -> Some(input)

    let (|Class|_|) = function
        | Fst (ComponentShape.Preserved(Token.Delim('.'))) (Head (ComponentShape.Preserved(Token.Ident(str)),left)) -> Some(str,left)
        | _ -> None

    let (|SimpleSelector|_|) = function
        | Class(cls,rest) -> Some(SimpleSelector.Class(cls),rest)
        //todo
        | _ -> None

    let (|ElementSelctor|_|) = function
        | Fst (ComponentShape.Preserved(Token.Delim('*'))) left -> Some(ElementSelector.All,left)
        | Head (ComponentShape.Preserved(Token.Ident(str)), left) -> Some(ElementSelector.Name(str),left)
        | left -> Some(ElementSelector.All,left)

    let (|NamespaceSelector|_|) = function
        | Fst (ComponentShape.Preserved(Token.Delim('*'))) (Fst (ComponentShape.Preserved(Token.Delim('|'))) left) -> Some(NamespaceSelector.AllNamespaces,left)
        | Fst (ComponentShape.Preserved(Token.Delim('|'))) left -> Some(NamespaceSelector.NoNamespace,left)
        | Head (ComponentShape.Preserved(Token.Ident(str)), Fst (ComponentShape.Preserved(Token.Delim('|'))) left ) -> Some(NamespaceSelector.Namespace(str),left)
        | left -> Some(NamespaceSelector.DefultNamespace,left)

    let (|TypeSelector|_|) = function
        | NamespaceSelector(ns,ElementSelctor(es,left)) -> Some({Namespace = ns; Element = es},left)
        | _ -> None

    let (|SelectorSequence|_|) (input : IStream<ComponentShape>)  =
        match input with
        | TypeSelector(typ,left) ->
          let q,rest = 
            ([],left)
            |> Stream.looper (fun (results,x) -> 
              match x with
              | SimpleSelector(s,left) -> Some(s::results,left),true
              | left -> Some(results,left),false)
          Some({Type = typ; Selectors = q},rest)
        | _ -> None

    let (|Combinator|_|) = function
      | Head(ComponentShape.Preserved(l),tail) ->
        match l with
        | Token.Delim('>') -> Some(Child,tail)
        | Token.Delim('*') -> Some(Desendent,tail)
        | Token.Delim('+') -> Some(Next,tail)
        | _ -> None
      | _ -> None

    let (|SelectorSeq|_|) = (|MaybeWhitespace|_|) >>. (|SelectorSequence|_|) .>> (|MaybeWhitespace|_|)

    let selectorGroup (input : IStream<ComponentShape>) =
        match input with
        | SelectorSeq(head,left) ->
          (SelectorGroup.Single(head),left)
          |> Stream.looper (fun (results,x) ->
              match x with
              | Combinator(c,SelectorSeq(s,left)) -> 
                let result = 
                  match results with
                  | SelectorGroup.Single(first) -> SelectorGroup.Multiple({|Head = first; Ls = [|c,s|]|})
                  | SelectorGroup.Multiple(many) -> SelectorGroup.Multiple({|Head = many.Head; Ls = Array.append many.Ls [|c,s|]|})
                Some(result,left),true
              | left -> Some(results,left),false
            )
          |> Some
        | _ -> None

    let nextSelectorGroup = function
      | Fst (ComponentShape.Preserved(Token.Comma)) (MaybeWhitespace(left)) -> Some(left)
      | _ -> None

    let parseSelectorGroup (input : ComponentShape list) : SelectorGroup list = 
      let mem = ref (input |> List.toArray)
      let stream = Stream(mem,20)
      sepBy1 selectorGroup nextSelectorGroup stream |> Option.get |> fst


    let parseRule (r : RuleShape) : Rule =
        match r with
        | RuleShape.Qualified(ls,bs) -> Rule.Qualified(parseSelectorGroup ls, bs)
        | RuleShape.At(s,ls,bs) -> Rule.At(s, (parseSelectorGroup ls), bs)

    let parseStylesheet (shape : StylesheetShape) : Stylesheet =
        shape |> List.map parseRule 