module Tokenizer

open System
open System.Text
open System.Text.RegularExpressions

//https://www.w3.org/TR/css-syntax-3/
open Stream
open Stream.ParserBuilder
open System

type NumberToken =
    { Exponent : (bool * string) option
      Number : (bool * string) option
      Decimal : string option }

[<RequireQualifiedAccessAttribute>]
type Token =
    | Whitespace of string list
    | Ident of string
    | Function of string
    | At of string
    | Hash of string
    | String of string
    | Url of string
    | Number of NumberToken
    | Dimension
    | Percentage
    | UnicodeRange
    | IncludeMatch
    | DashMatch
    | PrefixMatch
    | SuffixMatch
    | SubstringMatch
    | Column
    | CDO
    | CDC
    | Delim of char
    | Colon
    | SemiColon
    | Comma
    | SquareStart
    | SquareEnd
    | SwiggleStart
    | SwiggleEnd
    | ParenStart
    | ParenEnd
 
 
let (|Comment|_|)  =
  parse {
    let! chrs = PString "/*" >>. SplitWith "*/"
    return String(chrs)
  }

let (|Space|_|) = NewLine <|> PChar '\t' <|> PChar ' '

let (|SplitWith|_|) = ParserBuilder.SplitWith

let rec whitespace_fn stream =
  match stream with
  | Space(_,left) -> 
    let cmts, ls = whitespace_fn left
    cmts, ls
  | Comment(cmt,left) -> 
    let cmts, ls = whitespace_fn left
    cmt :: cmts, ls
  | s -> [],s

let whitespace stream =
  let q,rest = whitespace_fn stream
  if stream.Position() = rest.Position() then None else Some(Token.Whitespace(q),rest)

let (|Whitespace|_|) = whitespace

let (|MustWhitespace|) (input : IStream<char>) =
    let opt = whitespace input
    if opt.IsNone then failwith "must whitespace"
    else opt
    
let hexDigits = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F' |]

let (|HexDigit|_|) = function
    | Head(t,rest) when Array.contains t hexDigits -> Some(t,rest)
    | _ -> None

let hexToByte (input : char) =
    match input with
    | '0' -> 0uy
    | '1' -> 1uy
    | '2' -> 2uy
    | '3' -> 3uy
    | '4' -> 4uy
    | '5' -> 5uy
    | '6' -> 6uy
    | '7' -> 7uy
    | '8' -> 8uy
    | '9' -> 9uy
    | 'a' | 'A' -> 10uy
    | 'b' | 'B' -> 11uy
    | 'c' | 'C' -> 12uy
    | 'd' | 'D' -> 13uy
    | 'e' | 'E' -> 14uy
    | 'f' | 'F' -> 15uy
    | _ -> failwith "bad hex char"

let hexToUni (input : string) =
    let ary = 
        match input.Length with
        | 1 -> [|hexToByte input.[0]|] 
        | 2 -> [|hexToByte input.[0] * 16uy + hexToByte input.[1]|]
        | 3 -> [|hexToByte input.[0]; 
            hexToByte input.[0] * 16uy + hexToByte input.[1]|] 
        | 4 -> [|hexToByte input.[0] * 16uy + hexToByte input.[1]; 
            hexToByte input.[0] * 16uy + hexToByte input.[1]|]
        | 5 -> [|hexToByte input.[0]; 
            hexToByte input.[0] * 16uy + hexToByte input.[1]; 
            hexToByte input.[0] * 16uy + hexToByte input.[1]|]
        | 6 -> [|hexToByte input.[0] * 16uy + hexToByte input.[1]; 
            hexToByte input.[0] * 16uy + hexToByte input.[1]; 
            hexToByte input.[0] * 16uy + hexToByte input.[1]|]
        | _ -> failwithf "invalid hex codepoint %s" input
    Encoding.UTF8.GetString(ary)


let (|Escape|_|) (input : IStream<char>) =
    match input with
    | PString "\\" (_,blok) ->
        match blok with
        | NewLine(_,_) -> failwith "cannot escape newline"
        | HexDigit(h,left) -> 
            ((string h, 1),left)
            |> Stream.looper (fun ((hex,cnt),stream) ->
              if cnt <= 6 then
                match stream with
                | HexDigit(h, left) -> Some((hex + string h, cnt+1),left),true
                | Space(_,left) -> Some((hex, cnt+1),left),false
                | _ -> None,false
              else Some((hex, cnt+1),left),false)             
            |> (fun ((hex,_),stream) -> Some(hexToUni(hex),stream))

        | Head(chr,left) -> Some(string chr,left)
    | _ -> None


let (|IdentCodon|_|) = function
    | Head(c,left) ->
        match UTF8Encoding.UTF8.GetBytes([|c|]) with
        | [|u|] when u >= 65uy && u <= 90uy -> Some(c)
        | [|u|] when u >= 97uy && u <= 122uy -> Some(c)
        | [|u|] when u >= 48uy && u <= 57uy -> Some(c)
        | [|95uy|] -> Some(c)
        | [|u|] when u > 128uy -> Some(c)
        | ary when ary.Length > 1 -> Some(c) //?
        | _ -> None
        |> Option.map (fun x -> x,left)
    | _ -> None


let (|Ident|_|) (input : IStream<char>) =
    let mutable fst = true
    ("",input)
    |> Stream.looper (fun (result,n1) -> 
      match n1 with
      | PChar '-' (_,n2) -> 
        if fst then
          fst <- false
          match n2 with
          | Escape(b,n3) -> Some(result + string b, n3),true
          | _ -> Some(result + "-", n2),true
        else Some(result + "-", n2),true
      | IdentCodon(codon,n2) -> Some(result + string codon,n2),true
      | Escape(b,n2) -> Some(result + string b, n2),true
      | _ -> if result = "" then None,false else Some(result,n1),false)
    |> (fun (result,next) -> if result = "" then None else Some(result,next))

let Ident = (|Ident|_|)

let (|Function|_|) = Ident .>> PChar '(' 


let (|AtKeyword|_|) = PChar '@' >>. Ident


let (|HashToken|_|) (input : IStream<char>) =
    match input with
    | PChar '#' (_,n1) -> 
      ("",n1)
      |> Stream.looper (fun (result,n2) -> 
        match n2 with
        | PChar '-' (_,n3) ->  Some(result + "-", n3),true
        | IdentCodon(codon,n2) -> Some(result + string codon,n2),true
        | Escape(b,n2) -> Some(result + string b, n2),true
        | _ -> if result = "" then None,false else Some(result,n1),false)
      |> Some
    | _ -> None

let (|StringToken|_|) (input : IStream<char>) = 
    match input with
    | Between "\"" (str, next)
    | Between "'" (str, next) ->
        let stream = CachelessStream(ref str,0) :> IStream<char>
        ("",stream)
        |> Stream.looper (fun (result,n1) -> 
          match n1 with
          | PChar '\\' (_,(NewLine(_,n2))) ->  Some(result + "\u000A", n2),true
          | Char(codon,n2) -> Some(result + string codon,n2),true
          | Escape(b,n2) -> Some(result + string b, n2),true
          | _ -> Some(result,n1),false)
        |> (fun (str,_) -> Some(str,next))
    | _ -> None

//need to test and rethink
let (|UrlUnQuote|_|) (input : IStream<char>) =
    let mutable ret = false //used to check the entier stream is processed
    ("",input)
    |> Stream.looper (fun (result,n1) -> 
      match n1 with
      | Escape(a,left) -> Some(result + a,left),true
      | Char (char, left) when 
        char = '\\' 
        || char = '"' 
        || char = ''' 
        || char = '('
        || char = ')'
        || Char.IsControl char
        -> None,false //failwith "url cannot contain \\ \" ' ( ) "
      | Whitespace(_,left) -> Some(result,n1),false
      | Char (char,left) -> Some(result + string char,left),true
      | _ ->  //endofstream 
        ret <- true
        Some(result,n1),false)
    |> (fun (result,rest) -> if ret then Some(result,rest) else None) 

        

let (|UrlToken|_|) (input : IStream<char>) =
    match input with
    | PString "url" (_,(PChar '(' (_,(SplitWith ")" (inner,rest))))) ->
        let istream = CachelessStream(ref inner,0) :> IStream<char>
        match istream with
        | Whitespace(_,left) ->
            match left with
            | UrlUnQuote(out,_) -> Some(out,rest)
            | StringToken(out,Whitespace(_,left)) -> 
                if left.Head().IsSome then failwith "invliad url token"
                Some(out,rest)
            | _ -> None
        | _ -> None
    | _ -> None                

let charDigits = [|'0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'|]

let (|Digits|_|) (input : IStream<char>) =
    ("",input)
    |> Stream.looper (fun (result,n1) -> 
       match n1 with
       | Char (chr,n2) when Array.contains chr charDigits -> Some(result+string chr,n2),true
       | _ -> Some(result,n1),false)
    |> (fun (result,next) -> if result.Length > 0 then Some(result,next) else None)


let (|NumberToken|_|) (input : IStream<char>) =
    let neg,left1 = 
        match input with 
        | PChar '+' (_,left) -> false,left
        | PChar '-' (_,left) -> true,left
        | str ->  false,(str)
    let number, decimal, left2 = 
        match left1 with
        | Digits (num,(PChar '.' (_,(Digits (d,left))))) -> num, d, left
        | Digits (num,left) -> num, "", left
        | PChar '.' (_,(Digits (d,left))) -> "", d, left
        | _ -> "", "", left1 
    
    let eneg, exponent, left3 =
        match left2 with
        | PChar 'e' (_,left)
        | PChar 'E' (_,left) ->
            match left with 
            | PChar '+' (_,(Digits (num,left))) -> false, num, left
            | PChar '-' (_,(Digits (num,left))) -> true,num,left
            | Digits (num,left) ->  false,num,left 
            | _ -> false, "", left2 //case of "em" terminal
        | _ -> false, "", left2 

    if number = "" && decimal = "" then None 
    else 
    Some({ Exponent = if exponent = "" then None else Some(eneg,exponent) 
           Number = if number = "" then None else Some(neg,number)
           Decimal = if decimal = "" then None else Some(decimal)}, left3)


let tokenise (input : IStream<char>) =
    match input with
    | Whitespace(t,left) -> t,left
    | StringToken(t,left) -> Token.String(t),left
    | HashToken(t,left) -> Token.Hash(t),left //incorrect?
    | PString "$=" (_,left) -> Token.SuffixMatch,left
    //apostrohpy string 
    | PChar '(' (_,left) -> Token.ParenStart, left
    | PChar ')' (_,left) -> Token.ParenEnd, left
    | PString "*=" (_,left) -> Token.SubstringMatch, left
    | NumberToken (num, left) -> Token.Number(num), left
    | PChar ',' (_,left) -> Token.Comma, left
    | PString "-->" (_,left) -> Token.CDC, left
    //ident like
    | Function (word,left) -> Token.Function word, left
    | UrlToken (word,left) -> Token.Url word, left
    | Ident (word,left) -> Token.Ident word, left
    | PChar ':' (_,left) -> Token.Colon, left
    | PChar ';' (_,left) -> Token.SemiColon, left
    | PString "<!--" (_,left) -> Token.CDO, left
    | AtKeyword (word,left) -> Token.At word, left
    | PChar '[' (_,left) -> Token.SquareStart, left
    //ident like
    | PChar ']' (_,left) -> Token.SquareEnd, left
    | PString "^=" (_,left) -> Token.PrefixMatch, left
    | PChar '{' (_,left)-> Token.SwiggleStart, left
    | PChar '}' (_,left) -> Token.SwiggleEnd, left
    | PString "|=" (_,left) -> Token.DashMatch, left
    | PString "||" (_,left) -> Token.Column, left
    | PString "~=" (_,left) -> Token.IncludeMatch, left
    //unicode range
    //eof
    | Char (chr,left) -> Token.Delim chr, left

let printToken (t : Token) : string =
    match t with
    | Token.Whitespace(strs) -> String.Concat strs
    | Token.Ident(str) -> str
    | Token.Function(str) -> str
    | Token.At(str) -> str
    | Token.Hash(str) -> str
    | Token.String(str) -> str
    | Token.Url(str) -> str
    | Token.Number(nums) ->
        let mutable str = ""
        if nums.Number.IsSome then
            let neg,value = nums.Number.Value
            if neg then str <- "-"
            str <- str + value
        if nums.Decimal.IsSome then
            str <- str + "." + nums.Decimal.Value
        if nums.Exponent.IsSome then
            let neg,value = nums.Exponent.Value
            str <- str + (if neg then "e-" else "e") + value 
        str
        
    | Token.Dimension -> "dims"
    | Token.Percentage -> "%"
    | Token.UnicodeRange -> ""
    | Token.IncludeMatch -> "~="
    | Token.DashMatch -> "|="
    | Token.PrefixMatch -> "^="
    | Token.SuffixMatch -> "$="
    | Token.SubstringMatch -> "*="
    | Token.Column -> "||"
    | Token.CDO -> "<!--"
    | Token.CDC -> "-->"
    | Token.Delim(c) -> string c
    | Token.Colon -> ":"
    | Token.SemiColon -> ";"
    | Token.Comma -> ","
    | Token.SquareStart -> "["
    | Token.SquareEnd -> "]"
    | Token.SwiggleStart -> "{"
    | Token.SwiggleEnd -> "}"
    | Token.ParenStart -> "("
    | Token.ParenEnd -> ")"

let tokenStream (input : IStream<char>) =
    input |> Seq.unfold (fun stream -> 
        if stream.Head().IsNone then None else Some(tokenise stream)
        ) |> Seq.toList
