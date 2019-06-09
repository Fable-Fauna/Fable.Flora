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
    return chrs
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
    String(cmt) :: cmts, ls
  | _ -> [],stream

let whitespace stream =
  let q,rest = whitespace_fn stream
  if stream.Position() = rest.Position() then None else Some(Token.Whitespace(q),rest)

let (|Whitespace|_|) = whitespace

let (|MustWhitespace|) (input : IStream<char>) =
    let opt = whitespace input
    if opt.IsNone then failwith "must whitespace"
    else opt
    
let hexDigits = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F' |]

let (|HexDigit|_|) (input : IStream<char>) =
    match input with
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
    | PString "\\" blok ->
        match blok with
        | NewLine(_) -> failwith "cannot escape newline"
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


let (|IdentCodon|_|) (input : IStream<char>) = 
    match input with
    | Head(c,left) ->
        match UTF8Encoding.UTF8.GetBytes([|c|]) with
        | [|u|] when u >= 65uy && u <= 90uy -> Some(c)
        | [|u|] when u >= 97uy && u <= 122uy -> Some(c)
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
      | PChar '-' n2 -> 
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


let (|Function|_|) (input : IStream<char>) =
    match input with
    | Ident(id,rest) ->
        match rest with
        | PChar '(' str -> Some(id,str)
        | _ -> None 
    | _ -> None


let (|AtKeyword|_|)  (input : IStream<char>) =
    match input with
    | PChar '@' (Ident(id,rest)) -> Some(id,rest)
    | _ -> None

let (|HashToken|_|) (input : IStream<char>) =
    match input with
    | PChar '#' n1 -> 
      ("",n1)
      |> Stream.looper (fun (result,n2) -> 
        match n2 with
        | PChar '-' n3 ->  Some(result + "-", n3),true
        | IdentCodon(codon,n2) -> Some(result + string codon,n2),true
        | Escape(b,n2) -> Some(result + string b, n2),true
        | _ -> if result = "" then None,false else Some(result,n1),false)
      |> Some
    | _ -> None

let (|StringToken|_|) (input : IStream<char>) = 
    match input with
    | Between '"' (str, next)
    | Between ''' (str, next) ->
        let mem = ref (str)
        let stream = Stream(mem,20)
        ("",stream :> IStream<char>)
        |> Stream.looper (fun (result,n1) -> 
          match n1 with
          | PChar '\\' (NewLine(n2)) ->  Some(result + "\u000A", n2),true
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
    | PString "url" (PChar '(' (SplitWith ")" (inner,rest))) ->
        let istream = Stream(ref inner,20) :> IStream<char>
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
        | PChar '+' left -> false,left
        | PChar '-' left -> true,left
        | str ->  false,str
    let number, decimal, left2 = 
        match left1 with
        | Digits (num,(PChar '.' (Digits (d,left)))) -> num, d, left
        | Digits (num,left) -> num, "", left
        | PChar '.' (Digits (d,left)) -> "", d, left
        | _ -> "", "", left1 
    
    let eneg, exponent, left3 =
        match left2 with
        | PChar 'e' left
        | PChar 'E' left ->
            match left with 
            | PChar '+' (Digits (num,left)) -> false, num, left
            | PChar '-' (Digits (num,left))-> true,num,left
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
    | HashToken(t,left) -> Token.Hash(t),left //incorrect
    | PString "$=" left -> Token.SuffixMatch,left
    //apostrohpy string 
    | PChar '(' left -> Token.ParenStart, left
    | PChar ')' left -> Token.ParenEnd, left
    | PString "*=" left -> Token.SubstringMatch, left
    | NumberToken (num, left) -> Token.Number(num), left
    | PChar ',' left -> Token.Comma, left
    | PString "-->" left -> Token.CDC, left
    //ident like
    | Function (word,left) -> Token.Function word, left
    | UrlToken (word,left) -> Token.Url word, left
    | Ident (word,left) -> Token.Ident word, left
    | PChar ':' left -> Token.Colon, left
    | PChar ';' left -> Token.SemiColon, left
    | PString "<!--" left -> Token.CDO, left
    | AtKeyword (word,left) -> Token.At word, left
    | PChar '[' left -> Token.SquareStart, left
    //ident like
    | PChar ']' left -> Token.SquareEnd, left
    | PString "^=" left -> Token.PrefixMatch, left
    | PChar '{' left -> Token.SwiggleStart, left
    | PChar '}' left -> Token.SwiggleEnd, left
    | PString "|=" left -> Token.DashMatch, left
    | PString "||" left -> Token.Column, left
    | PString "~=" left -> Token.IncludeMatch, left
    //unicode range
    //eof
    | Char (chr,left) -> Token.Delim chr, left

let tokenStream (input : IStream<char>) =
    input |> Seq.unfold (fun stream -> 
        if stream.Head().IsNone then None else Some(tokenise stream)
        ) |> Seq.toList
