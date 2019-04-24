module Tokenizer

open System
open System.Text
open System.Text.RegularExpressions

//https://www.w3.org/TR/css-syntax-3/

type Parse<'T> = 
    { Comments : string list 
      Errors : string list
      Result : 'T option}
    
type Parser<'T> = string -> Parse<'T> * string

let (.>>) (p1 : Parser<'a>) (p2 : Parser<'b>) = 
    fun str -> 
        match (p1 str) with
        | (R1,left) when R1.Result.IsSome ->
            match (p2 left) with
            | (R2,left) when R2.Result.IsSome ->
                {R1 with Comments = R1.Comments @ R2.Comments},left
            | (R2,left) -> 
                {Comments = R1.Comments @ R2.Comments
                 Errors = R1.Errors @ R2.Errors
                 Result = None}, left
        | x -> x
                   


type Consumer = Parser<unit>

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
    


// let fixStream (str : string) =
//     str.Replace('\u000D','\u000A').Replace('\u000C','\u000A')

let (|PChar|_|) (char : char) (input : string) =
    match input with
    | str when str.[0] = char -> Some(input.Remove(0,1))
    | _ -> None

let (|PString|_|) (str : string) (input : string) =
    match input with
    | str when str.StartsWith str -> 
        Some(input.Remove(0,str.Length))
    | _ -> None    

let (|Between|_|) (char : char) (input : string) =
    match input with
    | PChar char str -> 
        let i = str.IndexOf char
        let s = str.Substring(0,i)
        Some(s,input.Remove(0,i+1))
    | _ -> None    

let (|Char|_|) (input : string) =
    if input.Length > 0 then
        Some(input.[0],input.Remove(0,1)) 
    else None    

let (|NewLine|_|) (input : string) =
    if input.StartsWith("\u000D\u000A") then Some(input.Substring(2))
    else if input.[0] = '\u000D' then Some(input.Substring(1))
    else if input.[0] = '\u000C' then Some(input.Substring(1))
    else if input.[0] = '\u000A' then Some(input.Substring(1))
    else None

let (|Comment|_|) (input : string) =
    if input.StartsWith("/*") then
        let index = input.IndexOf("*/",2)
        Some(input.Substring(2,index), input.Substring(index+2))
    else None

let (|Space|_|) (input : string) =
    match input with
    | NewLine(left) -> Some(left)
    | str when str.[0] = '\t' || str.[0] = ' ' -> Some(input.Substring(1))
    | _ -> None

let (|Whitespace|) (input : string) =
    let mutable loop = true
    let mutable ins = input
    let mutable comment = []
    while loop do
        match ins with
        | Space(str) -> ins <- str
        | Comment(cmt,str) -> ins <- str; comment <- cmt :: comment 
        | _ -> loop <- false
    Token.Whitespace(comment),ins
    
let (|MaybeWhitespace|_|) (input : string) =
    let result = (|Whitespace|) input
    if (snd result).Length = input.Length then None
    else Some(result)

let (|HexDigit|_|) (input : string) =
    match input.[0] with
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
    | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' 
    | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' 
        -> Some(input.[0],input.Remove(0,1))
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


let (|Escape|_|) (input : string) =
    if input.[0] = '\\' then
        let mutable str = input.Remove(0,1)
        let mutable hex = ""
        match str with
        | NewLine(left) -> failwith "cannot escape newline"
        | HexDigit(h,left) ->
            let mutable loop = 1
            str <- left
            hex <- hex + (string) h
            while loop <= 6 do
                match str with
                | HexDigit(h, left) -> 
                    str <- left
                    hex <- hex + (string) h
                    loop <- loop + 1
                | Space(left) -> str < left; loop <- 10
                | _ -> loop <- 10
            Some(hexToUni(hex), left)
        | a when a.Length > 1 -> Some(a, input.Remove(0,2))
    else None       


let (|IdentCodon|_|) (input : string) =
    let c = input.[0] |> string
    match UTF8Encoding.UTF8.GetBytes(c) with
    | [|u|] when u >= 65uy && u <= 90uy -> Some(c)
    | [|u|] when u >= 97uy && u <= 122uy -> Some(c)
    | [|95uy|] -> Some(c)
    | [|u|] when u > 128uy -> Some(c)
    | ary when ary.Length > 1 -> Some(c)
    | _ -> None

let (|Ident|_|) (input : string) = 
    let mutable loop = true
    let mutable fst,rest =
        match input with
        | a when a.[0] = '-' -> 
            match a.Remove(0,1) with
            | IdentCodon(b) -> 
                "-" + b, a.Remove(0,1)
            | Escape(b,left) ->
                b, left
            | _ -> failwith "bad id"
        | IdentCodon(a) -> 
            a,input.Remove(0,1)
        | Escape(a,left) ->
            a, left
        | _ -> loop <- false; "",""
    if loop then
        while loop do 
            match rest with
            | PChar '-' str -> 
                fst <- fst + "-"
                rest <- str
            | IdentCodon(a) -> 
                fst <- fst + a
                rest <- rest.Remove(0,1)
            | Escape(a,left) ->
                fst <- fst + a
                rest <- left
            | _ -> loop <- false
        Some(fst,rest)
    else None

let (|Function|_|) (input : string) =
    match input with
    | Ident(id,rest) ->
        match rest with
        | PChar '(' str -> Some(id,str)
        | _ -> None 
    | _ -> None


let (|AtKeyword|_|)  (input : string) =
    match input with
    | PChar '@' (Ident(id,rest)) -> Some(id,rest)
    | _ -> None

let (|HashToken|_|) (input : string) =
    match input with
    | PChar '#' str -> 
        let mutable loop = true
        let mutable fst, rest = "",""
        while loop do 
            match rest with
            | PChar '-' str -> 
                fst <- fst + "-"
                rest <- str
            | IdentCodon(a) -> 
                fst <- fst + a
                rest <- rest.Remove(0,1)
            | Escape(a,left) ->
                fst <- fst + a
                rest <- left
            | _ -> loop <- false
        Some(fst,rest)
    | _ -> None

let (|StringToken|_|) (input : string) = 
    match input with
    | Between '"' (str, left)
    | Between ''' (str, left) ->
        let mutable loop, result, rest  = true,"",str
        while loop do 
            match rest with
            | Escape(a,left) ->
                result <- result + a
                rest <- left
            | PChar '\\' (NewLine(left)) -> 
                result <- result + "\u000A"
                rest <- left
            | Char (char, left) -> 
                result <- result + string char
                rest <- left
            | _ -> ()
        Some(result,left)    
    | _ -> None

let (|UrlUnQuote|_|) (input : string) =
    let mutable loop, result, rest  = true,"",input
    let mutable ret = false
    while loop do 
        match rest with
        | Escape(a,left) ->
            result <- result + a
            rest <- left
        | Char (char, left) when 
            char = '\\' 
            || char = '"' 
            || char = ''' 
            || char = '('
            || char = ')'
            || Char.IsControl char
            -> loop <- false //failwith "url cannot contain \\ \" ' ( ) "
        | MaybeWhitespace(left) -> loop <- false
        | Char (char,left) ->
            result <- result + string char
            rest <- left
        | a -> 
            result <- result + a
            loop <- false
            ret <- true
    if ret then Some(result) else None        

let (|UrlToken|_|) (input : string) =
    match input with
    | PString "url" (PChar '(' str) ->
        let i = str.IndexOf ')'
        let inner = str.Substring(0,i)
        let rest = input.Remove(0,i+1)
        match inner with
        | MaybeWhitespace(_,left) ->
            match left with
            | UrlUnQuote(out) -> Some(out,rest)
            | StringToken(out,MaybeWhitespace(_,left)) -> 
                if left.Length > 0 then failwith "invliad url token"
                Some(out,rest)
            | _ -> None
        | _ -> None
    | _ -> None                

let (|Digits|_|) (input : string) =
    let mutable loop, result, rest  = true,"",input
    while loop do 
       match rest with
       | PChar '0' left
       | PChar '1' left
       | PChar '2' left
       | PChar '3' left
       | PChar '4' left
       | PChar '5' left
       | PChar '6' left
       | PChar '7' left
       | PChar '8' left
       | PChar '9' left ->
            result <- result + string rest.[0]
            rest <- left
       | _ -> loop <- false   
    if result.Length > 0 then
        Some(result,rest)
    else None     

let (|NumberToken|_|) (input : string) =
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
        | _ -> false, "", left2 

    if number = "" && decimal = "" then None 
    else 
    Some({ Exponent = if exponent = "" then None else Some(eneg,exponent) 
           Number = if number = "" then None else Some(neg,number)
           Decimal = if decimal = "" then None else Some(decimal)}, left3)


let tokenise (input : string) =
    match input with
    | MaybeWhitespace(t,left) -> t,left
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
    | _ -> Token.Delim input.[0], input.Remove(0,1)

type Stylesheet =
    Rule list

and Rule =
    | Qualified of Component list * Block
    | At of string * Component list * Block

and Component =
    | Preserved of Token
    | SwiggleBlock of Block
    | ParenBlock of Block
    | SquareBlock of Block
    | Function of string * Block

and Block = Component list    

let parse (input : Token list) =
    match input with
    | Token.Function(name) :: left -> left

