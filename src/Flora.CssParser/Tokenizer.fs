module Tokenizer

open System
open System.Text
open System.Text.RegularExpressions

//https://www.w3.org/TR/css-syntax-3/

[<RequireQualifiedAccessAttribute>]
type Token =
    | Whitespace of string list
    | Ident of string
    | Function
    | At
    | Hash of string
    | String of string
    | Url
    | Number 
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
    | Delim
    | Colon
    | SemiColon
    | Comma
    | SquareStart
    | SquareEnd
    | SwiggleStart
    | SwiggleEnd
    | ParenStart
    | ParenEnd


let fixStream (str : string) =
    str.Replace('\u000D','\u000A').Replace('\u000C','\u000A')

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

let (|Ident|) (input : string) = 
    let mutable fst,rest =
        match input with
        | a when a.[0] = '-' -> 
            match a.Remove(0,1) with
            | IdentCodon(b) -> 
                "-" + b, a.Remove(0,1)
            | Escape(b,left) ->
                b, left
        | IdentCodon(a) -> 
            a,input.Remove(0,1)
        | Escape(a,left) ->
            a, left
        | _ -> failwith "not ident"
    let mutable loop = true
    while loop do 
        match rest with
        | a when a.[0] = '-' -> 
            fst <- fst + "-"
        | IdentCodon(a) -> 
            fst <- fst + a
            rest <- rest.Remove(0,1)
        | Escape(a,left) ->
            fst <- fst + a
            rest <- left
        | _ -> loop <- false
    Token.Ident(fst),rest
