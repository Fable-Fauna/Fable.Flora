module Stream

open System
open System.Text

type IStream<'t> =
    abstract Read : int -> 't [] option
    abstract Consume : int -> IStream<'t>
    abstract Head : unit -> 't option
    abstract SubSearch : ('t -> 't -> bool) * 't [] -> int option
    abstract Search : ('t -> 't -> bool) *'t -> int option
    abstract Length : unit -> int
    abstract Position : unit -> int

type Stream<'a>( model : 'a [] ref, cacheSize : int, state ) =
    
    let mutable state = state

    new( model : 'a [] ref, cacheSize : int) = 
      let len = if model.Value.Length < cacheSize then model.Value.Length else cacheSize
      Stream<'a>(model,cacheSize, {| Pos = 0; TotalLength = model.Value.Length; Cache = Array.sub model.Value 0 len |})

    interface IStream<'a> with
        member this.Length(): int = 
          state.TotalLength

        member this.Position(): int = 
          state.Pos

        member this.SubSearch(comp, arg: 'a []): int option = 
            let cfn = (fun x y  -> if comp x y then 0 else -1) 
            let mutable target = arg
            let mutable src = state.Pos
            let mutable result = None
            while result.IsNone
             && src < (state.TotalLength - arg.Length) do
              Array.blit model.Value src target 0 arg.Length
              if Array.compareWith cfn target arg = 0 then
                result <- Some(src)
              src <- src + 1
            result

        member this.Search(comp, arg : 'a): int option =
          let mutable src = state.Pos
          let mutable result = None
          while result.IsNone
           && src < state.TotalLength do
            if (comp (model.Value.[src]) arg) then result <- Some(src)
            src <- src + 1
          result


        member this.Consume len = 
            match state.Cache.Length, len with
            | _,b when b < 0 -> failwith "cannot consume negitive length"
            | a,b when a > b -> 
                 Stream<'a>(model,cacheSize,{| state with 
                    Pos = state.Pos + len; 
                    Cache = Array.skip len state.Cache |}) :> IStream<'a>                
            | a,b when state.Pos + b <= state.TotalLength && a <= b -> 
                let readlen = 
                  if state.Pos + state.Cache.Length + cacheSize > state.TotalLength 
                  then state.TotalLength - state.Pos - state.Cache.Length 
                  else cacheSize
                let temp = Array.sub model.Value (state.Pos + state.Cache.Length) readlen
                let temp2 = Array.append state.Cache temp
                Stream<'a>(model,cacheSize,{| state with 
                  Pos = state.Pos + len; 
                  Cache = Array.skip len temp2|}) :> IStream<'a>                   
            | _,_ -> failwith "stream consume exceeds limit"


        member this.Read len =
            match state.Cache.Length, len with
            | _,b when b < 0 -> failwith "cannot read negitive length"
            | _,0 -> None
            | a,b when a >= b -> Some (Array.sub state.Cache 0 len)
            | a,b when a < b && state.Pos + b < state.TotalLength ->
                let readlen = 
                  if len > cacheSize then len 
                  else if state.Pos + cacheSize > state.TotalLength then state.TotalLength - state.Pos
                  else cacheSize
                let temp = Array.sub model.Value (state.Pos + state.Cache.Length) readlen
                state <- {| state with 
                  Pos = state.Pos + len; 
                  Cache = Array.append state.Cache temp|}
                Some (Array.sub state.Cache 0 len)
            | a,b when a < b -> None //outofbounds

        member this.Head(): 'a option = 
          if state.Cache.Length >= 1 then Some(state.Cache.[0])
          else (this :> IStream<'a>).Read 0 |> Option.map Array.head




type ActiveParser<'a,'t> = IStream<'t> -> ('a * IStream<'t>) option

module ParserBuilder = 
    let Return (x: 'a): ActiveParser<'a,'t> =
      let p stream = Some(x, stream)
      in p

    let Bind (p: ActiveParser<'a,'t>) (f: 'a -> ActiveParser<'b,'t>) : ActiveParser<'b,'t> =
          let q stream =
              match p stream with
              | Some(x, rest) -> (f x) rest
              | None -> None
          in q

    let Combine (p: ActiveParser<'a,'t>) (p2: ActiveParser<'b,'t>) : ActiveParser<'b,'t> =
        fun stream ->
          let result = p stream
          match result with
          | Some(_,rest) ->
            p2 rest

    let (>>=) = Bind

    /// If parser p succeeds returns x as a result.
    let (>>%) p x : ActiveParser<'b,'t> =
          p >>= (fun _ -> Return x)

    /// Applies parsers p1 and p2 returning the result of p2.
    let (>>.) p1 p2 : ActiveParser<'b,'t> =
          p1 >>= (fun _ -> p2)

    /// Applies parsers p1 and p2 returning the result of p1.
    let (.>>) p1 p2 : ActiveParser<'a,'t> =
          p1 >>= (fun x -> p2 >>% x)
      
    /// Applies parsers p1 and p2 returning both results.
    let (.>>.) p1 p2: ActiveParser<'a*'b,'t> =
          p1 >>= (fun x -> p2 >>= (fun y -> Return (x, y)))

    type ParserBuilder() =
          member x.Zero () = fun stream -> Some((),stream)
          member x.Bind(p, f) = Bind p f
          member x.Return(y) = Return y
          member inline x.Combine< ^B >(p1 : ActiveParser<unit,'t>, p2 : ActiveParser< ^B,'t>) = p1 >>. p2
          //member x.Combine(p1 ,p2) = p1 .>>. p2
          member x.Delay f = f ()


    let parse = new ParserBuilder()

    let Either (p1: ActiveParser<'a,'t>) (p2: ActiveParser<'a,'t>) : ActiveParser<'a,'t> =
          let p stream =
              match p1 stream with
              | None -> p2 stream
              | res -> res
          in p

    // This is the Either combinator defined in the previous blog post.
    let (<|>) = Either

    let Char (c : char) : ActiveParser<char,char> =
      fun stream ->
          match stream.Head() with
          | Some(chr) when chr = c -> Some(chr, stream.Consume(1))
          | _ -> None
    
    let PChar (c : char) : ActiveParser<unit,char> =
      fun stream ->
          match stream.Head() with
          | Some(chr) when chr = c -> Some((), stream.Consume(1))
          | _ -> None

    let PString (s : string) : ActiveParser<unit,char> =
      fun stream -> 
        match stream.Read(s.Length) with
        | Some(str) when String(str) = s -> Some((),stream.Consume(s.Length))
        | _ -> None    

    let SplitWith (s : string) : ActiveParser<char [],char> =
      fun stream ->
        match stream.SubSearch ((=),s.ToCharArray()) with
        | Some(position) -> 
          match stream.Read position with
          | Some(chars) -> Some(chars,stream.Consume(position + s.Length))
          | _ -> failwith "splitwith is bork"
        | _ -> None

    let Between (s : string) =
        parse {
          let! chrs = PString s >>. SplitWith s
          return chrs
        }

    let Head : ActiveParser<'t,'t> =
        fun stream ->
          stream.Head() |> Option.bind (fun x -> Some(x,stream.Consume(1)))


    let NewLine =
      parse {
        let! c = Char '\u000D' <|>  Char '\u000C' <|>  Char '\u000A'
        if c = '\u000D' then
          do! PChar '\u000A'
          return ()
        return ()
      }





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

let (|Head|_|) (input : IStream<'a>) =
  match input.Head() with
  | Some(v) -> Some(v,input.Consume(1))
  | _ -> None

let (|Fst|_|) value (input : IStream<'a>) =
    match input.Head() with
    | Some(v) when v = value -> Some(input.Consume(1))
    | _ -> None

let (.>>.) (p : ActiveParser<'a,'t>) (q : ActiveParser<'b,'t>) : ActiveParser<'a * 'b,'t> =
  fun x ->
    (p x) |> Option.bind (fun (a,y) -> (q y) |> Option.bind (fun (b,z) -> Some((a,b),z)))

let (>>.) (p : ActiveParser<'a,'t>) (q : ActiveParser<'b,'t>) : ActiveParser<'b,'t> =
  fun x ->
    (p x) |> Option.bind (fun (a,y) -> (q y) |> Option.bind (fun (b,z) -> Some(b,z)))

let (.>>) (p : ActiveParser<'a,'t>) (q : ActiveParser<'b,'t>) : ActiveParser<'a,'t> =
  fun x ->
    (p x) |> Option.bind (fun (a,y) -> (q y) |> Option.bind (fun (b,z) -> Some(a,z)))

let sepBy1 (p : ActiveParser<'a,'t>) (q : ActiveParser<'b,'t>) : ActiveParser<'a list,'t> =
  fun x ->
    ([],x)
    |> looper (fun (r,y) ->
        match (p y) with
        | Some(a,left) ->
          match (q left) with
          | Some(b,z) -> Some(a::r,z),true
          | None -> Some(a::r,left),false
        | None -> None, false)
    |> Some
    


let (|Then|_|) (p: ActiveParser<'a,'t>) (q: ActiveParser<'b,'t>) = (p .>>. q) 


    


let (|PChar|_|) (char : char) (input : IStream<char>) =
    match input.Head() with
    | Some(chr) when chr = char -> Some(input.Consume(1))
    | _ -> None
    
let (|PString|_|) (pattern : string) (input : IStream<char>) =
    match input.Read(pattern.Length) with
    | Some(str) -> 
        let s = String(str)
        if s = pattern then
          Some(input.Consume(pattern.Length))
        else None
    | _ -> None    
    
let (|Between|_|) (char : char) (input : IStream<char>) =
    match input with
    | PChar char str -> 
        match str.Search((fun x y -> x = y),char) with
        | Some(i) -> 
          str.Read(i - str.Position())
          |> Option.map (fun chrs -> chrs, str.Consume(chrs.Length + 1))
        | _ -> None
    | _ -> None    
    
let (|Char|_|) (input : IStream<char>) = input.Head() |> Option.map (fun x -> x,input.Consume(1))

    
let (|NewLine|_|) (input : IStream<char>) =
    match input.Head() with
    | Some('\u000D') -> 
      match input.Read(2) with
      | Some([|'\u000D';'\u000A'|]) -> Some(input.Consume(2))
      | _ -> Some(input.Consume(1))
    | Some(chr) when chr = '\u000C' || chr = '\u000A' -> Some(input.Consume(1))
    | _ -> None

