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
                Stream<'a>(model,cacheSize,{| state with 
                  Pos = state.Pos + len; 
                  Cache = Array.append (Array.skip len state.Cache) temp|}) :> IStream<'a>                   
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


let (|PChar|_|) (char : char) (input : IStream<char>) =
    match input.Head() with
    | Some(chr) when chr = char -> Some(input.Consume(1))
    | _ -> None
    
let (|PString|_|) (pattern : string) (input : IStream<char>) =
    match input.Read(pattern.Length) with
    | Some(str) when string str = pattern -> 
        Some(input.Consume(pattern.Length))
    | _ -> None    
    
let (|Between|_|) (char : char) (input : IStream<char>) =
    match input with
    | PChar char str -> 
        match str.Search((fun x y -> x = y),char) with
        | Some(i) -> 
          let s = str.Read(i - str.Position())
          Some(s,str.Consume(s.Value.Length + 1))
        | _ -> None
    | _ -> None    
    
let (|Char|_|) (input : IStream<char>) = input.Head() |> Option.map (fun x -> x,input.Consume(1);input)

    
let (|NewLine|_|) (input : IStream<char>) =
    match input.Head() with
    | Some('\u000D') -> 
      match input.Read(2) with
      | Some([|'\u000D';'\u000A'|]) -> Some(input.Consume(2))
      | _ -> Some(input.Consume(1))
    | Some(chr) when chr = '\u000C' || chr = '\u000A' -> Some(input.Consume(1))
    | _ -> None
