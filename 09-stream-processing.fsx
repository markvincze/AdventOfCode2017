open System.IO

let input = File.ReadAllText("09-stream-processing-input.txt")

type ParseState = 
| Normal
| Garbage
| AfterCancel

type Context = {
    State : ParseState
    Depth : int
    TotalScore : int
    TotalNonConcealedGarbage : int
}

let initial = {
    State = Normal
    Depth = 1
    TotalScore = 0
    TotalNonConcealedGarbage = 0
}

let rec proc (input : string) index ctx =
    if index >= input.Length
    then
        ctx
    else
        match ctx.State with
        | Normal -> match input.[index] with
                    | '{' -> { ctx with TotalScore = ctx.TotalScore + ctx.Depth; Depth = ctx.Depth + 1 }
                    | '}' -> { ctx with Depth = ctx.Depth - 1 }
                    | '<' -> { ctx with State = Garbage }
                    | ',' -> ctx
                    | _ -> failwith "Invalid input"
                    |> proc input (index + 1)
        | Garbage -> match input.[index] with
                     | '!' -> { ctx with State = AfterCancel }
                     | '>' -> { ctx with State = Normal }
                     | _ -> { ctx with State = Garbage; TotalNonConcealedGarbage = ctx.TotalNonConcealedGarbage + 1 }
                     |> proc input (index + 1)
        | AfterCancel -> { ctx with State = Garbage } |> proc input (index + 1)

let ctx = proc input 0 initial
printfn "TotalScore: %d" ctx.TotalScore
printfn "TotalNonConcealedGarbage: %d" ctx.TotalNonConcealedGarbage