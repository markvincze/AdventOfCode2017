open System
open System.IO

type Operand =
| Value of int64
| Reg of char

type Instruction =
| Snd of Operand
| Set of char * Operand
| Add of char * Operand
| Mul of char * Operand
| Mod of char * Operand
| Rcv of Operand
| Jgz of char * Operand

let parseOp (str : string) =
    if Char.IsLetter str.[0]
    then Reg str.[0]
    else Value (Int64.Parse str)

let parseLine (line : string) =
    match line.Split(' ') with
    | [| cmd; op1; op2 |] -> match cmd with
                             | "set" -> Set (op1.[0], parseOp op2)
                             | "add" -> Add (op1.[0], parseOp op2)
                             | "mul" -> Mul (op1.[0], parseOp op2)
                             | "mod" -> Mod (op1.[0], parseOp op2)
                             | "jgz" -> Jgz (op1.[0], parseOp op2)
                             | _ -> failwith "Invalid input"
    | [| cmd; op |] -> match cmd with
                       | "snd" -> Snd (parseOp op)
                       | "rcv" -> Rcv (parseOp op)
                       | _ -> failwith "Invalid input"
    | _ -> failwith "Invalid input"

let input = File.ReadAllLines "18-duet-input.txt" |> Array.map parseLine

type State = {
    Registers : int64[]
    LastReceived : int64
}

let state = {
    Registers = Array.create ((int)'z' - (int)'a') 0L
    LastReceived = 0L
}

let toi label = (int)label - (int)'a'

let value state op =
    match op with
    | Value v -> v
    | Reg c -> state.Registers.[toi c]

let rec proc state (program : Instruction[]) pcnt =
    let instruction = program.[pcnt]
    match instruction with
    | Snd x -> proc { state with LastReceived = (value state x) } program (pcnt + 1)
    | Set (x, y) -> state.Registers.[toi x] <- value state y
                    proc state program (pcnt + 1)
    | Add (x, y) -> state.Registers.[toi x] <- state.Registers.[toi x] + (value state y)
                    proc state program (pcnt + 1)
    | Mul (x, y) -> state.Registers.[toi x] <- state.Registers.[toi x] * (value state y)
                    proc state program (pcnt + 1)
    | Mod (x, y) -> state.Registers.[toi x] <- state.Registers.[toi x] % (value state y)
                    proc state program (pcnt + 1)
    | Rcv x -> if (value state x) <> 0L
               then state
               else proc state program (pcnt + 1)
    | Jgz (x, y) -> if state.Registers.[toi x] > 0L
                    then proc state program (pcnt + (int)(value state y))
                    else proc state program (pcnt + 1)

[<EntryPoint>]
let main argv = 
    let endState = proc state input 0
    printfn "The end state: %s" (endState.ToString())
    0