open System
open System.Collections.Generic
open System.IO

type Operand =
| Value of int64
| Reg of char

type Instruction =
| Snd of char
| Set of char * Operand
| Add of char * Operand
| Mul of char * Operand
| Mod of char * Operand
| Rcv of char
| Jgz of Operand * Operand

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
                             | "jgz" -> Jgz ((parseOp op1), parseOp op2)
                             | _ -> failwith "Invalid input"
    | [| cmd; op |] -> match cmd with
                       | "snd" -> Snd op.[0]
                       | "rcv" -> Rcv op.[0]
                       | _ -> failwith "Invalid input"
    | _ -> failwith "Invalid input"

let input = File.ReadAllLines "18-duet-input.txt" |> Array.map parseLine

let toi label = (int)label - (int)'a'

let registers0 = Array.create ((int)'z' - (int)'a') 0L
registers0.[toi 'p'] <- 0L
let registers1 = Array.create ((int)'z' - (int)'a') 0L
registers1.[toi 'p'] <- 1L

let queue0 = new Queue<int64>()
let queue1 = new Queue<int64>()

let value (regs : int64[]) op =
    match op with
    | Value v -> v
    | Reg c -> regs.[toi c]

type StepResult =
| Step of int
| Sent of int
| CouldNotReceive
| Terminated

let step (regs : int64[]) (queueRec : Queue<int64>) (queueSend : Queue<int64>) (program : Instruction[]) pcnt =
    if pcnt >= program.Length
    then Terminated
    else let instruction = program.[pcnt]
         match instruction with
         | Snd x -> queueSend.Enqueue((regs.[toi x]))
                    Sent (pcnt + 1)
         | Set (x, y) -> regs.[toi x] <- value regs y
                         Step (pcnt + 1)
         | Add (x, y) -> regs.[toi x] <- regs.[toi x] + (value regs y)
                         Step (pcnt + 1)
         | Mul (x, y) -> regs.[toi x] <- regs.[toi x] * (value regs y)
                         Step (pcnt + 1)
         | Mod (x, y) -> regs.[toi x] <- regs.[toi x] % (value regs y)
                         Step (pcnt + 1)
         | Rcv x -> if queueRec.Count > 0
                    then let value = queueRec.Dequeue()
                         regs.[toi x] <- value
                         Step (pcnt + 1)
                    else CouldNotReceive
         | Jgz (x, y) -> if (value regs x) > 0L
                         then Step (pcnt + (int)(value regs y))
                         else Step (pcnt + 1)

let rec proc pcnt0 pcnt1 acc =
    let res0 = step registers0 queue0 queue1 input pcnt0
    let res1 = step registers1 queue1 queue0 input pcnt1

    match (res0, res1) with
    | Step pcnt0, Step pcnt1 -> proc pcnt0 pcnt1 acc
    | Step pcnt0, Sent pcnt1 -> proc pcnt0 pcnt1 (acc + 1)
    | Sent pcnt0, Sent pcnt1 -> proc pcnt0 pcnt1 (acc + 1)
    | Sent pcnt0, Step pcnt1 -> proc pcnt0 pcnt1 acc
    | _, Step pcnt1 -> proc pcnt0 pcnt1 acc
    | _, Sent pcnt1 -> proc pcnt0 pcnt1 (acc + 1)
    | Step pcnt0, _ -> proc pcnt0 pcnt1 acc
    | Sent pcnt0, _ -> proc pcnt0 pcnt1 acc
    | _, _ -> (res0, res1, acc)

[<EntryPoint>]
let main argv =
    let endState = proc 0 0 0
    printfn "The end state: %s" (endState.ToString())
    0
