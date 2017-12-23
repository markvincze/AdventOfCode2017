// Naive solution. This could only solve part 1.
// To solve part 2, I translated the assembly code to C# and optimized it in 23-coprocessor-part2.cs
open System
open System.IO
open System.Text.RegularExpressions

type Operand =
| Value of int64
| Reg of char

type Instruction =
| Set of char * Operand
| Sub of char * Operand
| Mul of char * Operand
| Jnz of Operand * Operand

let parseOp (op : string) =
  if Char.IsLetter op.[0]
  then Reg op.[0]
  else Value (Int64.Parse op)

let parseInstruction (line : string) =
  match line.Split(' ') with
  | [| ins; op1; op2 |] ->
    match ins with
    | "set" -> Set(op1.[0], parseOp op2)
    | "sub" -> Sub(op1.[0], parseOp op2)
    | "mul" -> Mul(op1.[0], parseOp op2)
    | "jnz" -> Jnz(parseOp op1, parseOp op2)
    | _ -> failwith "Invalid input"
  | _ -> failwith "Invalid input"

let input = File.ReadAllLines "23-coprocessor-input.txt"
            |> Array.map parseInstruction

let registers = Array.create 8 0L
registers.[0] <- 1L // Part 2: set a to 1

let toi reg = (int32)reg - (int32)'a'

let value (registers : int64[]) op = match op with
                                     | Value v -> v
                                     | Reg r -> registers.[toi r]

let procInstruction (registers : int64[]) ins procCnt =
  match ins with
  | Set(reg, op) -> registers.[toi reg] <- value registers op
                    procCnt + 1
  | Sub(reg, op) -> registers.[toi reg] <- registers.[toi reg] - value registers op
                    procCnt + 1
  | Mul(reg, op) -> registers.[toi reg] <- registers.[toi reg] * value registers op
                    procCnt + 1
  | Jnz(op1, op2) -> if value registers op1 <> 0L
                     then procCnt + (int32)(value registers op2)
                     else procCnt + 1

let rec proc (regs : int64[]) input procCnt iter =
  if iter % 10000000L = 0L
  then printfn
        "Iteration %d. procCnt: %d. a: %d, b: %d, c: %d, d: %d, e: %d, f: %d, g: %d, h: %d"
        iter procCnt regs.[0] regs.[1] regs.[2] regs.[3] regs.[4] regs.[5] regs.[6] regs.[7]
  else ()

  if procCnt < Array.length input
  then let procCnt = procInstruction regs input.[procCnt] procCnt
       proc regs input procCnt (iter + 1L)
  else regs.[toi 'h']

[<EntryPoint>]
let main argv =
  proc registers input 0 0L |> printfn "The final value of h is %d"
  let finalHValue = registers.[toi 'h']
  finalHValue |> printfn "The final value in register h is %d"
  0

