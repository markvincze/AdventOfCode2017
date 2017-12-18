open System
open System.IO

type Move =
| Spin of int
| Exchange of int * int
| Partner of char * char

let parseMove (str : string) =
    match str.[0] with
    | 's' -> Spin (str.Substring 1 |> Int32.Parse)
    | 'x' -> let nums = (str.Substring 1).Split('/') |> Array.map Int32.Parse
             Exchange (nums.[0], nums.[1])
    | 'p' -> Partner (str.[1], str.[3])
    | _ -> failwith "Invalid input"

let moves = File.ReadAllText("16-permutation-promenade-input.txt").Split(',')
            |> Array.map parseMove
            |> List.ofArray

let programs = [| 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p' |]
let length = Array.length programs

let rec proc programs moves =
    match moves with
    | [] -> programs
    | h :: t -> match h with
                | Spin i -> let programs = programs
                                              |> Array.splitAt (length - i)
                                              |> (fun (x, y) -> Array.append y x)

                            proc programs t
                | Exchange (i, j) -> let tmp = programs.[i]
                                     programs.[i] <- programs.[j]
                                     programs.[j] <- tmp

                                     proc programs t
                | Partner (a, b) -> let idxA = Array.IndexOf(programs, a)
                                    let idxB = Array.IndexOf(programs, b)
                                    let tmp = programs.[idxA]
                                    programs.[idxA] <- programs.[idxB]
                                    programs.[idxB] <- tmp

                                    proc programs t

// Part1
// let programsPart1 = proc programs moves
// let programsPart1Sol = new string(programsPart1)

let rec dance programs moves times =
    if times = 0
    then programs
    else let programsNew = proc programs moves 
         dance programsNew moves (times - 1)

let originalPrograms = [| 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p' |]
let rec findCycle programs moves iteration =
    let programsNew = proc programs moves
    if programs = originalPrograms
    then iteration
    else findCycle programsNew moves (iteration + 1)

// let cycle = findCycle programs moves 0
// printfn "Found cycle in iteration %d" cycle

[<EntryPoint>]
let main argv =

    let programsPart2 = dance programs moves 101
    let programsPart2Sol = new string(programsPart2)
    programsPart2Sol |> printfn "Solution: %s"
    0