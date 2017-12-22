open System.IO

let input = File.ReadAllLines "22-virus-input.txt"

type Position = int32 * int32

type Direction =
| Up
| Right
| Down
| Left

type VirusState = {
    Position : Position
    Direction : Direction
}

let map = Array.init
            1000 
            (fun i -> Array.init
                        1000
                        (fun j -> if i >= 487 && i <= 511 && j >= 487 && j <= 511
                                  then input.[i - 487].[j - 487] = '#'
                                  else false))

let state = {
    Position = (499, 499)
    Direction = Up
}

map.[499].[498] <- true
map.[498].[500] <- true

let print (map : bool[][]) state =
    for i in 485..515 do
        for j in 485..515 do
            let marker = if map.[i].[j]
                         then "#"
                         else "."
            printf "%s%s"
                   marker
                   (if (j, i) = state.Position
                    then "]"
                    else if (j + 1, i) = state.Position
                    then "["
                    else " ")
        printfn ""

let turnLeft dir = match dir with
                   | Up -> Left
                   | Right -> Up
                   | Down -> Right
                   | Left -> Down

let turnRight dir = match dir with
                    | Up -> Right
                    | Right -> Down
                    | Down -> Left
                    | Left -> Up

let move dir (x, y) = match dir with
                      | Up -> x, y - 1
                      | Right -> x + 1, y
                      | Down -> x, y + 1
                      | Left -> x - 1, y

let step (map : bool[][]) state =
    let (x, y) = state.Position
    let newDir = if map.[y].[x]
                 then turnRight state.Direction
                 else turnLeft state.Direction
    
    let causedInfection = not map.[y].[x] 

    map.[y].[x] <- not map.[y].[x]

    { Position = move newDir state.Position; Direction = newDir }, causedInfection

let rec spread map state count acc =
    if count = 0
    then (state, acc)
    else let (state, infected) = step map state
         spread map state (count - 1) (if infected then (acc + 1) else acc)

let (endState, infectionCount) = spread map state 10000 0

map
|> Array.sumBy (fun l -> l |> Array.filter (fun n -> n) |> Array.length)