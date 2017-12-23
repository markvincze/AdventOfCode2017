open System.IO

let input = File.ReadAllLines "22-virus-input.txt"

type Position = int32 * int32

type Direction =
| Up
| Right
| Down
| Left

type NodeState =
| Clean
| Weakened
| Infected
| Flagged

type VirusState = {
    Position : Position
    Direction : Direction
}

let map = Array.init
            1000
            (fun i -> Array.init
                        1000
                        (fun j -> if i >= 487 && i <= 511 && j >= 487 && j <= 511
                                  then (if input.[i - 487].[j - 487] = '#' then Infected else Clean)
                                  else Clean))

let state = {
    Position = (499, 499)
    Direction = Up
}

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

let reverse dir = match dir with
                  | Up -> Down
                  | Right -> Left
                  | Down -> Up
                  | Left -> Right

let move dir (x, y) = match dir with
                      | Up -> x, y - 1
                      | Right -> x + 1, y
                      | Down -> x, y + 1
                      | Left -> x - 1, y

let evolve node = match node with
                  | Clean -> Weakened
                  | Weakened -> Infected
                  | Infected -> Flagged
                  | Flagged -> Clean

let changeDir node dir = match node with
                         | Clean -> turnLeft dir
                         | Weakened -> dir
                         | Infected -> turnRight dir
                         | Flagged -> reverse dir

let step (map : NodeState[][]) state =
    let (x, y) = state.Position
    let newDir = changeDir map.[y].[x] state.Direction

    map.[y].[x] <- evolve map.[y].[x]
    let causedInfection = map.[y].[x] = Infected

    { Position = move newDir state.Position; Direction = newDir }, causedInfection

let rec spread map state count acc =
    if count = 0
    then (state, acc)
    else let (state, infected) = step map state
         spread map state (count - 1) (if infected then (acc + 1) else acc)

let (endState, infectionCount) = spread map state 10_000_000 0
