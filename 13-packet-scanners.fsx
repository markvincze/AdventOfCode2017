open System
open System.IO
open System.Linq

let input = File.ReadAllLines "13-packet-scanners-input.txt"
// let input = 
//     [| "0: 3"
//        "1: 2"
//        "4: 4"
//        "6: 4" |]

let firewallLength = input
                     |> Array.last
                     |> (fun l -> l.Split([| ": " |], StringSplitOptions.RemoveEmptyEntries))
                     |> (fun arr -> Int32.Parse arr.[0] + 1)

type ScannerMoveDir = | Down | Up
type Layer = {
    Range : int
    ScannerPos : int
    ScannerDir : ScannerMoveDir
}

type State = {
    Layers : Map<int, Layer>
    PacketIndex : int
}

let layers = input
             |> Array.map
                ((fun l -> l.Split([| ": " |], StringSplitOptions.RemoveEmptyEntries)) >>
                (fun arr -> (Int32.Parse arr.[0], Int32.Parse arr.[1])) >>
                (fun (depth, range) -> (depth, { Range = range; ScannerPos = 0; ScannerDir = Down} )))
             |> Map.ofArray

let initial = {
    Layers = layers
    PacketIndex = 0
}

let moveScanner layer =
    match layer.ScannerDir with
    | Down -> if layer.ScannerPos < layer.Range - 1
              then { layer with ScannerPos = layer.ScannerPos + 1 }
              else { layer with ScannerPos = layer.ScannerPos - 1; ScannerDir = Up }
    | Up -> if layer.ScannerPos > 0
              then { layer with ScannerPos = layer.ScannerPos - 1 }
              else { layer with ScannerPos = layer.ScannerPos + 1; ScannerDir = Down }

let rec tripTail length state acc =
    if state.PacketIndex >= length
    then acc
    else
        let severity = match state.Layers |> Map.tryFind state.PacketIndex with
                       | Some layer -> if layer.ScannerPos = 0
                                       then layer.Range * state.PacketIndex
                                       else 0
                       | _ -> 0
        
        let newState = {
            PacketIndex = state.PacketIndex + 1
            Layers = state.Layers
                     |> Map.map (fun i layer -> moveScanner layer)
        }

        tripTail length newState (severity + acc)

let runLimit = 10000000

let layersCache = new System.Collections.Generic.List<Map<int, Layer>>(runLimit + 100)
layersCache.Add(initial.Layers)

let stepLayers layers = layers
                        |> Map.map (fun _ layer -> moveScanner layer)

let rec fillCache until =
    if layersCache.Count > until
    then ()
    else layersCache.Add(stepLayers (layersCache.Last()))
         fillCache until

let rec tripCheck length state time =
    if state.PacketIndex >= length
    then true
    else
        let caught = if state.PacketIndex >= 0
                     then match state.Layers |> Map.tryFind state.PacketIndex with
                          | Some layer -> layer.ScannerPos = 0
                          | _ -> false
                     else false

        if caught
        then false
        else 
            fillCache (time + 1) |> ignore
            let newState = {
                PacketIndex = state.PacketIndex + 1
                Layers = layersCache.[time + 1]
            }

            tripCheck length newState (time + 1)

let initiallDelay delay =
    fillCache delay |> ignore
    { 
        PacketIndex = 0
        Layers = layersCache.[delay]
    }

let rec findSol delay =
    let limit = runLimit
    if delay > limit
    then printfn "No solution found in %d tries" limit
         -1
    else
        if tripCheck firewallLength (initiallDelay delay) delay
        then delay
        else findSol (delay + 1)

[<EntryPoint>]
let main argv =
    findSol 0 |> printfn "Result: %d"
    0 // return an integer exit code