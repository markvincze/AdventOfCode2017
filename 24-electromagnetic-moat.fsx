open System
open System.IO

//let input = [| "0/2"; "2/2"; "2/3"; "3/4"; "3/5"; "0/1"; "10/1"; "9/10"; |]
let input = File.ReadAllLines "24-electromagnetic-moat-input.txt"

type Comp = {
    Id : int
    Port1 : int
    Port2 : int
}

type CompInBridge = {
    Id : int
    PortLeft : int
    PortRight : int
}

let parseComp (line : string) id =
    match line.Split('/') with
    | [| p1; p2 |] -> { Id = id; Port1 = Int32.Parse p1; Port2 = Int32.Parse  p2 }
    | p -> failwith (sprintf "Invalid input, line: %s, parsed: %A" line p)

let components = input
                 |> Array.mapi (fun i l -> parseComp l i)
                 |> List.ofArray

let strength comp = comp.Port1 + comp.Port2

let rec build bridge components =
    let lastPort = match bridge with
                   | [] -> 0
                   | h :: _ -> h.PortRight

    let fittingComps = components
                       |> List.filter
                            (fun c -> (c.Port1 = lastPort || c.Port2 = lastPort) &&
                                       bridge |> List.exists (fun b -> b.Id = c.Id) |> not)

    match fittingComps with
    | [] -> bridge |> List.sumBy (fun c -> c.PortLeft + c.PortRight), List.length bridge
    | _ -> fittingComps
           |> List.map (fun c -> let nextComp = {
                                     Id = c.Id
                                     PortLeft = lastPort
                                     PortRight = if c.Port1 = lastPort then c.Port2 else c.Port1
                                 }

                                 build (nextComp :: bridge) components)
           |> List.maxBy (fun (strength, length) -> length, strength)

[<EntryPoint>]
let main argv =
    let (strength, length) = build [] components
    printfn "The strongest possible bridge has the strength %d, its length is %d" strength length
    0
