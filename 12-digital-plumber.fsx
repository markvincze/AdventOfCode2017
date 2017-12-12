open System
open System.Collections.Generic
open System.IO

// let input = [| "0 <-> 2"
//                "1 <-> 1"
//                "2 <-> 0, 3, 4"
//                "3 <-> 2, 4"
//                "4 <-> 2, 3, 6"
//                "5 <-> 6"
//                "6 <-> 4, 5" |]
        
let input = File.ReadAllLines("12-digital-plumber-input.txt")

let parseLine (line : string) = 
    let s = line.Split([| " <-> " |], StringSplitOptions.RemoveEmptyEntries)

    (s.[0] |> Int32.Parse, s.[1].Split([| ", " |], StringSplitOptions.RemoveEmptyEntries) |> Array.map Int32.Parse)

type Node = {
    Id : int
    Pipes : int array
}

let build input =
    let nodes = Map.empty<int, Node>
    let rec buildRec input nodes =
        match input with
        | (id, pipes) :: t ->
            let node = {
                Id = id
                Pipes = pipes
            }

            buildRec t (Map.add id node nodes)
        | [] -> nodes
    
    buildRec input nodes

let parsedInput = input |> Array.map parseLine |> Array.toList

let nodes = build parsedInput

let countReachable nodes node =
    let visited = new Dictionary<int, bool>()

    let rec countReachableRec nodes (visited : Dictionary<int, bool>) node =
        match visited.ContainsKey node.Id with
        | true -> 0
        | false -> visited.Add(node.Id, true)
                   let count = node.Pipes
                              |> Array.map (fun i -> Map.find i nodes)
                              |> Array.sumBy (countReachableRec nodes visited)
                   count + 1
    
    countReachableRec nodes visited node

let node0 = Map.find 0 nodes
countReachable nodes node0 |> printfn "Number of programs in group 0: %d"

let toVisit = Set.ofArray (nodes |> Seq.map (fun x -> x.Key) |> Seq.toArray)

let rec countGroups (nodes : Map<int, Node>) (toVisit : Set<int>) =
    let rec traverse (nodes : Map<int, Node>) (toVisit : Set<int>) (nodeId : int) =
        let toVisitWithout = Set.remove nodeId toVisit
        let node = Map.find nodeId nodes

        let neighborsNotVisited = node.Pipes |> Array.filter (fun id -> Set.contains id toVisitWithout)

        match neighborsNotVisited with
        | [||] -> toVisitWithout
        | _ -> neighborsNotVisited
               |> Array.map (traverse nodes toVisitWithout)
               |> Set.intersectMany

    if Set.count toVisit = 0
    then 0
    else
        let pick = Set.minElement toVisit
        let toVisitLeft = traverse nodes toVisit pick
        1 + (countGroups nodes toVisitLeft)

countGroups nodes toVisit |> printfn "Total number of groups: %d"