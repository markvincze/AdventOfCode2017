type Node = {
    data: int
    mutable next: Node option
}

type CircBuffer = {
    mutable front: Node
}

let find buffer value =
    let rec findRec node value =
        if node.data = value
        then node
        else findRec node.next.Value value
    
    findRec buffer.front value

let rec insertAfter node step value =
    if step = 0
    then let next = node.next
         let newNode = { data = value; next = next }
         node.next <- Some newNode
         newNode
    else insertAfter node.next.Value (step - 1) value

let rec proc (node : Node) limit step value =
    if value % 100000 = 0
    then printfn "Value: %d" value
    else ()

    if value > limit
    then node
    else let node = insertAfter node step value
         proc node limit step (value + 1)

let step = 349

[<EntryPoint>]
let main argv =
    // Part 1
    let buffer = { front = { data = 0; next = None } }
    buffer.front.next <- Some buffer.front

    let node2017 = proc buffer.front 2017 step 1
    let solution1 = node2017.next.Value.data
    printfn "Part1: the solution is %d" solution1

    // Part 2 (the solution was 47949463)
    let buffer = { front = { data = 0; next = None } }
    buffer.front.next <- Some buffer.front

    let node50b = proc buffer.front 50_000_000 step 1
    let zero = find buffer 0
    zero.next.Value.data |> printfn "Part 2: the solution is %d"
    0