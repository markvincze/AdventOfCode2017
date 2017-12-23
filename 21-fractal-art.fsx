open System.IO
open System.Text.RegularExpressions

type Image = bool[][]

type Rule = {
    Input : bool[][]
    Output : bool[][]
}

let parseLine line =
    let reg = Regex("^(.*) => (.*)$")
    let regMatch = reg.Match(line);

    let inputStr = regMatch.Groups.[1].Value
    let outputStr = regMatch.Groups.[2].Value

    {
        Input = inputStr.Split('/')
                |> Array.map (fun s -> s |> Seq.map (fun c -> c = '#') |> Array.ofSeq)
        Output = outputStr.Split('/')
                 |> Array.map (fun s -> s |> Seq.map (fun c -> c = '#') |> Array.ofSeq)
    }

let rules = File.ReadAllLines "21-fractal-art-input.txt"
            |> Array.map parseLine

// The starting image is
// .#.
// ..#
// ###
let image = [| [| false; true; false |]
               [| false; false; true |]
               [| true; true; true |] |]

// Rotations and flips at size 2
// ab     ca     dc     bd
// cd     db     ba     ac
// ab/cd  ca/db  dc/ba  bd/ac
// cd     db     ba     ac
// ab     ca     dc     bd
// cd/ab  db/ca  ba/dc  ac/bd

// Rotation at size 3
// abc gda
// def heb
// ghj jfc

let rot (img : Image) =
    if Array.length img = 2
    then [| [| img.[1].[0]; img.[0].[0] |]
            [| img.[1].[1]; img.[0].[1] |] |]
    else if Array.length img = 3
    then [| [| img.[2].[0]; img.[1].[0]; img.[0].[0] |]
            [| img.[2].[1]; img.[1].[1]; img.[0].[1] |]
            [| img.[2].[2]; img.[1].[2]; img.[0].[2] |] |]
    else failwith "Invalid array length"

let flipv (img : Image) =
    if Array.length img = 2
    then [| img.[1]; img.[0] |]
    else if Array.length img = 3
    then [| img.[2]; img.[1]; img.[0] |]
    else failwith "Invalid array length"

// Returns the square area in image with width s starting from (x,y) as its top-left corner.
let carve (image : Image) y x s =
    [|
        for i in y..(y + s - 1)
            -> [| for j in x..(x + s - 1) -> image.[i].[j] |]
    |]

let matchingRule (image : Image) rules y x s =
    let rot0 = carve image y x s
    let rot1 = rot rot0
    let rot2 = rot rot1
    let rot3 = rot rot2

    // Try all rotation and flips
    let variations = [| rot0; rot1; rot2; rot3; flipv rot0; flipv rot1; flipv rot2; flipv rot3 |]

    rules
    |> Array.filter (fun r -> Array.length r.Input = s)
    |> Array.find (fun r -> Array.exists (fun v -> v = r.Input) variations)

let fill (image : Image) rules y x s (newImage : Image) =
    let rule = matchingRule image rules y x s
    
    for i = (y + y / s) to (y + y / s) + s do
        for j = (x + x / s) to (x + x / s) + s do
            newImage.[i].[j] <- rule.Output.[i - (y + y / s)].[j - (x + x / s)]

let grow rules image =
    let s = if (Array.length image) % 2 = 0
            then 2
            else 3

    let newImageSize = (Array.length image) + ((Array.length image) / s)
    let newImage = Array.init newImageSize (fun _ -> Array.create newImageSize false)

    for y = 0 to (Array.length image) / s - 1 do
        for x = 0 to (Array.length image) / s - 1 do
            fill image rules (y * s) (x * s) s newImage
    
    newImage

[<EntryPoint>]
let main argv =
    try
        let after5 =
            image
            |> grow rules
            |> grow rules
            |> grow rules
            |> grow rules
            |> grow rules
        
        printfn "Final image:\r\n%A" after5

        after5
        |> Array.sumBy (fun l -> Seq.filter id l |> Seq.length)
        |> printfn "Number of pixels \"on\": %d"
        
        let sw = System.Diagnostics.Stopwatch.StartNew()

        let after18 =
            image
            |> grow rules
            |> grow rules
            |> grow rules
            |> grow rules
            |> grow rules
            |> grow rules
            |> grow rules
            |> grow rules
            |> grow rules
            |> grow rules
            |> grow rules
            |> grow rules
            |> grow rules
            |> grow rules
            |> grow rules
            |> grow rules
            |> grow rules
            |> grow rules
        
        sw.Stop()

        after18
        |> Array.sumBy (fun l -> Seq.filter id l |> Seq.length)
        |> printfn "Number of pixels \"on\": %d"
        
        printfn "Elapsed: %s" (sw.Elapsed.ToString())
    with
        | ex -> printfn "Exception: %s" (ex.ToString())
    0