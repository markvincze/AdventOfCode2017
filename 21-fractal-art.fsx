open System.IO
open System.Text.RegularExpressions

let input =
    "../.# => ##./#../...\n\
    .#./..#/### => #..#/..../..../#..#"

type Image = bool[][]

type Rule = {
    Input : bool[][]
    Output : bool[][]
}

let parseLine line =
    let reg = new Regex("^(.*) => (.*)$")
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

// Returns the square area in image with width s starting from (x,y) as its top-left corner.
let carve (image : Image) y x s =
    printfn "Carve called."
    printfn "image: %A" image
    printfn "y: %d, x: %d, s: %d" y x s
    [|
        for i in y..(y + s - 1)
            -> [| for j in x..(x + s - 1) -> image.[i].[j] |]
    |]

// ab     ca     dc     bd
// cd     db     ba     ac
// ab/cd  ca/db  dc/ba  bd/ac
// cd     db     ba     ac
// ab     ca     dc     bd
// cd/ab  db/ca  ba/dc  ac/bd
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

let ruleMatches (image : Image) y x s rule =
    let rot0 = carve image y x s
    let rot1 = rot rot0
    let rot2 = rot rot1
    let rot3 = rot rot2

    // Try all rotation and flips
    [ rot0; rot1; rot2; rot3; flipv rot0; flipv rot1; flipv rot2; flipv rot3]
    |> List.exists (fun i -> i = rule.Input)

let fill2 (image : Image) rules y x (newImage : Image) =
    let rule = rules
               |> Array.filter (fun r -> Array.length r.Input = 2)
               |> Array.find (ruleMatches image y x 2)
    
    for i = (y + y / 2) to (y + y / 2) + 2 do
        for j = (x + x / 2) to (x + x / 2) + 2 do
            newImage.[i].[j] <- rule.Output.[i - (y + y / 2)].[j - (x + x / 2)]

let fill3 (image : Image) rules y x (newImage : Image) =
    let rule = rules
               |> Array.filter (fun r -> Array.length r.Input = 3)
               |> Array.find (ruleMatches image y x 3)
    
    for i = (y + y / 3) to (y + y / 3) + 3 do
        for j = (x + x / 3) to (x + x / 3) + 3 do
            newImage.[i].[j] <- rule.Output.[i - (y + y / 3)].[j - (x + x / 3)]

let grow2 rules image =
    let newImageSize = (Array.length image) + ((Array.length image) / 2)
    let newImage = Array.init newImageSize (fun _ -> Array.create newImageSize false)

    for y = 0 to (Array.length image) / 2 - 1 do
        for x = 0 to (Array.length image) / 2 - 1 do
            fill2 image rules (y * 2) (x * 2) newImage
    
    newImage

let grow3 rules image =
    let newImageSize = (Array.length image) + ((Array.length image) / 3)
    let newImage = Array.init newImageSize (fun _ -> Array.create newImageSize false)

    for y = 0 to (Array.length image) / 3 - 1 do
        for x = 0 to (Array.length image) / 3 - 1 do
            fill3 image rules (y * 3) (x * 3) newImage
    
    newImage

let grow rules image =
    if (Array.length image) % 2 = 0
    then grow2 rules image
    else grow3 rules image

[<EntryPoint>]
let main argv =
    try
        let image2 = grow rules image
        printfn "New image: %A" image2
    with
        | ex -> printfn "Exception: %s" (ex.ToString())
    0