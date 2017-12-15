open System.Text

let reverseRegion (array : array<int>) start length =
    for i in start .. (start + (length / 2) - 1) do
        let reversePos = start + length - 1 - (i - start)
        let tmp = array.[i % array.Length]
        array.[i % array.Length] <- array.[reversePos % array.Length]
        array.[reversePos % array.Length] <- tmp

let rec proc array input pos skip =
    match input with
    | h :: t ->
        reverseRegion array pos h
        proc array t (pos + h + skip) (skip + 1)
    | [] -> (pos, skip)

// Part 1
// let input = [ 46; 41; 212; 83; 1; 255; 157; 65; 139; 52; 39; 254; 2; 86; 0; 204 ]
// proc array input 0 0

// printfn "Product of the two first items: %d" (array.[0] * array.[1])

// Part 2
// let inputString = "46,41,212,83,1,255,157,65,139,52,39,254,2,86,0,204"

let inputArray (inputString : string) =
    let codes = inputString
                |> Encoding.ASCII.GetBytes
                |> Seq.map (fun b -> (int)b)
                |> Seq.toList

    List.append codes [ 17; 31; 73; 47; 23 ]

let rec procTimes array input pos skip times =
    if times > 0
    then
        let (nextPos, nextSkip) = proc array input pos skip
        procTimes array input nextPos nextSkip (times - 1)
    else
        ignore ()

let densify array =
    let rec densifyRec (array : int[]) pos acc =
        if pos > 0
        then
            let xor = array.[pos] ^^^ array.[pos - 1] ^^^ array.[pos - 2] ^^^ array.[pos - 3] ^^^ array.[pos - 4] ^^^ array.[pos - 5] ^^^ array.[pos - 6] ^^^ array.[pos - 7] ^^^ array.[pos - 8] ^^^ array.[pos - 9] ^^^ array.[pos - 10] ^^^ array.[pos - 11] ^^^ array.[pos - 12] ^^^ array.[pos - 13] ^^^ array.[pos - 14] ^^^ array.[pos - 15]
            densifyRec array (pos - 16) (xor :: acc)
        else acc
    
    densifyRec array (array.Length - 1) []

let knotHash input =
    let array = [| 0 .. 255 |]
    procTimes array (inputArray input) 0 0 64
    densify array
    |> List.map (sprintf "%02x")
    |> String.concat ""

let tobin hex =
    match hex with
    | '0' -> "0000"
    | '1' -> "0001"
    | '2' -> "0010"
    | '3' -> "0011"
    | '4' -> "0100"
    | '5' -> "0101"
    | '6' -> "0110"
    | '7' -> "0111"
    | '8' -> "1000"
    | '9' -> "1001"
    | 'a' -> "1010"
    | 'b' -> "1011"
    | 'c' -> "1100"
    | 'd' -> "1101"
    | 'e' -> "1110"
    | 'f' -> "1111"
    | _ -> failwith "invalid input"

let usedSquares = [| 0..127 |]
                  |> Array.map
                      ((sprintf "wenycdww-%d") >>
                      knotHash >>
                      (fun h -> h |> Seq.map tobin |> String.concat ""))
                  |> Array.sumBy (fun binary -> (binary |> Seq.where (fun b -> b = '1') |> Seq.length))

let map = [| 0..127 |]
          |> Array.map
              ((sprintf "wenycdww-%d") >>
              knotHash >>
              (fun h -> h |> Seq.map tobin |> String.concat "") >>
              (fun l -> l |> Seq.map (fun c -> c = '1') |> Array.ofSeq))

let visited : bool[][] = [| 0..127 |] |> Array.map (fun _ -> Array.create 128 false)

let step (x, y) =
    if x < 127
    then (x + 1, y)
    else (0, y + 1)

let rec visitRegion (map : bool[][]) (visited : bool[][]) (x, y) =
    if x < 0 || x > 127 || y < 0 || y > 127 || (not map.[y].[x]) || visited.[y].[x]
    then visited
    else
        visited.[y].[x] <- true
        let visited = visitRegion map visited ((x + 1), y)
        let visited = visitRegion map visited (x, (y + 1))
        let visited = visitRegion map visited ((x - 1), y)
        let visited = visitRegion map visited (x, (y - 1))
        visited

let rec countRegions (map : bool[][]) (visited : bool[][]) (x, y) =
    if x = 127 && y = 127
    then if map.[y].[x] && (not visited.[y].[x])
         then 1
         else 0
    else 
        if visited.[y].[x]
        then countRegions map visited (step (x, y))
        else
            if map.[y].[x]
            then
                let visited = visitRegion map visited (x, y)
                1 + countRegions map visited (step (x, y)) 
            else countRegions map visited (step (x, y))

countRegions map visited (0, 0)