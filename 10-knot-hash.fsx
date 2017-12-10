open System
open System.Text

let array = [| 0 .. 255 |]

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
let inputString = "46,41,212,83,1,255,157,65,139,52,39,254,2,86,0,204"
let inputArray =
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

procTimes array inputArray 0 0 64

let densify array =
    let rec densifyRec (array : int[]) pos acc =
        if pos > 0
        then
            let xor = array.[pos] ^^^ array.[pos - 1] ^^^ array.[pos - 2] ^^^ array.[pos - 3] ^^^ array.[pos - 4] ^^^ array.[pos - 5] ^^^ array.[pos - 6] ^^^ array.[pos - 7] ^^^ array.[pos - 8] ^^^ array.[pos - 9] ^^^ array.[pos - 10] ^^^ array.[pos - 11] ^^^ array.[pos - 12] ^^^ array.[pos - 13] ^^^ array.[pos - 14] ^^^ array.[pos - 15]
            densifyRec array (pos - 16) (xor :: acc)
        else acc
    
    densifyRec array (array.Length - 1) []

let dense = densify array

printfn "Dense array length: %d" dense.Length

printf "Dense hex: "
dense
|> List.iter (fun i -> printf "%02x" i)
printfn ""