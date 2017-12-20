open System
open System.IO
open System.Text.RegularExpressions

type Vector = int * int * int

let dist ((x : int), (y : int), (z : int)) = (Math.Abs x) + (Math.Abs y) + (Math.Abs z)

type Particle = {
    Position : Vector
    Velocity : Vector
    Acceleration : Vector
}

let lineRegex = Regex("^p=<(.*)>, v=<(.*)>, a=<(.*)>$")
let parse line =
    let parseVec (str : string) =
        match str.Split(',') with
        | [| c1; c2; c3 |] -> (Int32.Parse c1, Int32.Parse c2, Int32.Parse c3)
        | _ -> failwith "Invalid input"

    let regexMatch = lineRegex.Match(line)
    {
        Position = parseVec regexMatch.Groups.[1].Value
        Velocity = parseVec regexMatch.Groups.[2].Value
        Acceleration = parseVec regexMatch.Groups.[3].Value
    }

let input = File.ReadAllLines "20-particle-swarm-input.txt"
let particles = input |> Array.map parse

let minElem = particles
              |> Array.minBy (fun p -> dist p.Acceleration, dist p.Velocity)

let withSameAcc = particles |> Array.filter (fun p -> (dist p.Acceleration) = (dist minElem.Acceleration))

let maxIndex = Array.findIndex (fun p -> p.Position = minElem.Position && p.Velocity = minElem.Velocity && p.Acceleration = minElem.Acceleration) particles