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
let particles = input |> Array.map parse |> List.ofArray

let minElem = particles
              |> List.minBy (fun p -> dist p.Acceleration, dist p.Velocity)

let withSameAcc = particles |> List.filter (fun p -> (dist p.Acceleration) = (dist minElem.Acceleration))

let maxIndex = List.findIndex (fun p -> p.Position = minElem.Position && p.Velocity = minElem.Velocity && p.Acceleration = minElem.Acceleration) particles

let add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

let stepParticle particle =
    let newVelocity = add particle.Velocity particle.Acceleration

    {
        Position = add particle.Position newVelocity
        Velocity = newVelocity
        Acceleration = particle.Acceleration
    }

let step particles =
    particles
    |> List.map stepParticle
    |> List.groupBy (fun p -> p.Position)
    |> List.filter (fun (_, ps) -> List.length ps = 1)
    |> List.collect snd

let rec stepn n particles =
    if n = 0
    then particles
    else step particles |> stepn (n - 1)

let initialLength = List.length particles
let after100Steps = stepn 100 particles |> List.length
let after1000Steps = stepn 1000 particles |> List.length
let after10000Steps = stepn 10000 particles |> List.length