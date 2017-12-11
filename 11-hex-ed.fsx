open System
open System.IO

let input = File.ReadAllText("11-hex-ed-input.txt");

// Using a "naive" coordinate system
let rec getPos x y steps =
    match steps with
    | h :: t -> match h with
                    | "n" -> getPos x (y + 2) t
                    | "ne" -> getPos (x + 1) (y + 1) t
                    | "se" -> getPos (x + 1) (y - 1) t
                    | "s" -> getPos x (y - 2) t
                    | "sw" -> getPos (x - 1) (y - 1) t
                    | "nw" -> getPos (x - 1) (y + 1) t
                    | _ -> failwith "Invalid input"
    | [] -> (x, y)

let x_, y_ = input.Split(',') |> Array.toList |> getPos 0 0

let dist1 = Math.Abs(x_) + (Math.Abs(y_) - Math.Abs(x_)) / 2

let dist (x : int) (y : int) (z : int) = (Math.Abs(x) + Math.Abs(y) + Math.Abs(z)) / 2

// Using the "Cube" coordinate system (https://www.redblobgames.com/grids/hexagons/)
let rec getPos2 x y z furthest steps =
    let newFurthest = Math.Max(furthest, (dist x y z))
    match steps with
    | h :: t -> match h with
                    | "n" -> getPos2 x (y + 1) (z - 1) newFurthest t
                    | "ne" -> getPos2 (x + 1) y (z - 1) newFurthest t
                    | "se" -> getPos2 (x + 1) (y - 1) z newFurthest t
                    | "s" -> getPos2 x (y - 1) (z + 1) newFurthest t
                    | "sw" -> getPos2 (x - 1) y (z + 1) newFurthest t
                    | "nw" -> getPos2 (x - 1) (y + 1) z newFurthest t
                    | _ -> failwith "Invalid input"
    | [] -> (x, y, z, furthest)

let x, y, z, furthest = input.Split(',') |> Array.toList |> getPos2 0 0 0 0
