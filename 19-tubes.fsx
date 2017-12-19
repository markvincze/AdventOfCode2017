open System
open System.IO

let input = File.ReadAllLines("19-tubes-input.txt")
let (x, y) = input.[0].IndexOf('|'), 0

type Direction =
| Up
| Right
| Down
| Left

let rec travel (input : string[]) x y from letters cnt =
    let current = input.[y].[x]
    let letters = if Char.IsLetter current
                  then letters + (string current)
                  else letters
    
    let (couldStep, x, y, from) = 
        match from with
        | Left -> if (current <> '+' && input.[y].[x + 1] <> ' ') then (true, x + 1, y, Left)
                  else if input.[y + 1].[x] = '|' then (true, x, y + 1, Up)
                  else if input.[y].[x + 1] = '-' then (true, x + 1, y, Left)
                  else if input.[y - 1].[x] = '|' then (true, x, y - 1, Down)
                  else (false, x, y, Left)
        | Up -> if (current <> '+' && input.[y + 1].[x] <> ' ') then (true, x, y + 1, Up)
                else if input.[y].[x + 1] = '-' then (true, x + 1, y, Left)
                else if input.[y + 1].[x] = '|' then (true, x, y + 1, Up)
                else if input.[y].[x - 1] = '-' then (true, x - 1, y, Right)
                else (false, x, y, Left)
        | Right -> if (current <> '+' && input.[y].[x - 1] <> ' ') then (true, x - 1, y, Right)
                   else if input.[y + 1].[x] = '|' then (true, x, y + 1, Up)
                   else if input.[y].[x - 1] = '-' then (true, x - 1, y, Right)
                   else if input.[y - 1].[x] = '|' then (true, x, y - 1, Down)
                   else (false, x, y, Left)
        | Down -> if (current <> '+' && input.[y - 1].[x] <> ' ') then (true, x, y - 1, Down)
                  else if input.[y].[x + 1] = '-' then (true, x + 1, y, Left)
                  else if input.[y - 1].[x] = '|' then (true, x, y - 1, Down)
                  else if input.[y].[x - 1] = '-' then (true, x - 1, y, Right)
                  else (false, x, y, Left)

    if couldStep 
    then travel input x y from letters (cnt + 1)
    else letters, (cnt + 1)

let result = travel input x y Up "" 0