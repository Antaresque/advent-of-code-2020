let binaryPart (chars:list<char>) (str:string) =
    let rec f str a b n = 
        match str with
        | "" -> a
        | str when str.[0] = chars.[0] -> f str.[1..] a (b - n/2) (n/2)
        | str when str.[0] = chars.[1] -> f str.[1..] (a + n/2) b (n/2)
        | _ -> 0

    let n = pown 2 str.Length
    f str 0 (n-1) n

let getRow = binaryPart ['F'; 'B']
let getCol = binaryPart ['L'; 'R']

let getRowAndColumn (str:string) = (getRow str.[0..6]), (getCol str.[7..])
let calcID (row, col) = row * 8 + col

let part1 list = list |> List.map (getRowAndColumn >> calcID) |> List.max
let part2 list = 0

[<EntryPoint>]
let main argv =
    let lines = System.IO.File.ReadAllLines("data.txt") |> Seq.toList

    lines |> part1 |> printfn "%d" 
    lines |> part2 |> printfn "%d"
    0 
