let part1 list = 0
let part2 list = 0

[<EntryPoint>]
let main argv =
    let lines = System.IO.File.ReadAllLines("data.txt") |> Seq.toList

    lines |> part1 |> printfn "%d" 
    lines |> part2 |> printfn "%d"
    0 
