let part1 list = 
    (0 :: list)
    |> List.sortDescending
    |> (fun l -> l.Head + 3 :: l)
    |> List.rev
    |> List.pairwise 
    |> List.countBy (fun (a, b) -> b - a)
    |> Map.ofList
    |> (fun map -> map.Item 1 * map.Item 3)

let part2 list = 0

[<EntryPoint>]
let main argv =
    let lines = System.IO.File.ReadAllLines("data.txt") |> Seq.toList |> List.map int

    lines |> part1 |> printfn "%A" 
    lines |> part2 |> printfn "%d"
    0 
