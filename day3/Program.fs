let countList f = List.sumBy (fun n -> if f n then 1 else 0)

let slope x y (arr:list<string>) =
    let len = arr.[0].Length
    match y with
    | 1 -> arr
        |> List.mapi (fun i str -> str.[(i * x) % len])
        |> countList (fun c -> c = '#')
    | _ -> arr
        |> List.indexed
        |> List.filter (fun (i, _) -> (i % y) = 0)
        |> List.mapi (fun i (_, str) -> str.[(i * x) % len])
        |> countList (fun c -> c = '#')

let part1 = slope 3 1

let part2 arr = 
    [(1,1); (3,1); (5,1); (7,1); (1,2)] 
    |> List.map ((fun (a, b) -> slope a b arr) >> int64)
    |> List.reduce ( * )

[<EntryPoint>]
let main _ =
    let lines = 
        System.IO.File.ReadAllLines("data.txt")
        |> Seq.toList

    lines |> part1 |> printfn "%d" 
    lines |> part2 |> printfn "%d"
    0 // return an integer exit code
