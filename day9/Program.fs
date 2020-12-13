let splitter list =
    let rec f list acc =
        match list with
        | []      -> List.rev acc
        | x :: xs -> f xs ((x, xs) :: acc)
    f list [] 

let checkNumber index list =
    let preamb = List.take (index + 25) list |> List.skip index
    let n = List.item (index+25) list 
    [ for x, xs in splitter preamb do
            for y in xs do yield x, y ]
        |> List.tryFind (fun (x, y) -> x + y = n)
        |> function | Some(_) -> true | None -> false

let findSum target index list =
    let rest = List.skip index list
    let rec f list sum n =
        match list with
        | [] -> None
        | x :: xs when x + sum = target -> Some(List.take n rest)
        | x :: xs when x + sum > target -> None
        | x :: xs                       -> f xs (x + sum) (n + 1)
    f rest 0L 1

let sumMinMax list = List.min list + List.max list

let part1 list =
    let rest = list |> List.skip 25 |> List.indexed
    List.tryFind (fun (i, _) -> not (checkNumber i list)) rest
        |> function | Some((_, s)) -> s | None -> 0L

let part2 list =
    let target = part1 list
    list 
    |> List.indexed
    |> List.tryPick (fun (i, _) -> findSum target i list)
    |> function | Some(l) -> sumMinMax l | None -> 0L

[<EntryPoint>]
let main argv =
    let lines = System.IO.File.ReadAllLines("data.txt") |> Seq.toList |> List.map int64

    lines |> part1 |> printfn "%A" 
    lines |> part2 |> printfn "%A"
    0 
