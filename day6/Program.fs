let inputRec list =
    let rec f list acc str =
        match list with
        | [] -> str::acc
        | x::xs -> 
            match x with
            | "" -> f xs (str::acc) []
            | x -> f xs acc (x::str)
    f list [] []

let dups str1 str2 = 
    let rec f (s1:string) (s2:string) acc =
        match s2 with
        | "" -> acc
        | x when s1.Contains x.[0] -> f s1 s2.[1..] (acc + string x.[0])
        | _ -> f s1 s2.[1..] acc
    f str1 str2 ""

let part1 = List.sumBy (List.reduce (+) >> Seq.distinct >> Seq.length)
let part2 = List.sumBy (List.reduce dups >> Seq.length)

[<EntryPoint>]
let main argv =
    let lines = 
        System.IO.File.ReadAllLines("data.txt") 
        |> Seq.toList
        |> inputRec

    lines |> part1 |> printfn "%d" 
    lines |> part2 |> printfn "%d"
    0 