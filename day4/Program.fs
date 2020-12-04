// Learn more about F# at http://fsharp.org

let countList f = List.sumBy (fun n -> if f n then 1 else 0)

let inputRec list =
    let rec f list (acc:list<string>) str =
        match list with
        | [] -> str::acc
        | x::xs -> 
            match x with
            | "" -> f xs (str::acc) ""
            | x -> f xs acc (str + " " + x) 
    f list [] ""

let isValid reqIds passIds =
    reqIds
        |> countList (fun n -> (List.contains n passIds))
        |> (fun n -> n = reqIds.Length)
    
let validPassports reqIds list =
    list 
        |> List.map (fun l -> (List.map (fst) l))
        |> countList (isValid reqIds)
    
let part1 = validPassports ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]

[<EntryPoint>]
let main argv =
    let lines = 
        System.IO.File.ReadAllLines("data.txt") 
        |> Seq.toList
        |> inputRec
        |> List.map (fun str -> 
            (str.Split " ") 
            |> List.ofArray 
            |> List.skip(1) 
            |> List.map ( (fun x -> x.Split ":") >> (fun x -> x.[0], x.[1]) )
        )

    lines |> part1 |> printfn "%d" 
    //lines |> part2 |> printfn "%d"

    0 