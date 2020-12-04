let countList f list = List.filter f list |> List.length

let inputRec list =
    let rec f list (acc:list<string>) str =
        match list with
        | [] -> str::acc
        | x::xs -> 
            match x with
            | "" -> f xs (str::acc) ""
            | x -> f xs acc (str + " " + x) 
    f list [] ""

let isValid passIds =
    let list = passIds |> List.map fst
    ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
        |> List.forall (fun n -> (List.contains n list))

let isReqValid passIds =
    let byr = int >> (fun n -> n >= 1920 && n <= 2002) 
    let iyr = int >> (fun n -> n >= 2010 && n <= 2020)
    let eyr = int >> (fun n -> n >= 2020 && n <= 2030)
    let ecl = function "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true | _ -> false
    let pid (str:string) = if str.Length = 9 then (Seq.forall System.Char.IsDigit str) else false

    let hgtCheck (str:string) =
        match str.[str.Length-2..] with
        | "cm" -> int str.[..str.Length-3] |> (fun n -> n >= 150 && n <= 193)
        | "in" -> int str.[..str.Length-3] |> (fun n -> n >= 59 && n <= 76)
        | _ -> false  
    let hgt (str:string) = if str.Length > 2 then (hgtCheck str) else false

    let isDigitOrAF c = System.Char.IsDigit c || (c >= 'a' && c <= 'f')
    let hclCheck (str:string) =
        match str.[0] with
        | '#' -> Seq.forall isDigitOrAF str.[1..]
        | _ -> false
    let hcl (str:string) = if str.Length = 7 then (hclCheck str) else false

    let func = Map [("byr", byr); ("iyr", iyr); ("eyr", eyr); ("ecl", ecl); ("pid", pid); ("hgt", hgt); ("hcl", hcl); ("cid", (fun _ -> true))]
    let testFunc (a, b) =
        match Map.tryFind a func with
            | Some f -> f b
            | None -> false

    passIds |> List.forall (fun (a, b) -> testFunc (a,b))
    
let part1 list = list |> countList isValid
let part2 list = list |> List.filter isValid |> countList isReqValid

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
    lines |> part2 |> printfn "%d"

    0 