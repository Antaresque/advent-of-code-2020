open System

type Record = string * list<int*string>
let countSum f list = List.filter f list |> List.length

let checkContains (dict:Map<string, list<int*string>>) target record =
    let rec f (record:Record) =
        match record with
        | color, _ when color = target    -> true
        | _, data  when List.isEmpty data -> false
        | _, data                         -> List.filter (fun (_, str) -> f (str, dict.Item str)) data 
                                                |> List.length 
                                                |> (fun n -> n > 0)
    f record

let countContains (dict:Map<string, list<int*string>>) targetList =
    let rec f list acc =
        match list with
        | [] -> acc
        | (i, str) :: xs -> (f xs acc) + i * (f (dict.Item str) 1)  
    f targetList 1

let part1 (list:list<Record>) =
    let target = "shiny gold"
    let data = Map.ofList list
    countSum (fun n -> checkContains data target n) list - 1 // hack to subtract bag itself
    
let part2 list = 
    let target = "shiny gold"
    let data = Map.ofList list
    countContains data (data.Item target) - 1

let dataConv (arr:string[]) :list<int*string> = 
    let rec slice = function | [] -> [] | list -> (List.take 3 list) :: (slice (List.skip 4 list)) 
    let sliceToData (list:list<string>) = (int list.[0], list.[1] + " " + list.[2])

    if arr.Length <= 3 
        then [] 
        else List.ofArray arr |> slice |> List.map sliceToData

let lineToData = 
    List.map ((fun (line:string) -> line.Split ' ') 
    >> (fun arr -> (arr.[0] + " " + arr.[1], dataConv arr.[4..] )))  


[<EntryPoint>]
let main argv =
    let lines =
        System.IO.File.ReadAllLines("data.txt") 
        |> Seq.toList 
        |> lineToData

    //lines |> List.iter (fun (n, s) -> if n = "shiny gold" then printf "%A" s)
    lines |> part1 |> printfn "%d" 
    lines |> part2 |> printfn "%d"
    0 
