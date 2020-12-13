let trackUntilRepeat (list:list<string*int>) =
    let arr = Array.zeroCreate list.Length
    let rec tracker acc n prevN =        
        if n = arr.Length && prevN = arr.Length-1 then acc
        elif n >= arr.Length then acc
        else 
        match list.[n] with
        | (_, _) when arr.[n] -> acc
        | (s, i) ->
            Array.set arr n true
            match (s, i) with
            | (s, _) when s = "nop" -> tracker acc (n + 1) n
            | (s, i) when s = "acc" -> tracker (acc + i) (n + 1) n
            | (s, i) when s = "jmp" -> tracker acc (n + i) n
            | _ -> -1
    tracker 0 0 0

let switch = function | "nop" -> "jmp" | "jmp" -> "nop" | s -> s

let checkIfRepeats list n =
    let newList = list |> List.mapi (fun i (str, num) -> if i <> n then (str, num) else (switch str, num))
    let arr = Array.zeroCreate newList.Length
    let rec tracker n prevN =
        if n = arr.Length && prevN = arr.Length-1 then true 
        elif n >= arr.Length then false
        else 
            match newList.[n] with
            | (_, _) when arr.[n] -> false
            | (s, i) ->
                Array.set arr n true
                match (s, i) with
                | (s, _) when s = "nop" -> tracker (n + 1) n
                | (s, i) when s = "acc" -> tracker (n + 1) n
                | (s, i) when s = "jmp" -> tracker (n + i) n
                | _ -> false
    tracker 0 0

let trackWithChange list n =
    List.mapi (fun i (str, num) -> if i <> n then (str, num) else (switch str, num)) list
    |> trackUntilRepeat

let part1 = trackUntilRepeat
let part2 list = 
    list 
    |> List.indexed 
    |> List.tryFindIndex (fun (i, _) -> checkIfRepeats list i) 
    |> function 
        | Some(i) -> trackWithChange list i
        | None -> -99

[<EntryPoint>]
let main argv =
    let lines = 
        System.IO.File.ReadAllLines("data.txt") 
        |> Seq.toList
        |> List.map ((fun str -> str.Split " ") >> (fun arr -> (arr.[0], int arr.[1])))
 
    lines |> part1 |> printfn "%d" 
    lines |> part2 |> printfn "%d"
    0 
