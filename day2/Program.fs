type PassRecord = int * int * char * string
let toPassRecord (str:string) :PassRecord =
    let list = str.Split " "
    let interval = list.[0].Split "-"
    ((interval.[0] |> int),
     (interval.[1] |> int),
     (list.[1].[0] |> char),
     (list.[2]))

let isBetween a b n = (n >= a && n <= b)
let countSeq f = Seq.sumBy (fun n -> if f n then 1 else 0)

let validateAmount (a, b, char, str) :bool =
    str |> countSeq (fun n -> n = char)  
        |> isBetween a b

// indexed from 1
let validateTwoChars (a, b, char, str) :bool =
    str |> Seq.indexed 
        |> countSeq (fun (i, c) -> (i = a-1 || i = b-1) && c = char)
        |> fun n -> n = 1

let part1 = countSeq validateAmount
let part2 = countSeq validateTwoChars

[<EntryPoint>]
let main argv =
    let lines = 
        System.IO.File.ReadAllLines("data.txt") 
        |> Seq.map toPassRecord
        |> Seq.toList
   
    printfn "%d" (lines |> part1)
    printfn "%d" (lines |> part2)
    0 