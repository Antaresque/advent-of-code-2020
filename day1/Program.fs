let part1 (list:list<int>) = 
    [for x in list do 
        for y in list do 
            yield x,y]
    |> List.filter(fun (a,b) -> a + b = 2020)
    |> List.iter(fun (a,b) -> printfn "%d*%d = %d" a b (a*b))

let part2 (list:list<int>) = 
    [for x in list do 
        for y in list do 
            for z in list do 
                yield x,y,z]
    |> List.filter(fun (a,b,c) -> a + b + c = 2020)
    |> List.iter(fun (a,b,c) -> printfn "%d*%d*%d = %d" a b c (a*b*c))

[<EntryPoint>]
let main argv =
    let lines = 
        System.IO.File.ReadAllLines("data.txt") 
        |> Seq.map System.Int32.Parse 
        |> Seq.toList
    
    lines |> part1
    lines |> part2
    0 
