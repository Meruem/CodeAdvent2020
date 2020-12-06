open System.IO

let filePath = "./Input/Input06.txt"
let file = File.ReadLines(filePath)

let splitBy element lst =
    let folder  line acc =
        let current, result = acc
        if line = element then ([], current :: result)
        else (line :: current, result)
    let (current, partResult) =
        List.foldBack folder lst ([],[])
    current :: partResult

let input = file |> List.ofSeq

let res operation input =
    input 
    |> splitBy ""
    |> List.sumBy (fun group -> 
        (group
        |> List.map Set.ofSeq
        |> List.reduce operation
        ).Count)             

let res1 = res Set.union input
let res2 = res Set.intersect input

printfn "6_1: %d" res1
printfn "6_2: %d" res2
