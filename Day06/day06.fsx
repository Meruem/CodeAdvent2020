#load "../utils/utils.fsx"
open System.IO

let filePath = "./Input/Input06.txt"
let file = File.ReadLines(filePath)

let input = file |> List.ofSeq

let res operation input =
    input 
    |> Utils.splitBy ""
    |> List.sumBy (fun group -> 
        (group
        |> List.map Set.ofSeq
        |> List.reduce operation
        ).Count)             

let res1 = res Set.union input
let res2 = res Set.intersect input

printfn "6_1: %d" res1
printfn "6_2: %d" res2
