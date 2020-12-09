#load "../utils/utils.fsx"

open System.IO

let filePath = "./Input/Input01.txt"
let file = File.ReadLines(filePath)

let target = bigint 2020

let inputs = file |> Seq.map (int >> bigint) |> List.ofSeq
let res1 = Utils.sumOfTwo target inputs |> Seq.map (fun (a, b) -> a*b)

res1 |> Seq.iter (fun r -> printfn "1_1: %A" r)

let res2 = seq { 
    for i in [0..inputs.Length-3] do
        for j in [i+1..inputs.Length-2] do
            for k in [j+1..inputs.Length-1] do
                if inputs.[i] + inputs.[j] + inputs.[k] = target then yield (inputs.[i] * inputs.[j] * inputs.[k]) }

res2 |> Seq.iter (fun r -> printfn "1_2: %A" r)