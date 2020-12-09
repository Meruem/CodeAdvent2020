#load "../utils/utils.fsx"

open System.IO

let filePath = "./Input/Input09.txt"
let file = File.ReadLines(filePath)

let input = file |> List.ofSeq |> List.map bigint.Parse

let isSumOfTwo (item: bigint) (list: bigint list) =
    Utils.sumOfTwo item list |> Seq.isEmpty |> not

let getFirstNonSum len (input: bigint list) =
    seq { 
        for i in [len..input.Length-1] do
            if not <| isSumOfTwo input.[i] input.[(i-len)..(i-1)] then yield input.[i] }

let res1list = getFirstNonSum 25 input |> List.ofSeq
let res1 = res1list.[0]        

let getContigousSetWithSum target min (input: bigint list) =
  [ let mutable sum = bigint 0
    for i in [min..input.Length-1] do
        sum <- sum + input.[i]
        if sum = target then yield input.[min..i] ]

let res2  =
  [ for i in [0..input.Length-1] do
        let contigousSet = getContigousSetWithSum res1 i input
        if not <| List.isEmpty contigousSet then
            let min = List.min contigousSet.[0]
            let max = List.max contigousSet.[0]
            yield (min + max) ]

printfn "9_1: %A" res1
printfn "9_2: %A" res2
