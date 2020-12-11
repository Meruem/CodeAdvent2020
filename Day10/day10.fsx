#load "../utils/utils.fsx"
open System.IO

let filePath = "./Input/Input10.txt"
let file = File.ReadLines(filePath)

let input = file |> Seq.map int |> List.ofSeq

let addStartEnd (input: int list) =
    0 :: input @ [input.[input.Length-1] + 3]

let rec iterate index ones threes (input: int list) =
    if index = input.Length - 1 then (ones, threes)
    else 
        match input.[index+1] - input.[index] with
        | 3 -> iterate (index+1) ones (threes+1) input
        | 1 -> iterate (index+1) (ones+1) threes input
        | 2 -> iterate (index+1) ones threes input
        | _ -> failwith "missing step"

let res1ones, res1threes =
    input 
    |> List.sort
    |> addStartEnd
    |> iterate 0 0 0

let res1 = res1ones * res1threes    

let getNextSteps (input: int list) index =
    [for next in index+1..index+3 do
        if next < input.Length && input.[next] - input.[index] <= 3 then yield next]

let tribonachi n =
    let rec tb (map:Map<int,int>) n =
        if map.ContainsKey n then map.[n]
        else
            printfn "not found: %d" n 
            tb map (n-1) + tb map (n-2) + tb map (n-3)    
    tb ([(0,1); (1,1); (2,2); (3,4)] |> Map.ofList) n

let toDiffList lst =
    lst |> List.mapi (fun i item -> if i = 0 then item else item - lst.[i-1])

let res2 =  
    input |> List.sort |> toDiffList |> Utils.splitBy 3 
    |> List.map (fun l -> l.Length |> tribonachi |> int64) |> List.reduce (*)
    