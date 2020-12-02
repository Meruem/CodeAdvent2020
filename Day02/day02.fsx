#load "../utils/utils.fsx"

open System.IO
open Utils

let filePath = "./Input/Input02.txt"
let file = File.ReadLines(filePath)

let parseInput line =
    match line with
    | Regex @"^(\d+)-(\d+) (\w): (.+)$" [ a; b; ch; text ] -> 
        (int a, int b, char ch, text)
    | _ -> failwithf "Unable to parse line %s" line

let inputs = 
    file 
    |> Seq.map parseInput
    |> List.ofSeq

let checkText min max char (text:string) =
    let count = text |> Seq.sumBy (fun c -> if c = char then 1 else 0)
    count >= min && count <= max

let checkText2 a b char (text:string) =
    let matchA = a < text.Length + 1 && text.[a - 1] = char
    let matchB = b < text.Length + 1 && text.[b - 1] = char
    matchA && not matchB || not matchA && matchB

let res1 = inputs |> List.sumBy (fun (min, max, char, text) -> if checkText min max char text then 1 else 0)    
let res2 = inputs |> List.sumBy (fun (min, max, char, text) -> if checkText2 min max char text then 1 else 0)  

printfn "2_1: %d" res1
printfn "2_1: %d" res2
