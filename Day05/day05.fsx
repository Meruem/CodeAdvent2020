open System.IO

let filePath = "./Input/Input05.txt"
let file = File.ReadLines(filePath)
let input = file |> List.ofSeq

let chooseRange useLower (min, max) = 
    let size = max - min + 1
    let mid = min + size / 2 
    if useLower then 
        min, mid
    else 
        mid, max    

let iterateRange str lowerCh min max =     
    let (res, _) =
        str
        |> Seq.fold (fun range ch -> 
            let useLower = ch = lowerCh
            chooseRange useLower range)
            (min, max)
    res

let getRow (str : string) = 
    iterateRange str.[0..6] 'F' 0 127

let getColumn (str : string) =
    iterateRange str.[7..9] 'L' 0 7

let getID row column = row * 8 + column 

let res = 
    input 
    |> List.map (fun line -> getID (getRow line) (getColumn line)) 

let res1 = List.max res

let resSet = res |> Set.ofList
let res2 = [1..128*8-1] |> List.filter( fun i -> not (resSet.Contains i) && resSet.Contains (i-1) && resSet.Contains (i+1))

printfn ("5_1: %d") res1
printfn ("5_2: %d") res2.[0]
