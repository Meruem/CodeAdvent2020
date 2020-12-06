open System.IO

let filePath = "./Input/Input06.txt"
let file = File.ReadLines(filePath)

let input = file |> List.ofSeq

let opOnSets op (set: Set<char> option) (str: string) =
   let newSet = Set.ofSeq str
   match set with
   | Some s -> Some (op s newSet)
   | None -> Some newSet

let countSetOperationItems op ((count: int), (charSet: Set<char> option)) line = 
    if line = "" then
        (count + charSet.Value.Count, None)
    else
        (count, opOnSets op charSet line)

let res op input = 
    let (parRes, charSet) = 
        input
        |> List.fold (countSetOperationItems op) (0, None)  
    parRes + charSet.Value.Count

let res1 = res Set.union input
let res2 = res Set.intersect input

printfn "6_1: %d" res1
printfn "6_2: %d" res2
