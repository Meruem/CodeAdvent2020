open System.IO

let file = File.ReadLines("./Input/Input13.txt")
let input = file |> List.ofSeq

let timeStamp = input.[0] |> int64

let buses (input: string) = 
    input.Split(',')
    |> Array.mapi (fun i item ->
        if item = "x" then None 
        else
            let bus = int64 item
            let wait = int64 i
            let afterLast = (bus - wait % bus) % bus  
            Some (bus, afterLast))
    |> Array.choose id 
    |> List.ofArray

let (waitTime, bus) = 
    buses input.[1]      
    |> List.map (fun (bus, _) ->
        bus - timeStamp % bus, bus)
    |> List.minBy (fun (wait, _) -> wait)    
     
let res1 = waitTime * bus

let rec solve timestamp modulo buses =  
    match buses with
    | [] -> timestamp
    | (bus, afterTime) :: rest ->
        let newMod = modulo * bus
        if timestamp % bus = afterTime then
            solve timestamp newMod rest
        else 
            solve (timestamp + modulo) modulo buses 

let solveBuses buses =
    let (bus, afterLast) :: rest = buses
    solve afterLast bus rest

let res2 = solveBuses <| buses2 input.[1]
