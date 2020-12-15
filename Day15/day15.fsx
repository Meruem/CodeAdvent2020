let input = [1;0;15;2;10;13]

let iterate max (input: int list) =
    
    let rec loop index last (map: Map<int,int>) =
        if index = max then map, last else 
            if index < input.Length - 1 then
                let item = input.[index] 
                let map' = map |> Map.add item index
                loop (index + 1) item map' 
            elif index = input.Length - 1 then 
                loop (index + 1) input.[index] map
            else 
                let dif = if map |> Map.containsKey last then index - map.[last] - 1 else 0
                loop (index + 1) dif (map |> Map.add last (index-1))   

    loop 0 0 Map.empty
 

let (_, res1) = iterate 2020 input
let (_, res2) = iterate 30000000 input

printfn "15_1: %A" res1
printfn "15_2: %A" res2
