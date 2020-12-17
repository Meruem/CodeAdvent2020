#load "../utils/utils.fsx"
open System.IO

let filePath = "./Input/Input17.txt"
let file = File.ReadLines(filePath)

type Dimension =
  { 
    Dim : int
    Min: int list
    Max: int list
    Cubes: Set<int list> }
    with
        static member Init dim = 
          { Dim = dim
            Min = [0..dim-1] |> List.map (fun _ -> 0)
            Max = [0..dim-1] |> List.map (fun _ -> 0)
            Cubes = Set.empty}

let addCube dimension (pos: int list)  =
    let min = dimension.Min |> List.mapi (fun i el -> min el pos.[i])
    let max = dimension.Max |> List.mapi (fun i el -> max el pos.[i])
    { dimension with Min = min; Max = max; Cubes = dimension.Cubes |> Set.add pos }    

let add l1 l2 = 
    List.zip l1 l2
    |> List.map (fun (a,b) -> a + b)

let neig n = 
    let dir = [-1;0;1]
    [1..n] 
    |> List.map (fun _ -> dir)
    |> Utils.cartesian
    |> List.filter (fun lst -> not (lst |> List.forall (fun i -> i = 0)))

let isActive pos dimension =
    dimension.Cubes |> Set.contains pos 

let getNeigCount pos dimension =
    neig dimension.Dim
    |> List.sumBy ( fun n -> if dimension |> isActive (pos |> add n) then 1 else 0)    

let iterate (dimension: Dimension) =
    [0..dimension.Dim-1]
    |> List.map (fun d -> [dimension.Min.[d]-1..dimension.Max.[d]+1])
    |> Utils.cartesian
    |> List.fold (fun dim pos -> 
        let nbCount = getNeigCount pos dimension
        if (dimension |> isActive pos && nbCount = 2) || (nbCount = 3) then
            addCube dim pos
        else dim )
        (Dimension.Init dimension.Dim)

let input = file |> List.ofSeq

let initDimension (input: string list) dim =
    let otherDim = [1..dim-2] |> List.map (fun _ -> 0)
    [for y in 0..input.Length - 1 do 
        for x in 0..input.[y].Length - 1 do 
            if input.[x].[y] = '#' then yield x::y::otherDim] 
    |> List.fold addCube (Dimension.Init dim)               

let resCubes =
    [1..6] 
    |> List.fold (fun acc i -> 
        printfn "iteration: %d" i
        iterate acc) 
        (initDimension input 3)   

let res1 = resCubes.Cubes.Count    

let resCubes2 =
    [1..6] 
    |> List.fold (fun acc i -> 
        printfn "iteration: %d" i
        iterate acc) 
        (initDimension input 4)   

let res2 = resCubes2.Cubes.Count  

printfn "17_1: %d" res1
printfn "17_2: %d" res2
