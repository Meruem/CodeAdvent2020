open System.IO

let filePath = "./Input/Input11.txt"
let file = File.ReadLines(filePath)

let getAtPos (x, y) (arr: char[,]) =
    if x >= (arr |> Array2D.length1) || x < 0 || y >= (arr |> Array2D.length2) || y < 0 then 'X'
    else
        arr.[x, y]

let input = file |> List.ofSeq

let initializeArray (input: string list) = 
    let arr = Array2D.create input.[0].Length input.Length '.'     
    input
    |> List.iteri (fun y line -> 
        line |> Seq.iteri (fun x ch -> arr.[x,y] <- ch ))   
    arr    

let directions =
    [ (-1, -1)
      (-1, 0)
      (-1, 1)
      (0, -1)
      (0, 1)
      (1, -1)
      (1, 0)
      (1, 1) ]  

let rec getFirstByDirection (x, y) (dirx, diry) (arr: char[,]) =
    let newPos = x + dirx, y + diry 
    match getAtPos newPos arr with 
    | '.' -> getFirstByDirection newPos (dirx, diry) arr
    | x -> x

let getNextInDirection (x, y) (dirx, diry) (arr: char[,]) =
    getAtPos (x+dirx, y+diry) arr

let getOccupiedNeighborCount pos getFromDirection (arr: char[,]) =  
    directions  
    |> List.sumBy (fun dir -> if getFromDirection pos dir arr = '#' then 1 else 0)

let nextStep maxNeighbours getNeighbor arr =
    let mutable changed = false
    let newArr = Array2D.init (arr |> Array2D.length1) (arr |> Array2D.length2) (fun x y ->
        let neighbours = getOccupiedNeighborCount (x, y) getNeighbor arr
        let newEl = 
            if neighbours = 0 && arr.[x,y] = 'L' then '#'
            elif neighbours >= maxNeighbours && arr.[x,y] = '#' then 'L'
            else arr.[x,y]
        if newEl <> arr.[x,y] then changed <- true    
        newEl)
    changed, newArr

let rec iterateUntilSame maxNeighbours getNeigbour arr = 
    let (changed, newArr) = nextStep maxNeighbours getNeigbour arr
    if changed then iterateUntilSame maxNeighbours getNeigbour newArr
    else arr

let res1 = 
    initializeArray input  
    |> iterateUntilSame 4 getNextInDirection
    |> Seq.cast<char> 
    |> Seq.sumBy (fun x -> if x = '#' then 1 else 0)

let res2 = 
    initializeArray input  
    |> iterateUntilSame 5 getFirstByDirection
    |> Seq.cast<char> 
    |> Seq.sumBy (fun x -> if x = '#' then 1 else 0)    
