open System.IO

let filePath = "./Input/Input03.txt"
let file = File.ReadLines(filePath)

let forest = file |> List.ofSeq

type Vector2 = 
   {X: int
    Y: int}

let makeStep (forest: string list) (step:Vector2) (pos: Vector2)  =
    let width = forest.[0].Length
    {X = (pos.X + step.X) % width; Y = pos.Y + step.Y}

let rec traverse (forest: string list) step count pos =
    if pos.Y >= forest.Length then count
    else
        let newCount = if forest.[pos.Y].[pos.X] = '#' then count + 1 else count 
        let newPos = makeStep forest step pos
        traverse forest step newCount newPos

let startPos = {X=0;Y=0}

let res1 = traverse forest {X=3;Y=1} 0 startPos

let res2 = 
   [{X=1;Y=1}
    {X=3;Y=1}
    {X=5;Y=1}
    {X=7;Y=1}
    {X=1;Y=2}]
    |> List.fold (fun acc step -> 
        let res = traverse forest step 0 startPos
        acc * (bigint res)) (bigint 1)

printfn "3_1: %d" res1
printfn "3_2: %A" res2    
