open System.IO

let filePath = "./Input/Input12.txt"
let file = File.ReadLines(filePath)

let rec applyInstruction ((shipx, shipy), direction) (instruction, n)  =
    match instruction with
    | 'N' -> (shipx, shipy + n), direction
    | 'S' -> (shipx, shipy - n), direction
    | 'E' -> (shipx + n, shipy), direction
    | 'W' -> (shipx - n, shipy), direction
    | 'L' -> (shipx, shipy), (360 + direction - n) % 360
    | 'R' -> (shipx, shipy), (direction + n) % 360
    | 'F' -> 
        match direction with 
        | 0 -> applyInstruction ((shipx, shipy), direction) ('E', n) 
        | 90 -> applyInstruction ((shipx, shipy), direction) ('S', n)
        | 180 -> applyInstruction ((shipx, shipy), direction) ('W', n) 
        | 270 -> applyInstruction ((shipx, shipy), direction) ('N', n) 
        | _ -> failwith "only support 90 angle turns"
    | _ -> failwithf "unsupported instruction %A" instruction

let rec applyInstructionVec ((shipx, shipy), (wayx, wayy)) (instruction, n)  =
    match instruction with
    | 'N' -> (shipx, shipy), (wayx, wayy + n)
    | 'S' -> (shipx, shipy), (wayx, wayy - n)
    | 'E' -> (shipx, shipy), (wayx + n, wayy)
    | 'W' -> (shipx, shipy), (wayx - n, wayy)
    | 'L' -> applyInstructionVec ((shipx, shipy), (wayx, wayy)) ('R', 360 - n)
    | 'R' -> 
        match n with 
        | 0 -> ((shipx, shipy), (wayx, wayy))
        | 90 -> ((shipx, shipy), (wayy, -wayx))
        | 180 -> ((shipx, shipy), (-wayx, -wayy))
        | 270 -> ((shipx, shipy), (-wayy, wayx))
        | _ -> failwith "only support 90 angle turns"

    | 'F' -> 
        if n = 0 then ((shipx, shipy), (wayx, wayy))
        else 
            applyInstructionVec ((shipx + wayx, shipy + wayy), (wayx, wayy)) (instruction, n - 1) 
    | _ -> failwithf "unsupported instruction %A" instruction


let parseInputLine (line: string) =
    let inst = line.[0]
    let n = line.[1..] |> int
    inst, n

let startShip = ((0,0),0)

let input =
    file
    |> Seq.map parseInputLine
    |> List.ofSeq

let ((posx, posy), _) =
    input
    |> List.fold applyInstruction startShip

let res1 = abs(posx) + abs(posy)

let ((pos2x, pos2y), _) = 
    input 
    |> List.fold applyInstructionVec ((0,0), (10,1))

let res2 = abs(pos2x) + abs(pos2y)

printfn "12_1: %d" res1
printfn "12_2: %d" res2
