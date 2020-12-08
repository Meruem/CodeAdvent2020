#load "../utils/utils.fsx"

open System.IO

let filePath = "./Input/Input08.txt"
let file = File.ReadLines(filePath)

type Instruction =
    | Nop of int
    | Acc of int
    | Jmp of int

type ExecutionState =
  { LineNumber : int
    Acc : int}
    with 
        static member start = {LineNumber = 0; Acc = 0}

type ProgramResult = 
    | Infinite of int
    | Success of int
    | Fail        

let parseInstruction line =
    match line with
    | Utils.Regex @"(nop|acc|jmp) ((?:\+|-)\d*)$" [instruction; value] -> 
        match instruction with
        | "nop" -> Nop (int value)
        | "acc" -> Acc (int value)
        | "jmp" -> Jmp (int value)
        | _ -> failwithf "Unknown instruction %s" line
    | _ -> failwithf "Unable to parse line %s" line    

let executeInstruction (executionState:ExecutionState) instruction =
    match instruction with
    | Nop _ -> {executionState with LineNumber = executionState.LineNumber + 1}
    | Acc v -> {LineNumber = executionState.LineNumber + 1; Acc = executionState.Acc + v}
    | Jmp v -> {executionState with LineNumber = executionState.LineNumber + v}

let rec executeProgram (program: Instruction list) (executionState: ExecutionState) (visitedLines: int Set) =
    match executionState.LineNumber with
    | ln when visitedLines.Contains ln -> Infinite executionState.Acc
    | ln when ln = program.Length -> Success executionState.Acc
    | ln when ln > program.Length -> Fail
    | ln ->
        let newState = executeInstruction executionState program.[ln]
        executeProgram program newState (visitedLines |> Set.add ln)

let executeNewProgram program = executeProgram program ExecutionState.start Set.empty

let program = file |> List.ofSeq |> List.map parseInstruction

let res1 = executeNewProgram program

let updateElementAt pos newElement list =
    list |> List.mapi (fun i el -> if i = pos then newElement else el)

let res2 =
    program 
    |> List.mapi (fun i inst -> i, inst)
    |> List.filter (fun (_, inst) -> 
        match inst with 
        | Jmp _ | Nop _ -> true
        | _ -> false)
    |> List.map (fun (i, inst) -> 
        let modifiedProgram = 
            match inst with
            | Jmp x -> program |> updateElementAt i (Nop x)
            | Nop x -> program |> updateElementAt i (Jmp x)
            | _ -> failwith "?!"
        executeNewProgram modifiedProgram)
    |> List.find( fun res -> 
        match res with
        | Success _ -> true
        | _ -> false)

printfn "8_1: %A" res1
printfn "8_2: %A" res2
    
