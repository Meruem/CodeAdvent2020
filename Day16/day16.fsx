#load "../utils/utils.fsx"
open System.IO

let filePath = "./Input/Input16.txt"
let file = File.ReadLines(filePath)

type FieldDef = 
  { Field : string
    Min1 : int
    Max1 : int
    Min2 : int
    Max2 : int } 

let parseRuleLine line =
    match line with
    | Utils.Regex @"^(.*): (\d*)-(\d*) or (\d*)-(\d*)$"  [field;min1;max1;min2;max2] -> 
        { Field = field; Min1 = int min1; Max1 = int max1; Min2 = int min2; Max2 = int max2 }
    | _ -> failwithf "Unable to parse rule line %s." line

let parseTicketLine (line:string) =
    line.Split(",")
    |> Array.map int
    |> List.ofArray

let rules, ticket, otherTickets = 
    let fileSections =
        file 
        |> List.ofSeq    
        |> Utils.splitBy ""

    fileSections.[0] |> List.map parseRuleLine,
    fileSections.[1].[1] |> parseTicketLine,
    fileSections.[2].[1..] |> List.map parseTicketLine

let isValid (rule: FieldDef) value =
    value >= rule.Min1 && value <= rule.Max1 || value >= rule.Min2 && value <= rule.Max2

let res1 =
    otherTickets
    |> List.collect id
    |> List.sumBy (fun value -> 
        if rules |> List.forall (fun rule -> not (isValid rule value)) then value else 0)

let validTickets =
    otherTickets
    |> List.filter (fun ticket -> 
        ticket 
        |> List.forall (fun value -> rules |> List.exists (fun rule -> isValid rule value)))

let allPossibleComb =
    [0..rules.Length-1] 
    |> List.map (fun i -> i, rules) 
    |> Map.ofList

let removeResultCombination excludeRule (possibleComb: Map<int, FieldDef list>)  =
    possibleComb 
    |> Seq.fold (fun acc kvp -> 
        acc |> Map.add kvp.Key (kvp.Value |> List.filter (fun rule -> rule.Field <> excludeRule.Field)))
        possibleComb

let checkTicket (possibleComb: Map<int, FieldDef list>) (ticket: int list) =
    let (res, _) =
        ticket
        |> List.fold (fun ((comb: Map<int, FieldDef list>), i) value ->
            let newRules = 
                comb.[i] 
                |> List.filter (fun rule -> isValid rule value)
            comb |> Map.add i newRules, (i+1)    
            )
            (possibleComb, 0)
    res        

let rec findValidCombination result (possibleComb: Map<int, FieldDef list>) =
    match possibleComb |> Seq.tryFind (fun kvp -> kvp.Value.Length = 1) with
    | Some single -> 
        let result' = (single.Key, single.Value.[0]) :: result
        let comb = possibleComb |> removeResultCombination single.Value.[0]
        findValidCombination result' comb
    | None -> result
   
let validCombination =
    validTickets 
    |> List.fold checkTicket allPossibleComb 
    |> findValidCombination []
    |> List.map (fun (a,b) -> b,a)
    |> Map.ofList       

let res2 = 
    rules
    |> List.filter (fun rule -> rule.Field.StartsWith("departure"))
    |> List.map (fun rule -> int64 ticket.[validCombination.[rule]])
    |> List.reduce (*)    

printfn "16_1: %d" res1
printfn "16_2: %d" res2
