#load "../utils/utils.fsx"
open System.IO
open System.Text.RegularExpressions

let filePath = "./Input/Input19.txt"
let file = File.ReadLines(filePath)

type Rule =
| Char of string
| SubRules of int * int option
| OrSubRules of (int*int option)*(int*int option)

let toStr ruleN (rules: Map<int,Rule>) =
    let rec loop (ruleN: int) =
        let strOption (r1, r2) =
            match r2 with
            | Some r2 -> sprintf "%s%s" (loop r1) (loop r2)
            | None -> loop r1
        match rules.[ruleN] with
        | Char ch -> ch
        | SubRules (sub1, sub2) -> strOption (sub1,sub2)
        | OrSubRules (left, right) -> sprintf "(%s|%s)" (strOption left) (strOption right)
    loop ruleN

let parseRuleLine (line: string) =
    match line with
    | Utils.Regex @"^(\d*): (.*)$" [id; rules] ->
        let elements = rules.Split(' ')
        let id = int id
        let getSubrules ar = 
            match ar with
            | [|s1|] -> int s1, None
            | [|s1;s2|] -> int s1, Some (int s2)
            | _ -> failwithf "unable to parse line %s" line
        match elements with
        | [|el|] when el.Contains("\"") -> id, Char (el.Replace("\"", ""))
        | ar when ar |> Array.contains ("|") -> 
            let idx = ar |> Array.findIndex (fun e -> e = "|")
            id, OrSubRules (getSubrules ar.[0..idx-1],  getSubrules ar.[idx+1..])
        | ar -> id, SubRules (getSubrules ar)    
    | _ -> failwithf "unable to parse line %s" line

let rules, words = 
    match 
        file 
        |> List.ofSeq
        |> Utils.splitBy ("") with
    | [ruleLines;words] -> 
        let rules = ruleLines |> List.map parseRuleLine |> Map.ofList
        rules, words
    | _ -> failwith "unable to parse"    

let res1 =
    let regex = sprintf "^%s$" <| toStr 0 rules
    words           
    |> List.sumBy (fun w -> 
        let valid = Regex.Match(w, regex).Success
        if valid then 1 else 0)

let res2 =
    let tostr42 = toStr 42 rules
    let tostr31 = toStr 31 rules
    let regexs =  
        [1..10] 
        |> List.map (fun i -> 
            let tostr42n = [1..i] |> List.map (fun _ -> tostr42) |> List.reduce (+)
            let tostr31n = [1..i] |> List.map (fun _ -> tostr31) |> List.reduce (+)
            sprintf "^%s+%s%s$" tostr42 tostr42n tostr31n)      
    words           
    |> List.sumBy (fun w -> 
        let valid =  regexs |> List.exists (fun regex -> Regex.Match(w, regex).Success)
        if valid then 1 else 0)

printfn "19_1: %d" res1
printfn "19_1: %d" res2
