#load "../utils/utils.fsx"

open System.IO

let filePath = "./Input/Input07.txt"
let file = File.ReadLines(filePath)

let input = file |> List.ofSeq

let parseChilds (parent:string) (childs:string) =
    if childs = "no other bags" then []
    else
        childs.Split(", ")
        |> Array.map (fun items -> 
            let words = items.Split(" ")
            (parent, (words.[1..(words.Length-2)] |> String.concat " "), (int words.[0])))
        |> List.ofArray    

let parseLine line =
    match line with
    | Utils.Regex @"^(.*) bags contain (.*)\.$" [parent; childs] -> parseChilds parent childs
    | _ -> failwithf "Unable to parse line %s" line

let tree = List.collect parseLine input
    
// child --> parents map
let parentTree tree = 
    tree
    |> List.fold (fun (acc: Map<string, string list>) (parent, child, count) -> 
        let parents = 
            match acc.TryFind child with
            | Some p -> p
            | None -> []
        acc |> Map.add child (parent :: parents)) Map.empty

let rec findParents (parentTree: Map<string, string list>) queue traversed result =
    match queue with 
    | [] -> result
    | item :: rest ->
        match parentTree.TryFind item with 
        | None -> findParents parentTree rest (traversed |> Set.add item) (result |> Set.add item)
        | Some parents -> 
            let newParents = parents |> List.filter (fun p -> not (Set.contains p traversed) && not (List.contains p queue))
            findParents parentTree (rest @ newParents) (traversed |> Set.add item) (result |> Set.add item)

let res1l = findParents (parentTree tree) ["shiny gold"] Set.empty Set.empty
let res1 = res1l.Count - 1

// parent --> children map
let childTree tree = 
    tree
    |> List.fold (fun (acc: Map<string, (string * int) list>) (parent, child, count) -> 
        let childs = 
            match acc.TryFind parent with
            | Some c -> c
            | None -> []
        acc |> Map.add parent ((child, count) :: childs)) Map.empty

let rec getCost (tree: Map<string, (string * int) list>) item =
    match tree.TryFind item with
    | None -> 1
    | Some childs -> (childs |> List.sumBy (fun (child, count) -> count * getCost tree child)) + 1

let res2 = (getCost (childTree tree) "shiny gold") - 1

printfn "7_1: %d" res1
printfn "7_2: %d" res2
