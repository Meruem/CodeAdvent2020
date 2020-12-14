#load "../utils/utils.fsx"
open System.IO

let file = File.ReadLines("./Input/Input14.txt")
let input = file |> List.ofSeq

let parseMaskLine (line: string) =
    let max = (pown 2L (line.Length + 1)) - 1L
    let (andMask, orMask, flucts, _) =
        Seq.foldBack (fun ch (andMask, orMask, flucts, i) -> 
            match ch with
            | '0' -> (andMask ^^^ (pown 2L i), orMask, flucts, i + 1)
            | '1' -> (andMask, orMask ||| (pown 2L i), flucts, i + 1)
            |  _ -> (andMask, orMask, i :: flucts, i + 1)) 
            line (max, 0L, [], 0)
    andMask, orMask, flucts


let applyMasksOnValue value ((andMask : int64), (orMask: int64), _) = 
    (value &&& andMask) ||| orMask

let getFluctAddresses (address: int) ((andMask : int64), (orMask: int64), (flucts: int list)) =
    let address' = (int64 address) ||| orMask
    let rec getAddresses address flucts =
        match flucts with
        | [] -> [address]
        | pos :: rest ->
            let newAddress = (1L <<< pos) ^^^ address
            (getAddresses address rest) @ (getAddresses newAddress rest)
    getAddresses address' flucts        

let getStaticAddress address ((andMask : int64), (orMask: int64), (flucts: int list)) =
    [int64 address]

let getStaticValue value _ = value   

let modifyMem getAddresses getValue =
    let (finalMem, _) = 
        input 
        |> List.fold (fun ((mem: Map<int64, int64>), masks) line -> 
            match line with 
            | Utils.Regex "^mask = (.*)$" [mask] -> mem, (parseMaskLine mask)
            | Utils.Regex @"^mem\[(\d*)\] = (\d*)$" [address; value] -> 
                let newValue = getValue (int64 value) masks
                let newAddress = getAddresses (int address) masks
                let newMem = newAddress |> List.fold (fun mem add -> Map.add add newValue mem) mem
                newMem, masks
            | _ -> failwithf "unable to parse line %s" line)
            (Map.empty, (0L, 0L, []))
    finalMem

let res1 = 
    modifyMem getStaticAddress applyMasksOnValue |> Seq.sumBy (fun kvp -> kvp.Value)

let res2 = 
    modifyMem getFluctAddresses getStaticValue |> Seq.sumBy (fun kvp -> kvp.Value)    

printfn "14_1: %d" res1
printfn "14_2: %d" res2
