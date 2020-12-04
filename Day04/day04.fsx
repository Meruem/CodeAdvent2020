#load "../utils/utils.fsx"

open System.IO
open System.Text.RegularExpressions

let filePath = "./Input/Input04.txt"
let file = File.ReadLines(filePath)

let input = file |> List.ofSeq

let parseKvp (kvp:string) =
    let elements = kvp.Split(":")
    elements.[0], elements.[1]

let parseLine (line: string) =
    line.Split(" ") |> Array.map parseKvp |> List.ofArray

let (total, last) =
    input
    |> List.fold (fun (total, current) line -> 
        if line = "" then (total @ [current |> Map.ofList], [])
        else
            (total,current @ parseLine line))
        ([], [])

let passports = total @ [last |> Map.ofList]

let requiredKeys = 
    [
        "byr" //(Birth Year)
        "iyr" //(Issue Year)
        "eyr" //(Expiration Year)
        "hgt" //(Height)
        "hcl" //(Hair Color)
        "ecl" //(Eye Color)
        "pid" //(Passport ID)
       // "cid" //(Country ID)
    ]


let isValidSimple (passport: Map<string, string>) =
    requiredKeys |> List.forall passport.ContainsKey

let res1 = passports |> List.sumBy (fun p -> if isValidSimple p then 1 else 0)

type Passport = Map<string,string>    
type Validation = Passport -> bool


let matchRegex key regex (passport: Passport) = 
    if not (passport.ContainsKey key) then false   
    else Regex.Match(passport.[key], regex).Success

let matchRegexValues key regex (passport: Passport) = 
    if not (passport.ContainsKey key) then []   
    else 
        match passport.[key] with 
        | Utils.Regex regex values -> values
        | _ -> []

let heightValidation values =
    match values with 
    | [sizeStr; heightType] ->
        let size = int sizeStr
        match heightType with 
        | "in" -> size >= 59 && size <= 76
        | "cm" -> size >= 150 && size <= 193
        | _ -> false
    | _ -> false    

let between a b (values: string list) = 
    match values with 
    | [value] -> 
        let intVal = int value
        a <= intVal && intVal <= b
    | _ -> false

let validations =
    [
        matchRegexValues "byr" @"^([0-9]{4})$" >> between 1920 2002
        matchRegexValues "iyr" @"^([0-9]{4})$" >> between 2010 2020
        matchRegexValues "eyr" @"^([0-9]{4})$" >> between 2020 2030
        matchRegexValues "hgt" @"^(\d*)(in|cm)$" >> heightValidation
        matchRegex "hcl" @"^#[0-9a-f]{6}$"
        matchRegex "ecl" @"^amb|blu|brn|gry|grn|hzl|oth$"
        matchRegex "pid" @"^[0-9]{9}$"
    ]

let isValid (validations: Validation list) (passport: Passport) =
    validations |> List.forall (fun validation -> validation passport)

let res2 = passports |> List.sumBy (fun p -> if isValid validations p then 1 else 0)

printfn "4_1: %d" res1
printfn "4_2: %d" res2
