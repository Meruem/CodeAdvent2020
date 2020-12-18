open System.IO

let filePath = "./Input/Input18.txt"
let file = File.ReadLines(filePath)

type Token =
    | Plus
    | Mul
    | OpenB
    | CloseB
    | Number of int64

let getClosingParIdx startIndex (line: Token list) =
    let mutable expected = 1
    let mutable i = startIndex
    while expected > 0 do
        match line.[i] with
        | CloseB -> expected <- expected - 1
        | OpenB -> expected <- expected + 1
        | _ -> ()
        i <- i + 1
    i - 1           


let parseLine (line: string) =
    let line' = line.Replace(" ", "")
    line'
    |> Seq.map (fun ch -> 
        match ch with
        | '+' -> Plus
        | '*' -> Mul
        | '(' -> OpenB
        | ')' -> CloseB
        | ch -> Number <| int64 ch - int64 '0' )
    |> List.ofSeq    

let squashAtPos pos newEl (line: Token list) =
    let pre = if pos > 1 then line.[0..pos-2] else []
    let suf = if pos < line.Length - 2 then line.[pos+2..] else [] 
    pre @ newEl :: suf

let isOpenB el = match el with | OpenB -> true | _ -> false

let rec resolveParentheses computeExpression (line: Token list)  =
    match line |> List.tryFindIndex isOpenB with 
        | Some parIdx ->
            let closingIdx = getClosingParIdx (parIdx+1) line
            let subExpr = computeExpression line.[parIdx+1..closingIdx-1]
            let line' = line.[0..parIdx-1] @ subExpr :: line.[closingIdx+1..]
            resolveParentheses computeExpression line'
        | None -> line

type OpDef =
  { Pred: Token -> bool 
    Op: Token -> Token -> Token-> Token }

let rec resolveOperations (operations: OpDef list) (line: Token list) =
    match line |> List.tryFindIndex (fun token -> operations |> List.exists (fun op -> op.Pred token)) with
    | Some opIdx ->
        let op = operations |> List.find (fun o -> o.Pred line.[opIdx])
        let newElem = op.Op line.[opIdx-1] line.[opIdx] line.[opIdx+1]
        let line' = squashAtPos opIdx newElem line
        resolveOperations operations line'
    | None -> line

let isPlus el = match el with | Plus -> true | _ -> false

let add left _ right = 
   match left,right with
    | Number l, Number r -> Number (l + r)
    | _ -> failwithf "expected numbers next to plus: %A %A" left right

let isMul el = match el with | Mul -> true | _ -> false

let mul left _ right = 
   match left,right with
    | Number l, Number r -> Number (l * r)
    | _ -> failwithf "expected numbers next to mul: %A %A" left right

let isPlusMul el = match el with | Plus | Mul -> true | _ -> false    

let addorMul left op right =
    match op with
    | Plus -> add left op right
    | Mul -> mul left op right
    | _ -> failwithf "unknown op token %A" op

let rec computeLineRec1 (line:Token list) =
    let res =
        line 
        |> resolveParentheses computeLineRec1
        |> resolveOperations [{Pred = isPlusMul; Op = addorMul}]
    res.[0]

let rec computeLineRec2 (line:Token list) =
    let res =
        line 
        |> resolveParentheses computeLineRec2
        |> resolveOperations [{Pred = isPlus; Op = add}]
        |> resolveOperations [{Pred = isMul; Op = mul}]
    res.[0]

let input =
    file 
    |> Seq.map parseLine
    |> List.ofSeq

let res1 = 
    input
    |> List.sumBy (fun line -> 
        match line |> computeLineRec1 with
        | Number x -> x
        | _ -> 0L)


let res2 = 
    input
    |> List.sumBy (fun line -> 
        match line |> computeLineRec2 with
        | Number x -> x
        | _ -> 0L)
