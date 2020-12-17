module Utils
    open System.Text.RegularExpressions

    // http://www.fssnip.net/29/title/Regular-expression-active-pattern
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some (List.tail [ for g in m.Groups -> g.Value ])
        else None

    let sumOfTwo target (inputs: bigint list) = 
        seq { 
            for i in [0..inputs.Length-2] do
                for j in [i+1..inputs.Length-1] do
                    if inputs.[i] + inputs.[j] = target then yield (inputs.[i], inputs.[j]) }

    let getContigousSublistWithSum target min (input: int64 list) =
      [ let mutable sum = int64 0
        for i in [min..input.Length-1] do
            sum <- sum + input.[i]
            if sum = target then yield input.[min..i] ]

    let getAllSublistsWithSum target (input: int64 list) =
        [for i in 0..input.Length-1 do
            let res = getContigousSublistWithSum target i input
            if res <> [] then yield res |> List.collect id]

    let splitBy element lst =
        let folder  line acc =
            let current, result = acc
            if line = element then ([], current :: result)
            else (line :: current, result)
        let (current, partResult) =
            List.foldBack folder lst ([],[])
        current :: partResult

    let rec cartesian lstlst =
        match lstlst with
        | [h] ->
            List.fold (fun acc elem -> [elem]::acc) [] h
        | h::t ->
            List.fold (fun cacc celem ->
                (List.fold (fun acc elem -> (elem::celem)::acc) [] h) @ cacc
                ) [] (cartesian t)
        | _ -> []    

