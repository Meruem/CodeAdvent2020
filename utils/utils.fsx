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

