namespace MachineLearning
open System
open MachineLearning.MathNeuron.LogicalOperations

module Program =
    let formatParamsAndResult1 p func = 
        sprintf "|%6b       | -> %A" p (func p)
    let formatParamsAndResult2 (a : bool, b : bool) func = 
        sprintf "|%6b|%6b| -> %A" a b (func (a, b))


    [<EntryPoint>]
    let main argv =
        printfn "and: %s" (formatParamsAndResult2 (true, true) ``and``)
        printfn "and: %s" (formatParamsAndResult2 (false, true) ``and``)
        printfn "and: %s" (formatParamsAndResult2 (true, false) ``and``)
        printfn "and: %s" (formatParamsAndResult2 (false, false) ``and``)
       
        printfn "or:  %s" (formatParamsAndResult2 (true, true) ``or``)
        printfn "or:  %s" (formatParamsAndResult2 (false, true) ``or``)
        printfn "or:  %s" (formatParamsAndResult2 (true, false) ``or``)
        printfn "or:  %s" (formatParamsAndResult2 (false, false) ``or``)
        
        printfn "not: %s" (formatParamsAndResult1 false ``not``)
        printfn "not: %s" (formatParamsAndResult1 true ``not``)
        0 // return an integer exit code
