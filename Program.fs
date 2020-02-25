open System


let formatParamsAndResult1 p func = 
    sprintf "%A -> %A" p (func p)
let formatParamsAndResult2 p1 p2 func = 
    sprintf "%A, %A -> %A" p1 p2 (func p1 p2)

open MachineLearning.MathNeuron.LogicalOperations
[<EntryPoint>]
let main argv =
    printfn "and: %s" (formatParamsAndResult1 (true, true) ``and``)
    printfn "and: %s" (formatParamsAndResult1 (false, true) ``and``)
    printfn "and: %s" (formatParamsAndResult1 (true, false)  ``and``)
    printfn "and: %s" (formatParamsAndResult1 (false, false)  ``and``)
    
    printfn "or: %s" (formatParamsAndResult1 (true, true) ``or``)
    printfn "or: %s" (formatParamsAndResult1 (false, true) ``or``)
    printfn "or: %s" (formatParamsAndResult1 (true, false) ``or``)
    printfn "or: %s" (formatParamsAndResult1 (false, false) ``or``)
    
    printfn "not: %s" (formatParamsAndResult1 false ``not``)
    printfn "not: %s" (formatParamsAndResult1 true ``not``)
    0 // return an integer exit code
