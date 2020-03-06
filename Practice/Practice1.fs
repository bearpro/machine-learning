namespace MachineLearning
module Practice1 = 
    
    open MachineLearning.MathNeuron.LogicalOperations

    let formatParamsAndResult1 p func = 
        sprintf "|%6b       | -> %A" p (func p)
    let formatParamsAndResult2 (a : bool, b : bool) func = 
        sprintf "|%6b|%6b| -> %A" a b (func (a, b))

    let network (i1, i2) =
        ("and", ``and`` (i1, i2),
         "or",  ``or``  (i1, i2),
         "not", ``not`` i1)

    let main () = 
        printfn "%s" (formatParamsAndResult2 (true, true) network)
        printfn "%s" (formatParamsAndResult2 (true, false) network)
        printfn "%s" (formatParamsAndResult2 (false, true) network)
        printfn "%s" (formatParamsAndResult2 (false, false) network)
        ()