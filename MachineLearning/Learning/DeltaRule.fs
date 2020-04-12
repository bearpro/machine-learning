namespace MachineLearning.Learning

module DeltaRule =
    let error actual expected : float list =
        List.map2 (-) expected actual

    let newWeight oldWeight coefficient error input : float =
        oldWeight + coefficient * error * input
