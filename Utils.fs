namespace MachineLearning
open System

module Utils =
    let enumerate list =
        let rec re list index =
            match list with
            | [] -> []
            | [item] -> [(index, item)]
            | head::tail -> (index, head) :: re tail (index + 1)
        re list 0
