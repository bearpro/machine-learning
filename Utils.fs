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

    /// Возвращает список из n значений value.
    let times value n =
        [for _ in 0..n -> value]

    /// Оператор повторения заданного значения n раз.
    let ( *| ) = times
