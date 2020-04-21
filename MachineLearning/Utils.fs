namespace MachineLearning
open System

module Utils =
    let enumerate list =
        list
        |> List.mapi (fun index item -> index, item)

    /// Возвращает список из n значений value.
    let times value n =
        [for _ in 1..n -> value]

    /// Оператор повторения заданного значения n раз.
    let ( *| ) = times

    /// Возвращает последовательность той же длины, с последним элементом, равным указанному value.
    let withLast value sequence =
        sequence
        |> Seq.rev
        |> Seq.tail
        |> fun sequence -> seq {
            yield value
            yield! sequence
        }
        |> Seq.rev

    /// Возвращает последовательность той же длины, применя к последнему элементу указанную функцию.
    let withLastf func sequence =
        sequence
        |> withLast ( (Seq.last >> func) sequence )
