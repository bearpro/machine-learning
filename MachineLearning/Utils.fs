namespace MachineLearning

open System
open System.IO

module Utils =
    let enumerate list =
        list
        |> List.mapi (fun index item -> index, item)

    /// Возвращает список из n значений value.
    let times value n =
        [for _ in 1..n -> value]

    /// Оператор повторения заданного значения n раз.
    let ( *| ) = times

    let set index value list =
        list
        |> List.mapi (fun i item -> if i = index then value else item)

    let setBack index value list =
        list
        |> List.rev
        |> List.mapi (fun i item -> if i = index then value else item)
        |> List.rev

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

    /// Возвращает декартово произвдение двух множеств.
    let cartesian a b =
        seq {
            for i in a do
                for j in b do
                    yield j, i
        }

    /// Если элементом последовательности оказывается путь к файлу - возвращает путь к файлу.
    /// Если элементом последовательности оказывается директория - возвращаются пути к файлам в этой директории.
    let getPaths filesOrDicts =
        seq {
            for path in filesOrDicts do
                match Directory.Exists path, File.Exists path with
                | true, _    -> yield! Directory.GetFiles path
                | _   , true -> yield path
                | _   , _    -> raise (FileNotFoundException(null, path))
        }
