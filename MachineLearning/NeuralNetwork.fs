namespace MachineLearning

open System
open MachineLearning.MathNeuron

module Network =
    type ConnectionMatrix =
    /// <summary>
    /// Произвольная матрица соединений.
    /// </summary>
    | Connections of (int * int) Set
    /// <summary>
    /// Означает, что для каждый выход нейрона слева связан со всеми нейронами
    /// справа. Количество входов у каждого нейрона справа равно количеству
    /// нейронов слева.
    /// </summary>
    | Cross
    /// <summary>
    /// Означает, что для одного выхода нейрона слева существует один вход
    /// нейрона справа.
    /// Соответсвие входов и выходов определяется порядком их определения.
    /// </summary>
    | OneToOne

    type Layer = Neuron list

    /// <summary>
    /// Возбуждает группу нейронов на одному уровне сети, в соответствии с входными значениями и
    /// матрицей соединений.
    /// </summary>
    let invokeLayer inputs (layer : Layer, connections) =
        match connections with
        | OneToOne -> List.map2 (fun n i -> MathNeuron.invoke [i] n) layer inputs
        | Cross -> List.map (fun n -> MathNeuron.invoke inputs n) layer
        | Connections c ->
            c
            |> Set.toList
            |> List.groupBy (fun (inputIndex, neuronIndex) -> neuronIndex )
            |> List.map (
                fun (neuronIndex, inputIndexes) ->
                    (layer.[neuronIndex],
                     [for (_, i) in inputIndexes -> inputs.[i]]
                    )
             >> fun (neuron, inputs) -> neuron |> MathNeuron.invoke inputs)

    /// <summary>
    /// Выполняет передачу значений на вход верхнему уровню сети, и возвращает результат
    /// с последнего уровня.
    /// </summary>
    let rec invoke inputs network =
        match network with
        | [(connections: ConnectionMatrix, layer: Layer)] ->
            invokeLayer inputs (layer, connections)
        | (connections: ConnectionMatrix, layer: Layer) :: tail ->
            tail |> invoke (invoke inputs [(connections, layer)])
        | [] -> failwith "Невозможно выполнять вычисления в пустой нейронной сети."
