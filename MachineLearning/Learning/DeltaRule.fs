namespace MachineLearning.Learning

open MachineLearning.MathNeuron
open MachineLearning.Utils
open MachineLearning

module DeltaRule =
    let mutable learningCoefficient = 1.0
    let error actual expected : float list =
        List.map2 (-) expected actual

    let newWeight coefficient error oldWeight input : float =
        oldWeight + coefficient * error * input

    /// Возвращает истину, если нейрон полностью обучен.
    let isCompleted (learningTable: LearningTable) neuron =
        learningTable
        |> Seq.tryFind (fun target -> (invoke target.Inputs neuron) <> (target.Output))
        |> Option.isNone

    let isNetCompleted targets network =
        targets
        |> List.tryFind (fun (inputs, expectedOut) ->
            let actualOut = network |> Network.invoke inputs
            actualOut <> expectedOut)
        |> function
        | None   -> true
        | Some _ -> false

    /// Выполняет обучение однослойной нейронной сети
    let epoch input expectedOut getLayer network =
        let actualOut = network |> MachineLearning.Network.invoke input
        let error = error actualOut expectedOut
        let layer = getLayer network
        let iItems = List.map2 (fun iError iNeuron -> iError, iNeuron) error layer
        let newNeurons =
            iItems
            |> List.map (fun (iError, iNeuron) ->
                { iNeuron
                    with
                        Weights =
                            List.map2 (newWeight learningCoefficient iError) iNeuron.Weights input }
            )
        network
        |> withLastf (fun (connections, oldNeurons) -> connections, newNeurons)
        |> List.ofSeq

    let rec studyTillCompleted getLayer targets network =
        if isNetCompleted targets network
        then network
        else
            targets
            |> List.find (
                fun (input, expected) ->
                    let actualOut = Network.invoke input network
                    let e = error actualOut expected
                    e
                    |> Seq.tryFind (fun iError -> iError <> 0.0)
                    |> Option.isSome )
            |> fun (input, output) ->
                network
                |> epoch input output (getLayer)
                |> studyTillCompleted (getLayer) targets
