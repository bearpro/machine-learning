namespace MachineLearning.Learning

open MachineLearning.MathNeuron
open MachineLearning.Utils
open MachineLearning

module DeltaRule =
    let mutable learningCoefficient = 1.0

    /// Возвращает вектор ошибки
    let error actual expected : float list =
        List.map2 (-) expected actual

    /// Возвращает новый вес на основе старого веса, входного значения для данной связи и ошибки данного нейрона.
    let newWeight coefficient error oldWeight input : float =
        oldWeight + coefficient * error * input

    /// Возвращает истину, если нейрон полностью обучен.
    let isCompleted (learningTable: LearningTable) neuron =
        learningTable
        |> Seq.tryFind (fun target -> (invoke target.Inputs neuron) <> (target.Output))
        |> Option.isNone

    /// Возвращает истину, если нейронная сеть правильно обрабаотывает все значения в обучающей таблице.
    let isNetCompleted targets network =
        targets
        |> List.tryFind (fun (inputs, expectedOut) ->
            let actualOut = network |> Network.invoke inputs
            actualOut <> expectedOut)
        |> Option.isNone

    /// Выполняет обучение указанного слоя нейронной сети.
    let epoch input expectedOut getLayer network =
        let actualOut = Network.invoke input network
        let error = error actualOut expectedOut
        let layer = getLayer network
        let iItems = List.map2 (fun iError iNeuron -> iError, iNeuron) error layer
        let neurons =
            iItems
            |> List.map (fun (iError, iNeuron) ->
                let weights = List.map2 (newWeight learningCoefficient iError) iNeuron.Weights input
                { iNeuron with Weights = weights} )
        network
        |> withLastf (fun (connections, _) -> connections, neurons)
        |> List.ofSeq

    /// Выполняет обучение до тех пор, пока сеть не будет соответсвовать обучающей таблице.
    let rec studyTillCompleted getLayer targets network =
        if isNetCompleted targets network
        then network
        else
            targets
            |> List.find (
                fun (input, expectedOut) ->
                    let actualOut = Network.invoke input network
                    error actualOut expectedOut
                    |> Seq.tryFind (fun iError -> iError <> 0.0)
                    |> Option.isSome )
            |> fun (input, output) ->
                network
                |> epoch input output (getLayer)
                |> studyTillCompleted (getLayer) targets
