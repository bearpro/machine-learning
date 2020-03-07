namespace MachineLearning

module HebbianLearning =

    type LearningTarget =
        { Inputs: float list
          Output: float }

    type LearningTable = LearningTarget list

    let private fixWeights learningTarget neuron: Neuron =
        let newWeight weight input = weight + input * learningTarget.Output
        let newOffset weight = weight - learningTarget.Output
        { neuron with
              Weights =
                  (List.map2 newWeight
                       (neuron.Weights
                        |> List.rev
                        |> List.tail
                        |> List.rev) learningTarget.Inputs)
                  @ [ newOffset (neuron.Weights |> List.last) ] }

    (*
лю
*)

    // let rec study learningTable (neuron: Neuron) =
    //     learningTable
    //     |> List.fold (fun neuron { Inputs = inputs; Output = expected } ->
    //         if (neuron |> MathNeuron.Offset.invoke inputs) = expected then
    //             neuron
    //         else
    //             study learningTable
    //                 (neuron
    //                  |> fixWeights
    //                      { Inputs = inputs
    //                        Output = expected })) neuron

    type CorrectionCheckResult =
        | Error of index: int
        | Correct

    let private iscorrect learningTable neuron =
        learningTable
        |> Seq.ofList
        |> Seq.map (fun { Inputs = inputs; Output = expected } -> MathNeuron.Offset.invoke inputs neuron = expected)
        |> Seq.tryFindIndex ((=) false)
        |> fun o ->
            match o with
            | Some index -> Error(index)
            | None -> Correct



    let rec studyfast learningTable neuron =
        match iscorrect learningTable neuron with
        | Correct -> neuron
        | Error index ->
            neuron
            |> fixWeights learningTable.[index]
            |> studyfast learningTable
