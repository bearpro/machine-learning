namespace MachineLearning

module HebbianLearning =
    open Utils

    /// <summary>
    /// Представляет отдельную запись в таблице значений обучающего набора.
    /// </summary>
    type LearningTarget =
        { Inputs: float list
          Output: float }

    /// <summary>
    /// Представляет таблицу значений для входов и выхода нейрона, по которой нейрон
    /// будет обучаться.
    /// </summary>
    type LearningTable = LearningTarget list

    /// <summary>
    /// Выполняет модификацию весов нейрона таким образом, чтобы они соответсвовали данной
    /// записи в таблице истинности.
    /// </summary>
    let private fixWeights learningTarget (learnCoeficient, forgetCoeficient) neuron: Neuron =
        let newWeight weight input = weight * (1.0 - forgetCoeficient) + learnCoeficient * input * learningTarget.Output
        let newOffset offset = offset * (1.0 - forgetCoeficient) - learnCoeficient * learningTarget.Output
        { neuron with
              Weights =
                  newOffset neuron.Weights.Head :: (List.map2 newWeight neuron.Weights.Tail learningTarget.Inputs) }

    (*
лю
*)

    /// <summary>
    /// Представляет результат проверки соответствия.
    /// </summary>
    type CorrectionCheckResult =
        | Error of index: int
        | Correct

    /// <summary>
    /// Проверяет соответсвие нейрона и таблицы истины. Возвращает
    /// индекс первой записи, в которой обнаружено неосоответствие.
    /// </summary>
    let private iscorrect learningTable neuron =
        learningTable
        |> Seq.ofList
        |> Seq.map (fun { Inputs = inputs; Output = expected } -> MathNeuron.invoke inputs neuron = expected)
        |> Seq.tryFindIndex ((=) false)
        |> function
        | Some index -> Error(index)
        | None -> Correct

    /// <summary>
    /// Обучает нейрон по модели Хебба. Возвращает обученный нейрон,
    /// с правильно настроенными весами.
    /// </summary>
    let rec studyNeuron learningTable coefficients neuron =
        match iscorrect learningTable neuron with
        | Correct -> neuron
        | Error index ->
            neuron
            |> fixWeights learningTable.[index] coefficients
            |> studyNeuron learningTable coefficients

    let studyLayer (studySet: (int * LearningTable) list) coefficients neurons =
        neurons
        |> Utils.enumerate
        |> List.map (fun (neuronIndex, neuron) ->
            (neuron
             |> (coefficients
                 |> studyNeuron
                     (studySet
                      |> List.filter (fun (tableIndex, table) -> tableIndex = neuronIndex)
                      |> List.exactlyOne
                      |> function
                      | _, x -> x))))
