namespace MachineLearning

module HebbianLearning =

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
    /// Возвращает список без последнего элемента.
    /// </summary>
    let private skiplast source =
        let array = source |> Array.ofList
        let length = array.Length
        array
        |> Array.take (length - 1)
        |> List.ofArray


    /// <summary>
    /// Выполняет модификацию весов нейрона таким образом, чтобы они соответсвовали данной
    /// записи в таблице истинности.
    /// </summary>
    let private fixWeights learningTarget neuron: Neuron =
        let newWeight weight input = weight + input * learningTarget.Output
        let newOffset weight = weight - learningTarget.Output
        { neuron with
              Weights =
                  (List.map2 newWeight (skiplast neuron.Weights) learningTarget.Inputs)
                  @ [ newOffset (neuron.Weights |> List.last) ] }

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
        |> Seq.map (fun { Inputs = inputs; Output = expected } -> MathNeuron.Offset.invoke inputs neuron = expected)
        |> Seq.tryFindIndex ((=) false)
        |> function
        | Some index -> Error(index)
        | None -> Correct


    /// <summary>
    /// Обучает нейрон по модели Хебба. Возвращает обученный нейрон,
    /// с правильно настроенными весами.
    /// </summary>
    let rec study learningTable neuron =
        match iscorrect learningTable neuron with
        | Correct -> neuron
        | Error index ->
            neuron
            |> fixWeights learningTable.[index]
            |> study learningTable
