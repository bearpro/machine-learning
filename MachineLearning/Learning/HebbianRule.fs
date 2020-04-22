namespace MachineLearning.Learning

open MachineLearning
open MachineLearning.MathNeuron

module HebbianRule =

    /// <summary>
    /// Выполняет модификацию весов нейрона таким образом, чтобы они соответсвовали данной
    /// записи в таблице истинности.
    /// l - коэффициент обучения, f - коэффициент забывания.
    /// </summary>
    let private fixWeights { Inputs = inputs; Output = o } (l, f) neuron: Neuron =
        let correct i w = w * (1.0 - f) + l * i * o
        { neuron with Weights = List.map2 correct (-1.0 :: inputs) neuron.Weights  }

(*
лю
*)

    /// <summary>
    /// Проверяет соответсвие нейрона и таблицы истины. Возвращает
    /// индекс первой записи, в которой обнаружено неосоответствие.
    /// </summary>
    let private iscorrect learningTable neuron =
        learningTable
        |> Seq.ofList
        |> Seq.tryFind (fun { Inputs = i; Output = o } -> MathNeuron.invoke i neuron <> o)
        |> function
        | Some target -> Error target
        | None -> Correct

    /// <summary>
    /// Обучает нейрон по модели Хебба. Возвращает обученный нейрон,
    /// с правильно настроенными весами.
    /// </summary>
    let rec studyNeuron learningTable coefficients neuron =
        match iscorrect learningTable neuron with
        | Correct -> neuron
        | Error target ->
            neuron
            |> fixWeights target coefficients
            |> studyNeuron learningTable coefficients

    let studyLayer (studySet: LearningTable list) coefficients neurons =
        List.map2 (fun neuron table -> studyNeuron table coefficients neuron) neurons studySet
