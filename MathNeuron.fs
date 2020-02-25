/// <summary>
/// Набор ассетов для решения задач машинного обучения.
/// </summary>
module MachineLearning

open System

/// <summary>
/// Описывает простейший математический нейрон.
/// </summary>
type Neuron = { 
    
    /// <summary>
    /// Список весов на входах. Так же определяет количество входов нейрона.
    /// </summary>
    Weights: double list
    
    /// <summary>
    /// Пороговое значение
    /// </summary>
    Threshold: double

    /// <summary>
    /// Функция, которая должна быть применена к значениям на входе.
    /// </summary>
    /// <remarks>
    /// В математическом нейроне эта функция - сумма всех значений. Я решил 
    /// на всякий случай оставить тут возможность использовать производную
    /// функцию.
    /// </remarks>
    AggregationFunction: (double list -> double)
}

/// <summary>
/// Операции над математическим нейроном.
/// </summary>
module MathNeuron = 

    /// <summary>
    /// Создание математического нкйрона с указынными высами и порогом.
    /// </summary>
    let create weights threshold = {
        Weights = weights
        Threshold = threshold
        AggregationFunction = List.sum
    }

    /// <summary>
    /// Подача значений на входы нейрона. Список входов должен содержать
    /// такой число элементов, сколько весов определено у нейрона.
    /// </summary>
    /// <remarks>
    /// У меня есть сомнения по поводу названия "invoke". Я не знаю,
    /// какой термин на самом деле описывает процесс подачи значений
    /// на синапсы.
    /// </remarks>
    let invoke (inputs: double list) neuron = 
        if inputs.Length = neuron.Weights.Length then
            let net = List.map2 (fun a b -> (a * b)) inputs neuron.Weights
                      |> neuron.AggregationFunction
            if net >= neuron.Threshold 
                then 1.0
                else 0.0
        else failwith (sprintf "Neuron has %d inputs, not %d" neuron.Weights.Length inputs.Length)

    /// <summary>
    /// Логические операции, определённые с помощью математических нейронов.
    /// </summary>
    module LogicalOperations =
        
        /// <summary>
        /// Возвращает список натуральных чисел, соответсвующих логическим
        /// значениям (true->1.0, false->0.0).
        /// </summary>
        let private boolToFloatList (source: bool list) = 
            source |> List.map Convert.ToDouble

        let ``and`` (a, b) = 
            (create [1.0 ; 1.0] 2.0)
            |> (invoke (boolToFloatList [a; b;]) )
            |> Convert.ToBoolean

        let ``or`` (a, b) = 
            (create [1.0 ; 1.0] 1.0)
            |> (invoke (boolToFloatList [a; b;]) )
            |> Convert.ToBoolean

        let ``not`` (a: bool) = 
            (create [-1.0] 0.0)
            |> (invoke ([Convert.ToDouble a]) )
            |> Convert.ToBoolean
        
