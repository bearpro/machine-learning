/// <summary>
/// Набор ассетов для решения задач машинного обучения.
/// </summary>
namespace MachineLearning

open System

/// <summary>
/// Описывает простейший математический нейрон.
/// </summary>
type Neuron = { 
    /// <summary>
    /// Список весов на входах. Так же определяет количество входов нейрона.
    /// </summary>
    Weights: float list
    
    /// <summary>
    /// Функция, которая должна быть применена к значениям на входе.
    /// </summary>
    /// <remarks>
    /// В математическом нейроне эта функция - сумма всех значений. Я решил 
    /// на всякий случай оставить тут возможность использовать произвольную
    /// функцию.
    /// </remarks>
    AggregationFunction: (float list -> float)

    /// <summary>
    /// Пороговая функция, описывающая результат в зависимости от результата работы AggregationFunction.
    /// </summary>
    ThresholdFunction: (float -> float)
}

/// <summary>
/// Операции над математическим нейроном.
/// </summary>
module MathNeuron = 

    /// <summary>
    /// Создание математического нейрона с указынными весами и порогом.
    /// </summary>
    let create weights threshold = {
        Weights = weights
        ThresholdFunction = ((<=) threshold >> Convert.ToDouble)
        AggregationFunction = List.sum
    }

    /// <summary>
    /// Активация нейрона. Список входов должен содержать такое число
    /// элементов, сколько весов определено у нейрона.
    /// </summary>
    let invoke (inputs: float list) neuron = 
        if inputs.Length = neuron.Weights.Length 
        then
            List.map2 (fun a b -> (a * b)) inputs neuron.Weights
            |> neuron.AggregationFunction
            |> neuron.ThresholdFunction
        else 
            failwith (sprintf "Neuron has %d inputs, not %d" neuron.Weights.Length inputs.Length)

    let createinvoke weights threshold inputs = 
        create weights threshold |> invoke inputs

    /// <summary>
    /// Логические операции, определённые с помощью математических нейронов.
    /// </summary>
    module LogicalOperations =
        
        /// <summary>
        /// Возвращает список натуральных чисел, соответсвующих логическим
        /// значениям (true->1.0, false->0.0).
        /// </summary>
        let private boolToFloatList (source: bool list) = 
            source
            |> List.map Convert.ToDouble

        let ``and`` (a, b) = 
            createinvoke [1.0 ; 1.0] 2.0 (boolToFloatList [a; b;])
            |> Convert.ToBoolean

        let ``or`` (a, b) = 
            createinvoke [1.0 ; 1.0] 1.0 (boolToFloatList [a; b;])
            |> Convert.ToBoolean

        let ``not`` (a: bool) = 
            createinvoke [-1.0] 0.0 ([Convert.ToDouble a])
            |> Convert.ToBoolean
        
