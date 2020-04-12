namespace MachineLearning

open MachineLearning.MathNeuron
open MachineLearning.Network
open MachineLearning.Utils

module Practice2 =
    let neuron weights =
        { Weights = weights
          ThresholdFunction = ((<=) 0.0 >> fun sum -> if sum then 1.0 else 0.0 )
          AggregationFunction = Seq.sum
          InvokationMode = InvokationMode.Normal }

    /// Возвращает однослойную нейронную сеть, имеющую inputCount нейронов на сенсорном слое,
    /// и imageCount нейронов на выходном слое.
    let network inputCount imageCount : (ConnectionMatrix * Layer) list =
        let sLayer = neuron (1.0 *| 1) *| inputCount
        let aLayer = neuron (0.0 *| inputCount) *| imageCount
        [ OneToOne, sLayer
          Cross, aLayer ]
