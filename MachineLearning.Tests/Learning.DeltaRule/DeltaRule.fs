namespace MachineLearning.Tests.Learning

open System
open System.Collections.Generic
open Xunit
open MachineLearning.Learning.DeltaRule
open MachineLearning
open MachineLearning.Learning

module DeltaRule =

    [<Fact>]
    let ``Вектор ошибки вычисляется корректно`` () =
        let expectedOutput = [  0.0; 1.0; 0.0 ]
        let actualOutput =   [  1.0; 1.0; 0.0 ]
        let expectedError =  [ -1.0; 0.0; 0.0 ]
        let actualError = error actualOutput expectedOutput
        Assert.Equal<float>(expectedError, actualError)

    [<Fact>]
    let ``Проверка завершения обучения работает корректно`` () =
        let learningTable = [ { Inputs = [ 0.0; 0.0; 1.0 ]; Output = 1.0 }
                              { Inputs = [ 0.0; 1.0; 0.0 ]; Output = 0.0 }
                              { Inputs = [ 1.0; 0.0; 0.0 ]; Output = 0.0 } ]
        let falseNeuron = MachineLearning.Practice3.neuron [ 0.0; 0.0; 0.0; ]
        let trueNeuron =  MachineLearning.Practice3.neuron [ 0.0; 0.0; 1.0; ]
        Assert.False(isCompleted learningTable falseNeuron)
        Assert.True(isCompleted learningTable trueNeuron)

    [<Fact>]
    let ``Обучение перцептрона выполняется`` () =
        let learningTable = [ [ 0.0; 0.0; 1.0 ], [ 1.0; 0.0]
                              [ 0.0; 1.0; 0.0 ], [ 0.0; 0.0]
                              [ 1.0; 0.0; 0.0 ], [ 1.0; 0.0]
                              [ 1.0; 1.0; 1.0 ], [ 1.0; 1.0] ]
        let net = Practice3.network 3 2
        DeltaRule.learningCoefficient <- (1.0 / 16.0)
        let studiedNet = studyTillCompleted (Network.neuronsAtLayer 1) learningTable net
        for input, output in learningTable do
            Assert.Equal<float> ( (studiedNet |> Network.invoke input ), output )
