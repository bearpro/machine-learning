namespace MachineLearning.Tests.Learning

open System
open System.Collections.Generic
open Xunit
open MachineLearning.Learning.DeltaRule

module DeltaRule =
    [<Fact>]
    let ``Error vector is right.`` () =
        let expectedOutput = [  0.0; 1.0; 0.0 ]
        let actualOutput =   [  1.0; 1.0; 0.0 ]
        let expectedError =  [ -1.0; 0.0; 0.0 ]
        let actualError = error actualOutput expectedOutput
        Assert.Equal<float>(expectedError, actualError)
