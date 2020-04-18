namespace MachineLearning.Tests

open System
open System.Collections.Generic
open Xunit
open MachineLearning.Utils

module Utils =
    [<Fact>]
    let ``Замена последнего элемента последовательности корректна`` () =
        let input = [1; 2; 3]
        let modufied = withLast 4 input
        Assert.Equal<int>(modufied, [1; 2; 4])
