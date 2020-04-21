namespace MachineLearning.Tests

open System
open Xunit
open MachineLearning.Practice3
open MachineLearning.Network

module Practice3 =
    [<Fact>]
    let ``Нейронная сеть строится корректно`` () =
        let net11 = network 1 1
        Assert.True(
            match net11 with
            | [ ConnectionMatrix.OneToOne, l1; ConnectionMatrix.Cross, l2 ]
                when List.length l1 = 1 && List.length l2 = 1->
                true
            | _ -> false )
