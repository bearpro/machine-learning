namespace MachineLearning

open System
open System.Drawing
open System.IO
open MachineLearning.Learning
open MachineLearning.Network
open MachineLearning.Utils
open MachineLearning.ImageSet
open CommandLine

module Practice3 =
    [<Verb("task3-train")>]
    type Options =
        { [<Option("train-set", Group = "training", MetaValue = "[FILE|DIR]...")>]
          TrainSet: string seq
          [<Option("train-dump", Group = "training", MetaValue = "FILE")>]
          TrainDump: string
          [<Option('d', "dump-trained", MetaValue = "FILE")>]
          DumpTrained: bool
          [<Value(0, MetaName = "test set", MetaValue = "[FILE|DIR]...")>]
          TestSet: string seq
          [<Option('c', "coefficient", Default = 1.0, MetaValue = "FLOAT")>]
          Coefficiend: float}

    let neuron weights =
        let threshold sum =
            if sum >= 1.0 then 1.0 else 0.0
        { Weights = weights
          ThresholdFunction = threshold
          AggregationFunction = Seq.sum
          InvokationMode = Normal }

    /// Возвращает однослойную нейронную сеть, имеющую inputCount нейронов на сенсорном слое,
    /// и imageCount нейронов на выходном слое.
    let network inputCount imageCount : (ConnectionMatrix * Layer) list =
        let sLayer = neuron (1.0 *| 1) *| inputCount
        let aLayer = neuron (0.0 *| inputCount) *| imageCount
        [ OneToOne, sLayer
          Cross, aLayer ]

    let trainingTableOfSet trainigSet =
        let len = Seq.length trainigSet
        trainigSet
        |> Seq.mapi (fun i item -> item.Values, 0.0 *| len |> set i 1.0)

    let getPaths filesOrDicts =
        seq {
            for path in filesOrDicts do
                match Directory.Exists path, File.Exists path with
                | true, _    -> yield! Directory.GetFiles path
                | _   , true -> yield path
                | _   , _    -> raise (FileNotFoundException(null, path))
        }

    let trainingTable options =
        options.TrainSet
        |> getPaths
        |> ImageSet.read
        |> trainingTableOfSet
        |> List.ofSeq

    let testImages options =
        options.TestSet
        |> getPaths
        |> ImageSet.read

    let test trainingSet testSet net =
        for image in testSet do
            let output = net |> Network.invoke image.Values
            let activeIndex = output |> List.findIndex (fun x -> x > 0.5)
            let matchedImage = trainingSet |> Seq.find(fun x -> x.Index = activeIndex)
            printfn "Image: '%s'; Neuron: '%s' (%i)" image.Name matchedImage.Name matchedImage.Index
            printfn "Exact output: %A" output

    let run options =
        let trainSet =
            options.TrainSet
            |> getPaths
            |> ImageSet.read
        let trainTable =
            trainSet
            |> trainingTableOfSet
            |> List.ofSeq
        let net = network (24 * 24) (List.length trainTable)
        DeltaRule.learningCoefficient <- options.Coefficiend
        let smartNet = DeltaRule.studyTillCompleted (neuronsAtLayer 1) trainTable net
        let testSet =
            options.TestSet
            |> getPaths
            |> ImageSet.read
        test trainSet testSet smartNet
        0
