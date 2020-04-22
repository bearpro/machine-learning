namespace MachineLearning

open System
open MachineLearning.Learning
open MachineLearning.Network
open MachineLearning.Utils
open CommandLine

module Practice3 =
    /// Определяет параметры командной строки для практической работы №3.
    [<Verb("task3", HelpText = "Практическое задание №3 - Обучение по дельта-правилу.\nРаспознавание образов символов алфавита из 8-битных изображений 24х24px.")>]
    type Options =
        {
            [<Option("train-set",
                Required = true,
                MetaValue = "[FILE|DIR]...",
                HelpText = "Пути к файлам и директориям, в которых находятся файлы обучающего набора.\nЕсли указана директория - используются все файлы в ней.\nФайлы распознаются с помощью System.Drawing.Bitmap, тестирование выполнялось только на BMP. Разрешение всех изображений должно быть 24х24 px.")>]
            TrainSet: string seq

            [<Option("display-neurons",
                Default = false,
                HelpText = "Включить отображение значений выходных сигналов с нейронов ассоциативного слоя.")>]
            DisplayNeuronOut: bool

            [<Option('c', "coefficient",
                Default = 1.0,
                MetaValue = "FLOAT",
                HelpText = "Коэффициент обучения для дельта-правила.")>]
            Coefficiend: float

            [<Value(0,
                MetaName = "test set",
                MetaValue = "[FILE|DIR]...",
                HelpText = "Пути к файлам и директориям, в которых находятся файлы тестового набора.\nЕсли указана директория - используются все файлы в ней.\nФайлы распознаются с помощью System.Drawing.Bitmap, тестирование выполнялось только на BMP. Разрешение всех изображений должно быть 24х24 px.")>]
            TestSet: string seq
        }

    /// Возвращает нейрон с указанным набором весов, сконфигурированный в соответствии с используемым алгоритмом.
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

    /// Возвращает обучающую таблицу на основе последовательности образов.
    let trainingTable (images: ImageSet.Image seq) =
        let zeroVector = 0.0 *| Seq.length images
        images
        |> Seq.mapi (fun i image -> image.Values, set i 1.0 zeroVector)
        |> List.ofSeq

    /// Печатает выходной вектор на консоль, так же помечает соответсвие элементов вектора образам.
    let printNeuronOut trainingSet output =
        let printValue out ({ Name = name }: ImageSet.Image) =
            printf "%s: %.2f; " name out
        printf "Output: "
        ignore <| Seq.map2
            printValue output trainingSet
        printf "%s" Environment.NewLine

    /// Проверяет результаты обучения нейронной сети на наборе образов.
    let test options (trainingSet: ImageSet.Image seq) (testSet: ImageSet.Image seq) network =
        for image in testSet do
            let output = network |> Network.invoke image.Values
            let maxIndex = output |> List.mapi (fun i v -> i, v) |> List.maxBy (fun (i, v) -> v) |> fun (i, _) -> i
            let matchedImage = trainingSet |> Seq.find(fun x -> x.Index = maxIndex)
            printfn "Image: '%s'; Neuron: '%s' (%i)" image.Name matchedImage.Name matchedImage.Index
            if options.DisplayNeuronOut then
                printNeuronOut trainingSet output

    let run options =
        let trainSet = (getPaths >> ImageSet.read) options.TrainSet
        let testSet =  (getPaths >> ImageSet.read) options.TestSet
        let trainTable = trainingTable trainSet
        do DeltaRule.learningCoefficient <- options.Coefficiend
        let net =
            network (24 * 24) (List.length trainTable)
            |> DeltaRule.studyTillCompleted (neuronsAtLayer 1) trainTable
        test options trainSet testSet net
        0
