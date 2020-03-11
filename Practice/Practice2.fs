namespace MachineLearning

open System

module Practice2 =
    open Network
    open HebbianLearning

    /// <summary>
    /// Содержит описание работы с файлами образов.
    /// </summary>
    module ImageSet =
        open System.IO

        /// <summary>
        /// <para>Возвращает последовательность значений на входе для данной строки.</para>
        /// <para>Например, строка "##.." будет распознана как [1.0; 1.0; -1.0; -1.0].</para>
        /// <para>Символы кроме '#' и '.' игнорируются.</para>
        /// </summary>
        let parse string: float list =
            string
            |> Seq.fold (fun acc item ->
                match item with
                | '#' -> 1.0 :: acc
                | '.' -> -1.0 :: acc
                | _ -> acc) []

        /// <summary>
        /// Возвращает все образы из папки, с их именами и строковыми представлениями.
        /// </summary>
        let loadInputs path =
            if Directory.Exists path then
                Directory.EnumerateFiles path
                |> List.ofSeq
                |> List.map
                    (fun file -> ((Path.GetFileName file), File.ReadAllText file)
                     >> fun (name, content) -> (name, content, parse content))
            else
                failwith "Такой директории не существует"

        /// <summary>
        /// Загружает обучающий набор из папки.
        /// </summary>
        let load path: (int * LearningTable) list =
            let parsedInputs = loadInputs path |> List.map (fun (name, _, pixels) -> (Int32.Parse name, pixels))
            parsedInputs
            |> List.map (fun (number, pixels) ->
                (number,
                 parsedInputs
                 |> List.map (fun (item, pixels) ->
                     { Inputs = pixels
                       Output =
                           if item = number then 1.0 else -1.0 })))

    let bits integer =
        let rec re =
            function
            | 0 -> [ 0 ]
            | 1 -> [ 1 ]
            | integer -> integer % 2 :: re (integer / 2)

        let rec offset bits =
            (List.length
             >> (<=) 4
             >> function
             | true -> bits
             | false -> offset (0 :: bits)) bits

        integer
        |> re
        |> List.rev
        |> offset

    let integersStudySet =
        let allBits =
            [ 0 .. 9 ]
            |> List.map
                (bits
                 >> List.map (function
                     | 0 -> -1.0
                     | 1 -> 1.0
                     | _ -> failwith "Not valid value."))

        let baseArray = System.Collections.Immutable.ImmutableArray.CreateRange([ 0 .. 9 ] |> List.map (fun _ -> -1.0))
        let allInputs = [ 0 .. 9 ] |> List.map ((fun i -> baseArray.SetItem(i, 1.0)) >> List.ofSeq)
        [ 0 .. 3 ]
        |> List.map (fun neuronIndex ->
            (neuronIndex,
             List.map2 (fun inputs (bits: float list) ->
                 { Inputs = inputs
                   Output =
                       match bits.[neuronIndex] with
                       | -1.0 -> -1.0
                       | 1.0 -> 1.0
                       | _ -> failwith "Not valid value." }) allInputs allBits))

    /// <summary>
    /// Конструирует нейронную сеть, обучая второй уровень ("A-элементы") с помощью
    /// обучающего наора 'aLayerStudySet'. Возвращает обученную нейронную сеть.
    /// </summary>
    let network: (ConnectionMatrix * Layer) list =
        let sLayer =
            [ for i in [ 1 .. 15 ] do
                yield MathNeuron.create [ 1.0 ] 1.0 ]

        let aLayer =
            [ for number in [ 0 .. 9 ] ->
                MathNeuron.create [ for _ in [ 0 .. 14 ] -> 0.0 ] 0.0 ]

        let rLayer =
            [ for _ in [ 0 .. 3 ] ->
                MathNeuron.create [ for _ in [ 0 .. 9 ] -> 0.0 ] 0.0 ]

        [ (OneToOne, sLayer)
          (Cross, aLayer)
          (Cross, rLayer) ]

    /// <summary>
    /// Простой пример обучения отдельного нейрона по модели Хебба.
    /// </summary>
    let sample() =
        let table: LearningTable =
            [ { Inputs = [ 1.0; -1.0 ]
                Output = 1.0 }
              { Inputs = [ 1.0; 1.0 ]
                Output = 1.0 }
              { Inputs = [ -1.0; 1.0 ]
                Output = 1.0 }
              { Inputs = [ -1.0; -1.0 ]
                Output = 1.0 } ]

        let neuron = MathNeuron.create [ 0.0; 0.0 ] 0.0
        let smartNeuron = neuron |> HebbianLearning.studyNeuron table (0.5, 0.0)
        ()

    /// <summary>
    /// <para>Представляет результаты обучения нейронной сети.</para>
    /// <para>Для каждого файла в папке 'path' - загружает образ и передаёт на вход
    /// нейронной сети.</para>
    /// <para>Печатает содержимое файла и выходы последнего слоя нейронной сети на экран.</para>
    /// </summary>
    let test path network =
        let set = ImageSet.loadInputs path
        let item =
            function
            | -1.0 -> 0
            | 1.0 -> 1
            | _ -> failwith "Fuck"
        for (name, content, inputs) in set do
            printfn "\nFile: %s\nContent:\n%s" name content
            let result = network |> Network.invoke inputs
            printfn "Result is %d.\nOutputs:\n%A\n"
                (result
                 |> List.rev
                 |> Utils.enumerate
                 |> List.sumBy (fun (i, v) -> int ((float 2 ** float i) * (float (item v)))))
                (result |> List.map item)
            ()
        ()


    /// <summary>
    /// Точка входа в модуль презентации результатов второго практического занятия.
    /// </summary>
    let main() =
        let studySet = ImageSet.load @"./Practice/Practice 2 study set"

        let net =
            network
            |> function
            | [ (w1, n1); (w2, n2); (w3, n3) ] ->
                [ (w1, n1)
                  (w2, n2 |> studyLayer studySet (1.0, 0.0))
                  (w3, n3 |> studyLayer integersStudySet (1.0, 0.0)) ]
            | [ (w1, n1); (w2, n2) ] ->
                [ (w1, n1)
                  (w2, n2 |> studyLayer studySet (1.0, 0.0)) ]
            | _ -> failwith "Незапланированная структура нейронной сети."
        net |> test @"./Practice/Practice 2 study set"
        ()
