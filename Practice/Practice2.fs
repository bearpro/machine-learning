namespace MachineLearning

open System

module Practice2 =
    open Network
    open HebbianLearning

    module ImageSet =
        open System.IO

        let parse string: float list =
            string
            |> Seq.fold (fun acc item ->
                match item with
                | '#' -> 1.0 :: acc
                | '.' -> -1.0 :: acc
                | _ -> acc) []

        let loadInputs path =
            if Directory.Exists path then
                Directory.EnumerateFiles path
                |> List.ofSeq
                |> List.map
                    (fun file -> ((Path.GetFileName file), File.ReadAllText file)
                     >> fun (name, content) -> (name, content, parse content))
            else
                failwith "Такой директории не существует"

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

    let network (aLayerStudySet: (int * LearningTable) list): (ConnectionMatrix * Layer) list =
        let ``input-s connection`` = Network.ConnectionMatrix.OneToOne

        let sLayer = Network.sensorLayer 15

        let ``s-a connection`` = Network.ConnectionMatrix.Cross

        let aLayer =
            [ for number in [ 0 .. 9 ] ->
                printfn "Creating %d neuron" number
                MathNeuron.create [ for _ in [ 0 .. 14 ] -> 0.0 ] 0.0
                |> HebbianLearning.study
                    (aLayerStudySet
                     |> List.filter (fun (n, _) -> n = number)
                     |> List.exactlyOne
                     |> function
                     | _, x -> x) (0.9, 0.0) ]

        let ``a-r connection`` = Network.ConnectionMatrix.Cross

        let rLayer =
            [ for _ in [ 0 .. 3 ] ->
                MathNeuron.create [ for _ in [ 0 .. 9 ] -> 0.0 ] 0.0 ]
        [ (``input-s connection``, sLayer)
          (``s-a connection``, aLayer) ]

    let sample () =
        let table: LearningTable =
            [ {Inputs = [ 1.0; -1.0]; Output =  1.0}
              {Inputs = [ 1.0;  1.0]; Output =  1.0}
              {Inputs = [-1.0;  1.0]; Output =  1.0}
              {Inputs = [-1.0; -1.0]; Output =  1.0} ]
        let neuron = MathNeuron.create [0.0; 0.0;] 0.0;
        let smartNeuron = neuron |> HebbianLearning.study table (0.5, 0.0)
        ()

    let test path network=
        let set = ImageSet.loadInputs path
        for (name, content, inputs) in set do
            printfn "\nName: %s\nContent:\n%s" name content
            let result = network |> Network.invoke inputs
            printfn "Result is %d.\nOutputs:\n%A\n" (result |> List.findIndex ((<=) 0.0)) result
        ()

    let main() =
        let studySet = ImageSet.load @"./Practice/Practice 2 study set"
        let net = network studySet
        net |> test @"./Practice/Practice 2 study set"
        ()
