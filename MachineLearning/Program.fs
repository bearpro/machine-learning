namespace MachineLearning

open System
open System.Text
open MachineLearning.MathNeuron.LogicalOperations
open CommandLine

module Program =
    do Console.OutputEncoding <- Encoding.UTF8

    let argparser = Parser.Default

    [<EntryPoint>]
    let main argv =
        let result = argparser.ParseArguments<Practice1.Options,
                                              Practice2.Options,
                                              Practice3.Options> argv
        match result with
        | :? Parsed<obj> as command ->
            match command.Value with
            | :? Practice1.Options as opts -> Practice1.run opts
            | :? Practice2.Options as opts -> Practice2.run opts
            | :? Practice3.Options as opts -> Practice3.run opts
            | _ -> 1
        | _ -> 1
