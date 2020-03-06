namespace MachineLearning

open System
open MachineLearning.MathNeuron

module Network = 
    type ConnectionMatrix = 
    | Connections of (int * int) Set
    | Cross
    | OneToOne

    type Layer = Neuron list
    let sensorLayer inputs = 
        [for i in [0..inputs] do yield create [1.0] 1.0 ]

    let connect layer1 connectionMatrix layer2 = 
        [(OneToOne, layer1); (connectionMatrix, layer2)]

    let invokeLayer inputs (layer, connections) = 
        match connections with
        | OneToOne -> List.map2 (fun n i -> MathNeuron.invoke [i] n) layer inputs
        | _ -> failwith "Not implemented"

    let rec invoke inputs network = 
        match network with
        | [(connections: ConnectionMatrix, layer: Layer)] -> invokeLayer inputs (layer, connections)
        | (connections: ConnectionMatrix, layer: Layer) :: tail -> tail |> invoke (invoke inputs [(connections, layer)])
        | _ -> failwith "Not implemented"