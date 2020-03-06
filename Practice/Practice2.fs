namespace MachineLearning

module Practice2 = 
    open Network

    let main () =
        let s1 = sensorLayer 4
        let s2 = sensorLayer 4
        let net = connect s1 OneToOne s2
        let r = invoke [0.0; 1.0; 0.0; 1.0] net
        printf "%A" r
        ()