namespace MachineLearning

open System
open System.Drawing
open System.IO
open MachineLearning.MathNeuron
open MachineLearning.Network
open MachineLearning.Utils
open CommandLine

module ImageSet =
    type Item =
        { Name: string
          Index: int
          Values: float list }

    module Item =
        let format item =
            use text = new System.IO.StringWriter()
            text.WriteLine (sprintf "%s [%i]" item.Name item.Index)
            for i in 0..23 do
                for j in 0..23 do
                    text.Write (if item.Values.[i*24+j] > 0.5 then '#' else ' ')
                text.WriteLine()
            text.ToString()

    let read (paths:string seq) =
        seq {
            for index, path in Seq.mapi (fun a b -> a, b) paths do
                use bmp = new System.Drawing.Bitmap(path)
                let pixels =
                    cartesian [0..23] [0..23]
                    |> Seq.map (bmp.GetPixel
                    >> fun color -> color.GetBrightness()
                    >> fun brightness -> if brightness > 0.5f then 1.0 else 0.0 )
                    |> List.ofSeq
                yield { Name = FileInfo(path).Name
                        Index = index
                        Values = pixels }
        }
