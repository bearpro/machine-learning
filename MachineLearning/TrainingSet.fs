namespace MachineLearning

open System
open System.Drawing
open System.IO
open MachineLearning.Utils

module ImageSet =
    /// Определяет образ (входной вектор) и мета-данные: номер образа и имя образа
    type Image =
        { Name: string
          Index: int
          Values: float list }

    module Image =
        /// Возвращает воспринимаемое человеком представление образа.
        let format item =
            use text = new StringWriter()
            text.WriteLine (sprintf "%s [%i]" item.Name item.Index)
            for i in 0..23 do
                for j in 0..23 do
                    text.Write (if item.Values.[i*24+j] > 0.5 then '#' else ' ')
                text.WriteLine()
            text.ToString()

        /// Возвращает значение элемента входного вектора на основе яркости пикселя.
        let valueOfBrightness brightness =
            if brightness > 0.5f then 1.0 else 0.0

        /// Читает образ из указанного файла.
        let read (path: string) =
            use bmp = new Bitmap(path)
            let values =
                List.ofSeq (cartesian [0..23] [0..23])
                |> List.map ( bmp.GetPixel >> ( fun color -> color.GetBrightness() ) >> valueOfBrightness )
            { Name = FileInfo(path).Name
              Index = 0
              Values = values }

    /// Читает все образы из указанных файлов, возвращет их пронумерованными.
    let read (paths:string seq) =
        paths
        |> Seq.mapi ( fun index path ->
            { Image.read path with Index = index } )
