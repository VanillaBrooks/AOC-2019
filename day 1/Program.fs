// Learn more about F# at http://fsharp.org


open System


open System.IO

open Core.Operators


[<EntryPoint>]
let main argv =
    let readLines (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine () |> int
    }

    let text_data: List<int> = readLines("input.txt") |> Seq.toList

    let rec do_math num:int= 
        let div = (num |> float) / 3.
        let floor = Core.Operators.floor(div) |> int
        let sub  = floor - 2
        if  sub < 0 then 0 else sub + do_math(sub)

    
    // printfn "%i" (do_math(14))
    // let map_ = List.map (do_math) text_data |> List.sum

    // let sum = List.map(do_math, text_data)
    let sum = text_data |> List.map do_math |> List.sum

    printfn "%i" sum

    0 // return an integer exit code

