// Learn more about F# at http://fsharp.org

open System
open System.IO
// open System.Collections.Generic

let read_input (arg: string)= 
    let data = System.IO.File.ReadAllLines (arg)
    let data = data.[0]
    let mutable data = data.Split(',') |> Array.map int
    data

let set_params (arg :int array, noun: int, verb: int) = 
    arg.[1] <- noun
    arg.[2] <- verb

    arg

let find_opt_code (data: int array) =
    let length = data.Length

    let mutable cont_loop = true
    let mutable i = 0
    while i <  length  && cont_loop do 
        let pos_1 = i+ 1
        let pos_2 = i + 2
        let set_position = i+3

        let operation = data.[i]
        let val_1 = data.[pos_1]
        let val_2 = data.[pos_2]

        let set_pos = data.[set_position]
        // printfn "pos 1 is %i val 1 is %i \npos 2 is %i val 2 is %i set position is %i actual position %i" pos_1 val_1 pos_2 val_2 set_position set_pos

        // add
        if operation = 1 then data.[set_pos] <- data.[val_1] + data.[val_2]
        elif operation =2 then data.[set_pos] <- data.[val_1] * data.[val_2]
        elif operation = 99 then 
            cont_loop <- false
            // printfn "exiting early"

        // printfn "%A" data

        i <- i+4

    data.[0]

[<EntryPoint>]
let main argv =
    // let data = System.IO.File.ReadAllLines ("input.txt")
    // let data = data.[0]
    // let mutable data = data.Split(',') |> Array.map int

    let noun = 12
    let verb = 2

    let  original_data = read_input("old.txt")
    // let mutable mod_data = set_params(original_data, noun, verb)

    let max__ = (original_data.Length)
    let max__ = max__ - 1
    for noun in [0..max__] do
        for verb in [0..max__] do
            let  data = Array.copy original_data


            let mod_data: int array = set_params(data, noun, verb)

            let opt_code = find_opt_code mod_data

            if opt_code = 19690720 then printfn "noun : %i verb %i val %i" noun verb (verb + (100*noun))  

    
    
    
    0 // return an integer exit code
