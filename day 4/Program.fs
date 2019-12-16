// Learn more about F# at http://fsharp.org

open System


let generateNumbers (minval: int, maxval: int) = 
    seq {
        for i in [minval..maxval] do 
            yield i.ToString()
    }


type Matcher = {
    value: string
    occurance: int
}

let toString x = x.ToString()

let checkValue (num: string) =
    let mutable last = num.[0];

    let mutable decreasing = false
    let mutable adjacent = true

        // check for decreasing values
    for i in num.[1..] do 
        if i < last then
            decreasing <- true
        else 
            last <- i
    
    let digits = ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "0"]

    let mutable counts = []


    for dig in digits do 
        let mutable runCount : int = 0;

        for origChar in num do
            
            // if the characters are the same then inc the counter
            if origChar.ToString() = dig then 
                runCount <- runCount + 1
            // the characters are different, save the record for later
            else 
                if not (runCount = 0) then 
                    let counter : Matcher = {value=dig; occurance = runCount}
                    counts <- counter::counts
                runCount <- 0
        
        if not (runCount = 0) then
            let counter : Matcher = {value=dig; occurance = runCount}
            counts <- counter::counts



    let dup_occurances: bool = counts |> List.exists (fun elem -> elem.occurance = 2)

    dup_occurances && decreasing = false



[<EntryPoint>]
let main argv =
    let min = 272091;
    let max = 815432;

    // assert (checkValue("112233") = true)
    // assert (checkValue("123444") = false)
    // assert (checkValue("111122") = true)


    let acceptable = generateNumbers(min,max) |> Seq.filter checkValue |> List.ofSeq


    printfn "%A length: %i" acceptable (acceptable.Length)


    0 // return an integer exit code
