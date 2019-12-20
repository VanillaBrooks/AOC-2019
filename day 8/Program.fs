open System

type Row = {
    startIndex: int
    endIndex: int
}

type Layer = List<Row>
type OriginalLayout = String

type Image = {
    original: OriginalLayout
    layers: List<Layer>
}


type PixelCount = {
    zero: int
    one: int
    two: int
}

type Dimensions = {
    height : int
    width : int
}

let printImageLayers (image: Image)  : int = 
    for layer : Layer in image.layers do
        printfn "starting new layer"
        for slice : Row in layer do 
            for letter in image.original.[slice.startIndex..slice.endIndex] do
                if letter = '0' then printf " "
                else printf "1"
            printf "\n"            
    0

let readInputNoTxt (data: List<string>) (dim: Dimensions)=     
    let mutable image : Image = {original =data.Item(0); layers = []}
    let mutable allRows : Layer = []
    let mutable currentRow : Row = {startIndex = 0; endIndex = dim.width-1}
    allRows <- allRows @ [currentRow]

    let len = image.original.Length
    while currentRow.endIndex + dim.width< len do

        let nextStart = currentRow.endIndex + 1
        currentRow <- {startIndex=nextStart; endIndex = (currentRow.endIndex + dim.width)}

        allRows <- allRows @ [currentRow]

        // when we have reached the height then we add the current set of rows to the layer stack 
        // and we start a new set of rows
        if allRows.Length = dim.height then        
            image <- {original = image.original; layers = image.layers @ [allRows]}
            allRows <- []

    image

let readInput path (dim: Dimensions)= 
    let data : List<string>= System.IO.File.ReadLines(path) |> List.ofSeq
   
    readInputNoTxt data dim

// flattens the rows of data into a single layer
let flattenPixels (data: OriginalLayout) (layer: Layer)  = 
    layer 
    |> List.map (fun row -> data.[row.startIndex .. row.endIndex]) 
    |> List.fold (fun acc row -> acc + row) ""

let rec findPixelType (data: OriginalLayout) (linearIndex: int) = 
    match data.[linearIndex] with
        | '0'-> 0
        | '1' -> 1
        | '2' -> findPixelType data (linearIndex + (25*6))
        | _ -> 
            printfn "should never happen"
            1

let convertToRecord listData= 
    let mutable zero = 0
    let mutable one = 0
    let mutable two = 0

    for (char, count) in listData do
        if char = '0' then zero <- count
        else if char = '1' then one <- count
        else if char = '2' then two <- count
        else printfn "SOME HORRIBLE STUFF HAS HAPPENED"

    {zero= zero; one = one; two= two}

let partOne = 
    let dim = {height = 25; width=6}
    let image: Image = readInput "input.txt" dim

    let minLayer = 
        image.layers 
        // flatten all the layer slices into strings
        |> List.map (fun layer -> flattenPixels image.original layer) 
        // change to array<Array<char>>
        |> Array.ofSeq 
        |> Array.map (fun x -> Array.ofSeq x) 
        // group by each number
        |> Array.map  (Array.groupBy id )
        // sum the counts of each number
        |> Array.map (Array.map (fun (x,y) -> (x, y.Length)))
        // convert the unorganized strings into records
        |> Array.map convertToRecord
        // find the smallest layer with zeros
        |> Array.minBy (fun x-> x.zero)

    minLayer


let partTwo = 
    let dim = {height = 6; width = 25}
    let image: Image = readInput "input.txt" dim

    [|0..(dim.width*dim.height)|] 
    // recursively find what pixel is not transparent
    |> Array.map (fun x -> (findPixelType image.original x))
    // concat all pixels together
    |> Array.fold (fun (acc: string) (x:int) -> acc + x.ToString()) ""
    // format the linear string into the height / width dimensions
    |> fun (x : string) -> readInputNoTxt [x] dim
    // print the data to console
    |> printImageLayers 
    |> ignore

[<EntryPoint>]
let main argv =
    // partOne |> ignore
    partTwo |> ignore
    0 // return an integer exit code
