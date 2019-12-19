// Learn more about F# at http://fsharp.org

open System

type Planet = string
type Orbit = string

type ParsedOrbit = {
    parent: Planet
    child: Planet
}

let parseOrbit (orbit: Orbit) = 
    let orbitList = orbit.Split(")")
    {parent= orbitList.[0]; child =orbitList.[1]}

let readInput (path:string)= 
    System.IO.File.ReadLines(path) 
    |> Array.ofSeq 
    |> Array.map parseOrbit


let rec pathFind (startOrbit: ParsedOrbit) (allOrbits: ParsedOrbit []) = 
    // printf "path finding for node: %A %A \n" (startOrbit.parent) (startOrbit.child)
    if startOrbit.parent = "COM" then 
        // printf "reached COM, stopping \n"
        1
    else 
        let orbitChild = 
            allOrbits |> Array.filter (fun subOrbit -> startOrbit.parent = subOrbit.child )
            |> Array.map (fun parent -> pathFind parent allOrbits)

        if orbitChild.Length <> 1 then printf "there was a serious error and the length is not 1"

        1 + orbitChild.[0]


let checkPath startOrbit destOrbit (prevOrbit: List<ParsedOrbit>) = 
    // printfn "checking \n%A\n against \n%A\n with previous \n%A\n" startOrbit destOrbit prevOrbit
    let res = 
        if startOrbit.parent = destOrbit.child && not (List.exists (fun x-> x = destOrbit) prevOrbit) then true
        elif startOrbit.child = destOrbit.parent && not (List.exists (fun x-> x = destOrbit) prevOrbit) then true
        else false
    // printfn "%A" res
    res

let rec pathFindPoints (startOrbit: ParsedOrbit) (destOrbit: ParsedOrbit) (prevOrbit: List<ParsedOrbit>) (allOrbits: ParsedOrbit []) (depth: int) = 
    let d2 = depth + 1
    if startOrbit.child = destOrbit.parent then 
        // printfn "AT destination returning  out with depth %i" depth
        // printfn "START:\n%A\nDEST:\n%A\nPREV:\n%A\n" startOrbit destOrbit prevOrbit
        depth
    else 

        let mutable depth = 0;
        let candidates : ParsedOrbit [] = 
            allOrbits 
            |> Array.filter (fun orbit -> checkPath startOrbit orbit prevOrbit)

        // TODO refactor into for pipes
        for orbit in candidates do
            let previousOrbits = startOrbit::prevOrbit
            let temp = pathFindPoints orbit destOrbit previousOrbits allOrbits d2

            if temp <> 0 then depth <- temp

        depth


let getItem (name: Planet) (orbits: ParsedOrbit []) = 
    let x : ParsedOrbit [] = orbits |> Array.filter (fun orbits -> orbits.child = name)
    let y: ParsedOrbit = x.[0]
    y


let testCases = 
    let orbitData : ParsedOrbit [] = readInput("test2.txt")
    let d : ParsedOrbit= getItem "D" orbitData
    let e : ParsedOrbit= getItem "E" orbitData
    let f : ParsedOrbit= getItem "F" orbitData
    let b : ParsedOrbit= getItem "B" orbitData

    start dest prev
    assert (checkPath e d [d] = false) // dest same as previous
    assert (checkPath e d [f] = true)     // dest diff than previous
    assert (checkPath e b [f] = false)    // dest not reachable
    assert (checkPath e e [e] = false)

[<EntryPoint>]
let main argv =
    let orbitData : ParsedOrbit [] = readInput("input.txt")

    let you : ParsedOrbit= getItem "YOU" orbitData
    let san : ParsedOrbit= getItem "SAN" orbitData
    
    let orbitData: int = pathFindPoints you san [] orbitData 0

    // distance between object you rotate about and SAN rotates about
    let depth = orbitData - 2

    printfn "final orbit depth: %A" depth

    0