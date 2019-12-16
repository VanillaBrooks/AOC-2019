// Learn more about F# at http://fsharp.org

open System


let read_input (path:string)= 
    let data = System.IO.File.ReadLines(path) |> Array.ofSeq
    let l: string array = data.[0].Split(",")
    let r: string array= data.[1].Split(",")

    (l,r)

// assertion the first test case passes
let run_case_1 = 
    let data = read_input("case 1.txt")

    false
// assertion that the second test case passes
let run_case_2 = 
    let data = read_input("case 2.txt")
    false
    
type Point2D = {
        x: int
        y: int
        dist: int
    } 

// turn R3 D4 etc into corresponding coordinates 
let generate_coorinates (data: string array) =
    let mutable last_coorinate :Point2D =  {x = 0; y=0; dist=0}
    printfn "generating coordinates"
    let mutable points : Point2D list= [];

    for pair in data do 
        let dir_ = pair.[0]
        let distance = pair.[1..] |> int
        let m1 = distance - 1;
        for i in [0..m1] do 
            if dir_ = 'R' then 
                last_coorinate <- {x = last_coorinate.x + 1; y=last_coorinate.y; dist=last_coorinate.dist + 1}
            elif dir_ = 'L' then
                last_coorinate <- {x = last_coorinate.x - 1; y=last_coorinate.y; dist=last_coorinate.dist + 1}
                // last_coorinate = {x = last_coorinate.x - 1; y=last_coorinate.y}
            elif dir_ = 'U' then
                last_coorinate <- {x = last_coorinate.x; y=last_coorinate.y + 1; dist=last_coorinate.dist + 1}
            elif dir_ = 'D' then
                last_coorinate <- {x = last_coorinate.x; y=last_coorinate.y - 1; dist=last_coorinate.dist + 1};
            
            // printfn "%A" last_coorinate
            points <- last_coorinate :: points
        // append to workflow

    points

// find all the points where the two paths intersect
let all_intersections(left: Point2D list, right: Point2D list) =

    let mutable matches = []
    let mutable distances = []

    let total: uint64 = (left.Length |> uint64) * (right.Length |> uint64)

    printfn "total: %i" total

    let mutable i : uint64 = (0 |> uint64)
    for l in left do 
        for r in right do 
            i <- i+ (1|>uint64)

            if l.x = r.x && l.y = r.y then
                matches <- l::matches;
                let totalDistance = l.dist + r.dist;
                distances <- totalDistance::distances;

            if i % (100000000 |> uint64) = (0|>uint64) then
                printfn "%f" ((i|> double)*100.0 / (total |> double))

    (matches, distances)

let manhattanDistance (point: Point2D) = abs point.x + abs point.y


// main run function
let find_closest_intersection(left: string array, right: string array  ) = 
    printfn "finding intersections"
    let left_coords = generate_coorinates(left)
    let right_coords = generate_coorinates(right)

    let (intersections, distances) = all_intersections(left_coords, right_coords);
    let intersections = intersections  |> List.map manhattanDistance |> List.min
    let distances = distances |> List.min;

    (intersections, distances)

[<EntryPoint>]
let main argv =
    // assert (run_case_1 = true)
    // assert (run_case_2 = false)

    // run actual program
    let (l: string array, r: string array)= read_input("input.txt")
    let (intersection, distances) = find_closest_intersection(l ,r)


    printfn "manhattan distance min: %A distance: %A" intersection distances
    0 // return an integer exit code
