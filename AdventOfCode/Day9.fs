module Day9

open Util

let getAdjacent w h c r (arr: int[][]) = 
    let mutable list = []
    for i = -1 to 1 do
        let ci = c+i
        let ri = r+i
        list <- if i <> 0 && ci > -1 && ci < w then (arr.[r].[ci] :: list) else list
        list <- if i <> 0 && ri > -1 && ri < h then (arr.[ri].[c] :: list) else list
    list

let getAdjacentRC w h c r (arr: int[][]) = 
    let curr = arr.[r].[c]
    let mutable list = []
    for i = -1 to 1 do
        let ci = c+i
        let ri = r+i
        list <- if i <> 0 && ci > -1 && ci < w && arr.[r].[ci] <> 9 && arr.[r].[ci] > curr then ((r,ci) :: list) else list
        list <- if i <> 0 && ri > -1 && ri < h && arr.[ri].[c] <> 9 && arr.[ri].[c] > curr then ((ri,c) :: list) else list
    list

let isLowPoint w h c r (arr: int[][]) =
    let curr = arr.[r].[c]
    // printfn "Checking low point. c: %A r: %A curr: %A" c r curr
    let adjacent = getAdjacent w h c r arr
    // printfn "Adjacent: %A" adjacent
    List.min adjacent > curr

let findLowPoints w h arr =
    let mutable lowPoints = []
    for c = 0 to (w-1) do
        for r = 0 to (h-1) do
            if isLowPoint w h c r arr then 
                lowPoints <- (r,c) :: lowPoints
            else
                lowPoints <- lowPoints
    lowPoints

let findBasinSize w h arr lowPoint =
    let mutable queue = [lowPoint]
    let mutable visited = []
    let mutable basinSize = 0

    while not (List.length queue = 0) do
        // printfn "Queue: %A Visited: %A" queue visited
        let (r,c) = List.head queue
        let adjacent = getAdjacentRC w h c r arr
        visited <- (r,c) :: visited
        basinSize <- basinSize+1
        queue <- List.tail queue
        List.map (fun (ra, ca)-> if not (List.contains (ra,ca) visited) && not (List.contains (ra,ca) queue) then queue <- (ra,ca) :: queue else queue <- queue) adjacent

    basinSize
    
let findRiskLevel w h arr =
    let mutable riskLevel = 0
    for c = 0 to (w-1) do
        for r = 0 to (h-1) do
            if isLowPoint w h c r arr then 
                riskLevel <- riskLevel+(arr.[r].[c]+1)
            else
                riskLevel <- riskLevel
    riskLevel

let countPoints w h (arr: int[][]) =
    let mutable acc = 0
    for c = 0 to (w-1) do
        for r = 0 to (h-1) do
            if arr.[r].[c] <> 9 then acc <- acc+1
            else acc <- acc
    acc

let sumBasins basins = List.fold (fun acc elem -> acc+elem) 0 basins

let solve1 () =
    let input = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day9\\sample.txt") |> Seq.map (fun str -> (Seq.map (fun digit -> (int digit - int '0')) str)) |> Seq.map (fun seq -> Seq.toArray seq) |> Seq.toArray
    let (width, height) = (Array.length input.[0], Array.length input)
    printfn "Input: %A \nwith width %A and height %A" input width height
    printfn "Risk level: %A" (findRiskLevel width height input)

let solve2 () =
    let input = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day9\\input.txt") |> Seq.map (fun str -> (Seq.map (fun digit -> (int digit - int '0')) str)) |> Seq.map (fun seq -> Seq.toArray seq) |> Seq.toArray
    let (width, height) = (Array.length input.[0], Array.length input)
    printfn "Input: %A \nwith width %A and height %A" input width height
    let lowPoints = findLowPoints width height input
    printfn "Low points: %A" lowPoints 
    let basinSizes = List.map (findBasinSize width height input) lowPoints
    printfn "Basin sizes: %A" basinSizes
    printfn "Points that are not 9: %A Sum of basins: %A" (countPoints width height input) (sumBasins basinSizes)
    let top3 = List.take 3 (List.sortDescending basinSizes)
    printfn "Top 3 basin sizes: %A" top3 
    printfn "Final answer: %A" (List.fold (fun acc elem -> acc*elem) 1 top3)

