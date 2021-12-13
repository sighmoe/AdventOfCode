module Day11

open Util

let print2DArray (msg: string) arr =
    printfn "%A" msg
    Array.map (fun innerArr -> printfn "%A" innerArr) arr

let getDimensions arr = (Array.length arr, Array.length arr.[0])

let incrementAdj (arr: int[][] ref) adjList = List.map (fun (r,c) -> ((!arr).[r].[c] <- (!arr).[r].[c]+1)) adjList

let step1 (arr: int[][] ref) =
    for r = 0 to (Array.length !arr)-1 do
        for c = 0 to (Array.length (!arr).[0])-1 do
            (!arr).[r].[c] <- (!arr).[r].[c]+1
    arr

let getAdjacent rows cols r c (arr: int[][]) = 
    let mutable list = []
    for i = -1 to 1 do
        let ci = c+i
        for j = -1 to 1 do
            let rj = r+j
            list <- if not (i = 0 && j = 0) && ci > -1 && ci < cols && rj > -1 && rj < rows then ((rj,ci) :: list) else list
    list

let step2 rows cols (arr: int[][] ref) =
    let mutable acc = ref 0
    let rec dfs rows cols r c (arr: int[][] ref) (visited: list<int*int> ref) (acc: int ref) =
        // printfn "Visited: %A" !visited
        for r = 0 to (rows-1) do
            for c = 0 to (cols-1) do
                if not (List.contains (r,c) !visited) && (!arr).[r].[c] > 9 then
                    // printfn "Visiting (%A,%A)" r c
                    visited := ((r,c) :: !visited)
                    acc := !acc+1
                    let adjacent = getAdjacent rows cols r c !arr
                    // printfn "Adjacent positions to (%A,%A): %A" r c adjacent
                    adjacent |> incrementAdj arr |> ignore
                    // print2DArray "Energy levels post increment" !arr |> ignore
                    List.iter (fun (ra, ca) -> if not (List.contains (ra,ca) !visited) then (dfs rows cols ra ca arr visited acc) else ()) adjacent
                else
                    ()
    let visited = ref []
    dfs rows cols 0 0 arr (visited) acc
    List.iter (fun (r,c) -> (!arr).[r].[c] <- 0) !visited
    acc

let solve1 () =
    let mutable input = ref (read2DIntArray("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day11\\input.txt"))
    let (rows, cols) = getDimensions !input
    print2DArray "Energy levels " !input |> ignore
    let mutable flashes = 0
    let steps = 100
    for i = 1 to steps do
        let flashesThisIteration = (step1 input) |> (step2 rows cols)
        printfn "Iteration %A had %A flashes" i !flashesThisIteration
        flashes <- flashes + !flashesThisIteration 
    printfn "Total flashes after %A steps: %A" steps flashes

let allZeroes (arr: int[][] ref) =
    let mutable b = true
    for r = 0 to (Array.length !arr)-1 do
        for c = 0 to (Array.length (!arr).[0])-1 do
            if (!arr).[r].[c] <> 0 then
                b <- false
    b

let solve2 () =
    let mutable input = ref (read2DIntArray("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day11\\input.txt"))
    let (rows, cols) = getDimensions !input
    print2DArray "Energy levels " !input |> ignore
    let mutable steps = 0 
    let mutable zeroes = false
    while not zeroes do
        steps <- steps + 1
        (step1 input) |> (step2 rows cols)
        zeroes <- allZeroes input
    printfn "All zeroes on step: %A" steps


        


