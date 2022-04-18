module Day15

open Util

let getDimensions arr = (Array.length arr, Array.length arr.[0])

let print2DArray (msg: string) arr =
    printfn "%A" msg
    Array.map (fun innerArr -> printfn "%A" innerArr) arr

let sortList (blob: list<(int*int)*int*list<int*int>>) = List.sortBy (fun (_, snd, _)-> snd) blob 

let getAdjacent w h c r (arr: int[][]) = 
    let mutable list = []
    for i = -1 to 1 do
        let ci = c+i
        let ri = r+i
        list <- if i <> 0 && ci > -1 && ci < w then (((r,ci), arr.[r].[ci]) :: list) else list
        list <- if i <> 0 && ri > -1 && ri < h then (((ri,c), arr.[ri].[c]) :: list) else list
    list

let dijkstra graph (start: int*int) (stop: int*int) =
    let (rows,cols) = getDimensions graph
    let rec loop queue visited =
        // printfn "Visited: %A" visited
        match queue with
        | (pos, cost, path) :: _ when pos = stop -> (cost,path)
        | (curr, cost, path) :: tail ->
            let (cr, cc) = (fst curr, snd curr)
            let adjacent = getAdjacent cols rows cc cr graph
            let q = List.fold (fun acc elem -> if not (List.contains (fst elem) visited) then ((fst elem, cost + snd elem, fst elem :: path) :: acc) else acc) List.empty adjacent
            loop (sortList (List.append q tail)) (curr :: visited)
    loop [(start, 0, [])] []

let solve1 () =
    let graph = read2DIntArray("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day15\\input.txt")
    let (rows, cols) = getDimensions graph 
    // print2DArray "Input:" graph 
    let (cost, path) = dijkstra graph (0,0) (rows-1,cols-1)
    printfn "Cost: %A Path: %A" cost path
   

