module Day5

open Util
open System

let parseLine (s: string) =
    s.Split([|','; ' '|])

let extractStart (parsedLine: string[]) =
    (Int32.Parse parsedLine.[0], Int32.Parse parsedLine.[1])

let extractStop (parsedLine: string[]) =
    (Int32.Parse parsedLine.[3], Int32.Parse parsedLine.[4])

let findTopLeft starts stops =
    let rec loop starts stops (acc: (Int32 * Int32)) =
        match (starts, stops) with
        | ([], []) -> acc
        | ((x1,y1) :: rest1, (x2,y2) :: rest2) -> loop rest1 rest2 (min (min (fst acc) x1) x2, min (min (snd acc) y1) y2)

    loop starts stops (Int32.MaxValue, Int32.MaxValue)

let findBottomRight starts stops =
    let rec loop starts stops (acc: (int * int)) =
        match (starts, stops) with
        | ([], []) -> acc
        | ((x1,y1) :: rest1, (x2,y2) :: rest2) -> loop rest1 rest2 (max (max (fst acc) x1) x2, max (max (snd acc) y1) y2)

    loop starts stops (Int32.MinValue, Int32.MinValue)

let createGrid bottomRight =
    let width = (fst bottomRight) + 1
    let height = (snd bottomRight) + 1
    Array2D.init width height (fun _ _ -> 0)

let drawLine (grid: int[,]) (line: (int * int) * (int * int)) =
    let (x1,y1) = (fst line) 
    let (x2,y2) = (snd line)


    // draw horizontal line
    if y1 = y2 then
        printfn "Drawing horizontal line from %A to %A" (x1,y1) (x2,y2)
        if x1 < x2 then
            for i = x1 to x2 do
                grid.[y1,i] <- grid.[y1,i]+1

        else
            for i = x1 downto x2 do
                grid.[y1,i] <- grid.[y1,i]+1

    // draw vertical line
    if x1 = x2 then
        printfn "Drawing vertical line from %A to %A" (x1,y1) (x2,y2)
        if y1 < y2 then
            for i = y1 to y2 do
                grid.[i,x1] <- grid.[i,x1]+1

        else
            for i = y1 downto y2 do
                grid.[i,x1] <- grid.[i,x1]+1

    // draw diagonal lines
    if x1 <> x2 && y1 <> y2 then
        printfn "Drawing diagonal line from %A to %A" (x1,y1) (x2,y2)
        let iter1 =
            if x1 < x2 then
                seq { x1 ..  x2 }
            else
                seq {x1 .. -1 .. x2 }
            |> Seq.toList

        let iter2 =
            if y1 < y2 then
                seq { y1 ..  y2 }
            else
                seq {y1 .. -1 .. y2 }
            |> Seq.toList

        for (i,j) in List.zip iter1 iter2 do
            grid.[j,i] <- grid.[j,i]+1

    ()

let countOverlaps (grid: int[,]) (bottomRight: (int * int)) atLeast =
    let mutable count = 0
    let (x, y) = bottomRight
    for i = 0 to x do
        for j = 0 to y do
            if grid.[i,j] >= atLeast then
                count <- count+1
    count
    

let solve1 () =
    let lines = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day5\\input.txt")
    let parsed = lines |> Seq.map parseLine 
    let starts = parsed |> Seq.map extractStart |> Seq.toList
    let ends = parsed |> Seq.map extractStop |> Seq.toList
    printfn "starts: %A\nstops: %A" starts ends
    let topLeft = findTopLeft starts ends
    let bottomRight = findBottomRight starts ends
    printfn "Top left: %A\nBottom right: %A" topLeft bottomRight
    let grid = createGrid bottomRight
    printfn "Grid before drawing:\n%A" grid
    List.zip starts ends |> List.map (drawLine grid) |> ignore
    printfn "Grid after drawing:\n%A" grid
    printfn "Overlaps: %A" (countOverlaps grid bottomRight 2)


