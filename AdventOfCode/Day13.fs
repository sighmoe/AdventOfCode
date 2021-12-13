module Day13

open Util
open System

let parseInstructions (instructions: list<string>) =
    let folds = List.map (fun (instr: string) -> instr.Split(" ")) instructions |> List.map (fun split -> split.[2]) |> List.map (fun fold -> fold.Split("=")) |> List.map (fun pair -> (pair.[0], Int32.Parse pair.[1]))
    folds

let parseInput input =
    let rec loop input acc =
        match input with
        | "" :: instructions -> (acc, parseInstructions instructions)
        | pair :: rest -> pair.Split(",") |> Array.map Int32.Parse |> (fun pair -> loop rest ((pair.[0],pair.[1]):: acc))
    loop input []

let getGridDim dots =
    let xs = List.map (fun (x,_) -> x) dots
    let ys = List.map (fun (_,y) -> y) dots
    (List.max xs, List.max ys)

let createGrid dots =
    let (cols, rows) = getGridDim dots
    let (grid: char[,]) = Array2D.init (rows+1) (cols+1) (fun _ _ -> '.')
    List.iter (fun (col,row) -> grid.[row, col] <- '#') dots
    grid

let getGridDimFromGrid (grid: char[,]) =
    let (cols,rows) = (Array.length grid.[0, *], Array.length grid.[*, 0])
    (rows,cols)

let applyYFold (grid: char[,]) pos =
    let (rows,cols) = getGridDimFromGrid grid
    let newGrid = Array2D.init pos cols (fun _ _ -> '.')
    for r = 0 to (pos-1) do
        for c = 0 to (cols-1) do
            if grid.[r,c] = '#' then 
                newGrid.[r,c] <- '#'

    for r = (pos+1) to (rows-1) do
        for c = 0 to (cols-1) do
            if grid.[r,c] = '#' then
                let offset = pos-(r-pos)
                newGrid.[offset,c] <- '#'
    // printfn "Grid after applying y fold on pos:%A\n%A" pos newGrid
    newGrid

let applyXFold (grid: char[,]) pos =
    let (rows,cols) = getGridDimFromGrid grid
    let newGrid = Array2D.init rows pos (fun _ _ -> '.')

    for r = 0 to (rows-1) do
        for c = 0 to (pos-1) do
            if grid.[r,c] = '#' then
                newGrid.[r,c] <- grid.[r,c]

    for r = 0 to (rows-1) do
        for c = (pos+1) to (cols-1) do
            if grid.[r,c] = '#' then 
                let offset = pos-(c-pos)
                newGrid.[r,offset] <- '#'
    // printfn "Grid after applying x fold on pos:%A\n%A" pos newGrid
    newGrid

let applyFold grid fold =
    match fold with
    | ("x", pos) -> applyXFold grid pos
    | ("y", pos) -> applyYFold grid pos

let applyFolds grid folds =
    List.fold (fun acc fold -> applyFold acc fold) grid folds

let countDots (grid: char[,]) =
    let mutable acc = 0
    let (rows,cols) = getGridDimFromGrid grid
    for r = 0 to (rows-1) do
        for c = 0 to (cols-1) do
            if grid.[r,c] = '#' then acc <- acc+1
    acc

let printGridAsString grid =
    let (rows,cols) = getGridDimFromGrid grid
    for r = 0 to (rows-1) do
        let row = grid.[r,*]
        printfn "%A" (row |> System.String)
    ()
            
let solve1 () =
    let input = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day13\\input.txt") |> Seq.toList
    let (dots, instructions) = input |> parseInput
    printfn "Parsed input.\nDots: %A\nInstructions:%A" dots instructions
    let grid = createGrid dots
    printfn "Grid:\n%A" grid
    let newGrid = applyFold grid (List.head instructions)
    printfn "New grid:\n%50A" newGrid
    printfn "Dots on new grid:%A" (countDots newGrid)

let solve2 () =
    let input = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day13\\input.txt") |> Seq.toList
    let (dots, instructions) = input |> parseInput
    printfn "Parsed input.\nDots: %A\nInstructions:%A" dots instructions
    let grid = createGrid dots
    printfn "Grid:\n%A" grid
    let newGrid = applyFolds grid instructions
    printfn "New grid:\n%50A" newGrid
    printfn "Dots on new grid:%A" (countDots newGrid)
    printGridAsString newGrid

