module Day7

open Util
open System

let fuelToAlign positions pos =
    List.fold (fun acc elem -> acc+(abs (elem-pos))) 0 positions

let fuelCost dist =
    let rec loop acc elem =
        match elem with
        | 0 -> acc
        | x -> loop (acc+x) (x-1)
    loop 0 dist

let fuelToAlign2 positions pos =
    List.fold (fun acc elem -> acc+(fuelCost (abs (elem-pos)))) 0 positions

let solve1 () =
    let input = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day7\\input.txt") |> Seq.head |> fun s -> s.Split(",") |> Array.map Int32.Parse |> Array.toList
    let fuels = List.map (fuelToAlign input) input
    printfn "Fuel: %A" fuels
    printfn "Min Fuel: %A" (List.min fuels)

let solve2 () =
    let input = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day7\\input.txt") |> Seq.head |> fun s -> s.Split(",") |> Array.map Int32.Parse |> Array.toList
    let (min,max) = (List.min input, List.max input)
    let fuels = List.map (fuelToAlign2 input) (seq { min .. max } |> Seq.toList) 
    printfn "Fuel: %A" fuels
    printfn "Min Fuel: %A" (List.min fuels)

