module Day6

open Util
open System

let simulateDay input =
    let newFish = input |> List.fold (fun acc elem -> if elem = 0 then 8 :: acc else acc) []
    input |> List.map (fun x -> if x = 0 then 6 else x-1) |> (fun i -> i @ newFish)

let simulateDay2 (state: int64[] ref) =
    let temp = (!state).[0]
    for i = 1 to 8 do
        (!state).[i-1] <- (!state).[i]
    (!state).[8] <- temp
    (!state).[6] <- (!state).[6]+temp

let getTotalFish (arr: int64[]) =
    Array.fold (fun acc elem -> acc+elem) 0L arr

let solve1 () =
    let input = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day6\\input.txt") |> Seq.head |> fun s -> s.Split(",") |> Array.map Int32.Parse |> Array.toList
    //printfn "Initial state %A" input 
    let mutable state = input
    for i = 1 to 80 do
        state <- (simulateDay state)
        // printfn "After %A day: %A" i state 
    printfn "Total fish: %A" (List.length state)

let solve2 () =
    let input = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day6\\input.txt") |> Seq.head |> fun s -> s.Split(",") |> Array.map Int32.Parse |> Array.toList
    let state: int64[] ref = ref (Array.init 9 (fun _ -> 0L))
    input |> List.map (fun i -> (!state).[i] <- (!state).[i]+1L) |> ignore
    printfn "Initial state %A" !state 
    for i = 1 to 256 do
        (simulateDay2 state)
        printfn "After %A day: %A Total fish: %A" i !state (getTotalFish !state)
    printfn "Total fish: %A" (getTotalFish !state)