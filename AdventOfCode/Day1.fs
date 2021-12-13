module Day1

open Util

// Compares each measurement to previous
let countIncreases1 list =
    let rec loop list acc =
        match list with
        | [] -> acc
        | [ _ ] -> acc
        | x :: y :: rest when y > x -> loop (y :: rest) (acc + 1)
        | x :: y :: rest -> loop (y :: rest) acc

    loop list 0

let solve1 () =
    let vals =
        readLines ("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day1\\input1.txt") |> Seq.toList |> List.map int

    printfn "%A" (countIncreases1 vals)

// Compares three-measurement sliding window
let countIncreases2 list =
    let rec loop list prev acc =
        match list with
        | [] -> acc
        | [ _ ] -> acc
        | [_; _] -> acc
        | a :: b :: c :: rest when (a+b+c) > prev -> loop (b :: c :: rest) (a+b+c) (acc+1)
        | a :: b :: c :: rest -> loop (b :: c :: rest) (a+b+c) acc 

    loop list System.Int32.MaxValue 0


let solve2 () =
    let vals =
        readLines ("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day1\\input2.txt") |> Seq.toList |> List.map int

    printfn "%A" (countIncreases2 vals)

