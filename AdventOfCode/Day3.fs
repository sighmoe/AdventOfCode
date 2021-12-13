module Day3

open Util

let getBitsAtPos list pos =
    let rec loop (list: list<string>) pos acc =
        match list with
        | [] -> acc
        | curr :: rest -> loop rest pos (curr.[pos] :: acc)

    loop list pos []

let countOnes list =
    let rec loop list acc =
        match list with
        | [] -> acc
        | '1' :: rest -> loop rest (acc+1)
        | _ :: rest -> loop rest acc

    loop list 0

let countZeroes list =
    let rec loop list acc =
        match list with
        | [] -> acc
        | '0' :: rest -> loop rest (acc+1)
        | _ :: rest -> loop rest acc

    loop list 0


let findMostCommon list pos =
    let bitsAtPos = getBitsAtPos list pos
    // printfn "pos %A" pos
    // printfn "bitsAtPos %A" bitsAtPos
    match ((countOnes bitsAtPos), (countZeroes bitsAtPos)) with
    | (x,y) when x > y -> '1'
    | (x,y) when x < y -> '0'
    | _ -> '2' 


let calculateGammaRate list =
    let rec loop (list: list<string>) idx acc =
        match idx with
        | size when size = String.length (list.Item(0)) -> acc
        | pos -> loop list (pos+1) ((findMostCommon list pos) :: acc)

    List.rev (loop list 0 [])

let fromBinary list =
    let rec loop list idx acc =
        match list with
        | [] -> acc
        | '1' :: rest -> loop rest (idx-1) (acc + 1 * (pown 2 idx))
        | '0' :: rest -> loop rest (idx-1) (acc)

    loop list (List.length list - 1) 0 

let complement list =
    let rec loop list acc =
        match list with
        | [] -> acc
        | '1' :: rest -> loop rest ('0' :: acc)
        | '0' :: rest -> loop rest ('1' :: acc)

    List.rev (loop list [])

let solve1 () =
    let vals =
        readLines ("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day3\\input1.txt") |> Seq.toList 
    let gammaRateList = calculateGammaRate vals
    let epsilonRateList = complement gammaRateList
    let gammaRate = fromBinary(gammaRateList)
    let epsilonRate = fromBinary(epsilonRateList)
    printfn "gammaRate %A" gammaRate
    printfn "epsilonRate %A" epsilonRate

