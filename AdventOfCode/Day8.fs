module Day8 

open Util
open System

let parseSignalPatterns (list: list<string>) =
    let rec loop list acc =
        match list with
        | "|" :: rest -> (acc, rest)
        | curr :: rest -> loop rest (curr :: acc)
    loop list []

let rec charInString c s =
    match s with
    | [] -> 0
    | curr :: rest when curr = c -> 1
    | curr :: rest -> charInString c rest

let commonCharacterCount s1 s2 = 
    let rec loop s1 s2 acc =
        match s1 with
        | [] -> acc
        | curr :: rest -> loop rest s2 (acc+(charInString curr s2))
    loop (s1 |> Seq.sort |> Seq.toList) (s2 |> Seq.sort |> Seq.toList) 0

let sortString (s: string) = s |> Seq.sort |> String.Concat

let decodePartial s =
    match String.length s with
    | 2 -> 1
    | 4 -> 4
    | 3 -> 7
    | 7 -> 8
    | _ -> 0

let decodeLen5 s map = 
    match (commonCharacterCount s (Map.find 1 map)) with
    | 2 -> 3
    | _ -> match (commonCharacterCount s (Map.find 4 map)) with
           | 2 -> 2
           | _ -> 5

let decodeLen6 s map =
    match (commonCharacterCount s (Map.find 1 map)) with 
    | 1 -> 6
    | _ -> match (commonCharacterCount s (Map.find 4 map)) with
           | 4 -> 9
           | _ -> 0

let decodePattern s map =
    match String.length s with
    | 2 -> 1
    | 4 -> 4
    | 3 -> 7
    | 7 -> 8
    | 5 -> decodeLen5 s map 
    | 6 -> decodeLen6 s map

let countUnique (map: Map<string, int>) toDecode =
    match (Map.find toDecode map) with
    | 1 | 4 | 7 | 8 -> 1
    | _ -> 0

let processLine1 (line: string) =
    let (signals, toDecode) =  line |> fun s -> s.Split(" ") |> Array.toList |> parseSignalPatterns
    let decodeMap = Map(List.map (fun x -> (x, decodePartial x)) toDecode)
    List.fold (fun acc elem -> acc+(countUnique decodeMap elem)) 0 toDecode

let processLine2 (line: string) = 
    let (signals, toDecode) =  line |> fun s -> s.Split(" ") |> Array.toList |> parseSignalPatterns
    let partialMapRev = Map(List.map (fun x -> (decodePartial x, x)) signals)
    let fullMap = Map(List.map (fun x -> ((sortString x), decodePattern x partialMapRev)) signals)
    List.map (fun elem -> Map.find (sortString elem) fullMap) toDecode

let convertToNum list =
    let rec loop acc list =
        match list with
        | curr :: [] -> (curr+acc)
        | curr :: rest ->  curr + 10*(loop acc rest)
    loop 0 (List.rev list)

let solve1 () =
    let input = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day8\\input.txt") |> Seq.toList
    let unique = List.fold (fun acc elem -> acc+(processLine1 elem)) 0 input
    printfn "Unique count: %A" unique

let solve2 () =
    let input = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day8\\input.txt") |> Seq.toList
    let decoded = List.map processLine2 input
    printfn "Input: %A\n Decoded: %A" input decoded 
    let ans = List.fold (fun acc elem -> acc+(convertToNum elem)) 0 decoded
    printfn "Final answer: %A" ans

