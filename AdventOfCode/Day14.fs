module Day14

open Util

let parseRules rules = 
    List.fold (fun acc (rule: string) -> rule.Split(" -> ") |> (fun split -> Map.add split.[0] split.[1] acc)) Map.empty rules

let parseInput (input: list<string>) = 
    let start = List.head input
    let rules = List.tail (List.tail input)
    (start, parseRules rules)

let getPairs (s: string) =
    s |> Seq.pairwise |> Seq.fold (fun acc (curr, pred) -> (string(curr), string(pred)) :: acc) List.empty |> List.rev

let applyRule (rules: Map<string, string>) (pair: string*string) =
    let key = fst pair + snd pair
    let middle = Map.find key rules
    (middle + snd pair)

let doStep (rules: Map<string,string>) (state: string) =
    let pairs = getPairs state
    let newState = List.fold (fun acc elem -> acc + (applyRule rules elem)) (state.Substring(0, 1)) pairs
    printfn "Pairs: %A\nNumber of pairs: %A" (getPairs newState) (List.length (getPairs newState))
    newState

let count x xs = xs |> Seq.filter (fun c -> c = x) |> Seq.length |> int64

let getFrequencies (s: string) =
    let distinct = Seq.distinct s
    let frequencies = Seq.fold (fun acc elem -> (elem, count elem s) :: acc) [] distinct
    frequencies

let getMostAndLeast frequencies = 
    let most = List.maxBy (fun pair -> snd pair) frequencies
    let least = List.minBy (fun pair -> snd pair) frequencies
    (least, most)

let solve1 () =
    let (start, rules) = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day14\\input.txt") |> Seq.toList |> parseInput
    printfn "Start: %A Rules: %A" start rules
    let steps = seq { 1 .. 10 }
    let result = Seq.fold (fun state _ -> doStep rules state) start steps
    let frequencies = getFrequencies result
    printfn "Frequencies: %A" frequencies 
    let (least,most) = getMostAndLeast frequencies
    printfn "Answer: %A" (snd most - snd least)

let createPairFreqMap state =
    let pairs = getPairs state
    let map = 
        List.fold (fun acc elem -> 
            let key = string(fst elem) + string(snd elem) 
            Map.change key (fun curr -> 
                match curr with
                | Some(i) -> Some(i+1L)
                | None -> Some(1L)
            ) acc
        ) Map.empty pairs
    printfn "Pair Map: %A" map
    map

let createCharFreqMap s =
    let map = 
        Seq.fold (fun acc elem -> 
            Map.change (string(elem)) (fun curr -> 
                match curr with
                | Some(i) -> Some(i+1L)
                | None -> Some(1L)
            ) acc
        ) Map.empty s
    printfn "Char Map: %A" map
    map

let updateFreqMap value curr =
    match curr with
    | Some(i) -> Some(i+value)
    | None -> Some(value)

let deleteFromFreqMap curr =
    match curr with
    | _ -> None

let getCountPairFreqMap pairFreqMap =
    Map.fold (fun acc _ value -> acc+value) 0L pairFreqMap

let doStep2 (rules: Map<string,string>) (startPairFreqMap: Map<string, int64>, startCharFreqMap: Map<string, int64>) =
    let maps = (Map.fold (fun acc key value -> 
        let (pairFreqMap, charFreqMap) = acc
        let middle = Map.find key rules
        printfn "Mini step for key: %A with middle: %A" key middle
        let (key1, key2) = (string(key.[0]) + middle, middle + string(key.[1]))
        let finalPairFreq = Map.change key deleteFromFreqMap pairFreqMap |> Map.change key1 (updateFreqMap value) |> Map.change key2 (updateFreqMap value) 
        let finalCharFreq = Map.change middle (updateFreqMap value) charFreqMap
        (finalPairFreq,finalCharFreq)
    ) (startPairFreqMap, startCharFreqMap) startPairFreqMap)
    printfn "Final map after step: %200A." maps
    printfn "Number of pairs: %A" (getCountPairFreqMap (fst maps))
    maps

let solve2 () =
    let (start, rules) = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day14\\sample.txt") |> Seq.toList |> parseInput
    printfn "Start: %A Rules: %A" start rules
    let steps = seq { 1 .. 3 }
    let startPairFreqMap = createPairFreqMap start
    let startCharFreqMap = createCharFreqMap start
    let (pairFreqMap, charFreqMap) = Seq.fold (fun state _ -> doStep2 rules state) (startPairFreqMap,startCharFreqMap) steps
    // printfn "PairFreqMap: %A" pairFreqMap 
    printfn "CharFreqMap: %A Length: %A" charFreqMap (Map.fold (fun acc _ value -> acc+value) 0L charFreqMap)

    // part 1 for comparison
    let result = Seq.fold (fun state _ -> doStep rules state) start steps
    let frequencies = getFrequencies result
    printfn "Frequencies: %A Length: %A" frequencies (List.fold (fun acc elem -> acc + (snd elem)) 0L frequencies)




