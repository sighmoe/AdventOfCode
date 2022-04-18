module Day18

open Util
open System

type SnailfishElement =
    | Value of int
    | Tuple of (SnailfishElement*SnailfishElement)

let int2String x = string x
let char2Int c = (int c) - (int '0')
let string2Int s = Int32.Parse(s)

let snailfishNumToString (num: SnailfishElement) =
    match num with
    | SnailfishElement.Value(x) -> int2String x
    | SnailfishElement.Tuple(elem1, elem2) -> "[" + snailfishElementToString(elem1) + "," + snailfishElementToString(elem2) + "]"

let parseSnailfishNum (num: string): SnailfishElement =
    printfn "Parsing SnailfishNum: %A" num 
    let rec parseSnailfishNum (num: string) (buf: string) =
        match num with
        | '[' :: rest -> SnailfishElement.Tuple()
        
let solve1 () =
    let input = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day18\\sample.txt") |> Seq.toList
    List.iter (fun line -> printfn "%A" line) input
    printfn "PRINTING DESERIALIZED STRINGS"
    List.iter (fun num -> printfn "%A" (parseSnailfishNum num |> snailfishNumToString)) input
    


