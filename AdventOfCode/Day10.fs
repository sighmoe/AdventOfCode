module Day10

open Util

type State =
    | Valid 
    | Incomplete 
    | Corrupted 

type ErrorInfo =
    | C of char
    | L of list<char>

let checkLine line = 
    let rec loop line acc =
        match line with
        | [] when (acc <> []) -> (State.Incomplete, ErrorInfo.L acc)
        | [] -> (State.Valid, ErrorInfo.C ' ')
        | '(' :: rest -> loop rest ('(' :: acc)
        | '[' :: rest -> loop rest ('[' :: acc)
        | '{' :: rest -> loop rest ('{' :: acc)
        | '<' :: rest -> loop rest ('<' :: acc)
        | ')' :: _ when (acc = [] || List.head acc <> '(') -> (State.Corrupted, ErrorInfo.C ')')
        | ']' :: _ when (acc = [] || List.head acc <> '[') -> (State.Corrupted, ErrorInfo.C ']')
        | '}' :: _ when (acc = [] || List.head acc <> '{') -> (State.Corrupted, ErrorInfo.C '}')
        | '>' :: _ when (acc = [] || List.head acc <> '<') -> (State.Corrupted, ErrorInfo.C '>')
        | _ :: rest -> loop rest (List.tail acc)
    loop line []

let scoreByCorruptedChar c =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137

let calculateCorruptedScore state =
    match state with
    | (State.Valid, _) | (State.Incomplete, _) -> 0
    | (State.Corrupted, ErrorInfo.C c) -> scoreByCorruptedChar c

let scoreByIncompleteChar c =
    match c with
    | '(' -> 1L
    | '[' -> 2L
    | '{' -> 3L
    | '<' -> 4L

let scoreByIncompleteLine l =
    let rec loop l acc =
        match l with
        | [] -> acc
        | c :: rest -> loop rest ((5L*acc) + scoreByIncompleteChar c)
    loop l 0L 

let calculateIncompleteScore state =
    match state with
    | (State.Valid, _) | (State.Corrupted, _) -> 0L
    | (State.Incomplete, ErrorInfo.L l) -> scoreByIncompleteLine l

let getMiddleScore arr =
    let length = Array.length arr 
    let sorted = Array.sort arr 
    sorted.[length/2]

let solve1 () =
    let input = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day10\\input.txt") |> Seq.toList |> List.map Seq.toList
    let states = input |> List.map checkLine
    printfn "States: %A" states
    printfn "Final score: %A " (List.fold (fun acc elem -> acc+calculateCorruptedScore elem) 0 states)

let solve2 () =
    let input = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day10\\input.txt") |> Seq.toList |> List.map Seq.toList
    let states = input |> List.map checkLine
    printfn "States: %A" states
    let finalScores = List.map calculateIncompleteScore states |> List.filter (fun x -> x <> 0L)
    printfn "Final scores with 0s filtered out: %A " finalScores
    printfn "Middle score: %A" (List.toArray finalScores |> getMiddleScore)
