module Util

open System.IO

let readLines (filePath: string) =
    seq {
        use sr = new StreamReader(filePath)

        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

let read2DIntArray (filePath: string) = readLines(filePath) |> Seq.map (fun str -> (Seq.map (fun digit -> (int digit - int '0')) str)) |> Seq.map (fun seq -> Seq.toArray seq) |> Seq.toArray

let fromBinary list =
    let rec loop list idx acc =
        match list with
        | [] -> acc
        | '1' :: rest -> loop rest (idx-1) (acc + 1 * (pown 2 idx))
        | '0' :: rest -> loop rest (idx-1) (acc)

    loop list (List.length list - 1) 0 

type 'a Edge = 'a * 'a

type 'a Graph = 'a list * 'a Edge list

type 'a Node = 'a * 'a list

type 'a AdjacencyGraph = 'a Node list

