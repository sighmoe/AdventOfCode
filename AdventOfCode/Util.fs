module Util

open System.IO

let readLines (filePath: string) =
    seq {
        use sr = new StreamReader(filePath)

        while not sr.EndOfStream do
            yield sr.ReadLine()
    }

let read2DIntArray (filePath: string) = readLines(filePath) |> Seq.map (fun str -> (Seq.map (fun digit -> (int digit - int '0')) str)) |> Seq.map (fun seq -> Seq.toArray seq) |> Seq.toArray

type 'a Edge = 'a * 'a

type 'a Graph = 'a list * 'a Edge list

type 'a Node = 'a * 'a list

type 'a AdjacencyGraph = 'a Node list

