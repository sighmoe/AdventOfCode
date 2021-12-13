module Day12

open Util

let addEdgeToExistingList edge edgeList =
    match edgeList with
    | Some edgeList -> Some (edge :: edgeList)
    | None -> None
    
let addEdgeToGraph edge graph =
    let (e1, e2) = edge
    match Map.containsKey e1 graph with
    | true -> Map.change e1 (addEdgeToExistingList e2) graph
    | false -> Map.add e1 [e2] graph

let addUndirectedEdgeToGraph edge graph =
    let (e1, e2) = edge
    addEdgeToGraph (e2,e1) (addEdgeToGraph (e1,e2) graph)

let buildGraph edges =
    let rec loop edges graph =
        match edges with
        | [] -> graph
        | edge :: tail -> loop tail (addUndirectedEdgeToGraph edge graph)
    loop edges Map.empty

let convertStringArrToTuple (s: string[]) = (s.[0], s.[1])

let isNextVertexValid (visited: string) (vertex: string) =
    match vertex with
    | vertex when vertex >= "A" && vertex <= "Z" -> true
    | vertex -> not (visited.Contains vertex)

let isSmallCave (vertex: string) = 
    let result = vertex.ToUpper() <> vertex && vertex <> "end"
    result

let hasVisitedSmallCaveTwice (visited: string) =
    let split = visited.Split(",")
    let rec loop list =
        match list with
        | [] -> false
        | vertex :: rest when (isSmallCave vertex) && List.contains vertex rest -> true
        | _ :: rest -> loop rest
    let result = loop (split |> Array.toList)
    // printfn "Checking hasVisitedSmallCaveTwice Visited: %A Result: %A" visited result
    result

let isNextVertexValid2 (visited: string) (vertex: string) =
    let result = match vertex with
    | "start" -> false
    | vertex when not (isSmallCave vertex) -> true
    | vertex when (isSmallCave vertex) && not (visited.Contains vertex) -> true
    | vertex when (isSmallCave vertex) && (visited.Contains vertex) && not (hasVisitedSmallCaveTwice visited) -> true
    | _ -> false
    // printfn "Checking next vertex. Visited: %A Vertex: %A isValid: %A" visited vertex result
    result


let findPaths (paths: list<string> ref) graph =
    let rec loop graph visited paths curr =
        match curr with
        | "end" -> paths := (visited + ",end") :: (!paths)
        | v -> (Map.find v graph) |> List.iter((fun (x:string) -> if isNextVertexValid visited x then loop graph (visited + "," + v) paths x else ()))
    loop graph "" paths "start"

let findPaths2 (paths: list<string> ref) graph =
    let rec loop graph visited paths curr =
        match curr with
        | "end" -> paths := (visited + ",end") :: (!paths)
        | v -> (Map.find v graph) |> List.iter((fun (x:string) -> if isNextVertexValid2 (visited + "," + v) x then loop graph (visited + "," + v) paths x else ()))
    loop graph "" paths "start"

let printPaths (paths: list<string> ref) =
    printfn "Printing paths"
    List.iter(fun path -> printfn "%A" path) (List.sort !paths)

let solve1 () =
    let paths = ref []
    let graph = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day12\\input.txt") |> Seq.map (fun line -> line.Split("-")) |> Seq.map convertStringArrToTuple |> Seq.toList |> buildGraph 
    graph |> findPaths paths
    printfn "Graph: %A" graph
    printfn "Paths: %A" !paths
    printfn "Total paths: %A" (List.length !paths)

let solve2 () =
    let paths = ref []
    let graph = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day12\\input.txt") |> Seq.map (fun line -> line.Split("-")) |> Seq.map convertStringArrToTuple |> Seq.toList |> buildGraph 
    graph |> findPaths2 paths
    printfn "Graph: %A" graph
    printPaths paths
    printfn "Total paths: %A" (List.length !paths)

