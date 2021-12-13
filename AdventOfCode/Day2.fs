module Day2

open Util

let calculatePosition1 list =
    let rec loop list x y =
        match list with
        | [] -> (x,y)
        | tup :: rest -> match tup with
                         | ("forward", dx) -> loop rest (x+dx) y
                         | ("down", dy) -> loop rest x (y+dy)
                         | ("up", dy) -> loop rest x (y-dy)
    loop list 0 0

let solve1 () =
    let vals =
        readLines ("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day2\\input1.txt") 
        |> Seq.toList
        |> List.map (fun line -> line.Split ' ')
        |> List.map (fun pair -> (pair.[0], int(pair.[1])))

    printfn "%A" (calculatePosition1 vals)

let calculatePosition2 list =
    let rec loop list x y aim =
        match list with
        | [] -> (x,y)
        | tup :: rest -> match tup with
                         | ("forward", dx) -> loop rest (x+dx) (y+(aim*dx)) aim
                         | ("down", daim) -> loop rest x y (aim+daim)
                         | ("up", daim) -> loop rest x y (aim-daim)
    loop list 0 0 0

let solve2 () =
    let vals =
        readLines ("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day2\\input1.txt") 
        |> Seq.toList
        |> List.map (fun line -> line.Split ' ')
        |> List.map (fun pair -> (pair.[0], int(pair.[1])))

    printfn "%A" (calculatePosition2 vals)

