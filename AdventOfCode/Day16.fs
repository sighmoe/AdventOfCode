module Day16
(*
open Util

let hexToBinary hexStr =
    let rec loop str acc =
        match str with
        | [] -> acc
        | '0' :: rest -> loop rest (acc + "0000" )
        | '1' :: rest -> loop rest (acc + "0001")
        | '2' :: rest -> loop rest (acc + "0010")
        | '3' :: rest -> loop rest (acc + "0011")
        | '4' :: rest -> loop rest (acc + "0100")
        | '5' :: rest -> loop rest (acc + "0101")
        | '6' :: rest -> loop rest (acc + "0110")
        | '7' :: rest -> loop rest (acc + "0111")
        | '8' :: rest -> loop rest (acc + "1000")
        | '9' :: rest -> loop rest (acc + "1001")
        | 'A' :: rest -> loop rest (acc + "1010")
        | 'B' :: rest -> loop rest (acc + "1011")
        | 'C' :: rest -> loop rest (acc + "1100")
        | 'D' :: rest -> loop rest (acc + "1101")
        | 'E' :: rest -> loop rest (acc + "1110")
        | 'F' :: rest -> loop rest (acc + "1111")
    loop hexStr ""

let charListToStr cs = cs |> Array.ofList |> System.String 
let binaryStrToDecimal binary = binary |> Seq.toList |> fromBinary

let parseVersion (binary: string) = binary.[0..2]
let parseTypeId (binary: string) = binary.[3..5]
let parseLengthTypeId (binary: string) = binary.[0]
let parseSubPacketLength (binary: string) = binary.[0..14]
let parseNumOfSubPackets (binary: string) = binary.[0..10]
let parseLiteral (binary: string) =
    printfn "Parsing Literal"
    let literal = binary |> Seq.toList
    let rec loop str acc =
        match str with
        | [] -> acc
        | '0' :: tail -> (acc + charListToStr(tail.[0..3]))
        | '1' :: tail -> loop (tail.[4..]) (acc + charListToStr(tail.[0..3]))
    loop literal ""

let parseSubPacketOperator (binary: string) =
    let lengthTypeId = parseLengthTypeId binary
    printfn "Parsing Operator Subpacket with LengthTypeId: %A" (binaryStrToDecimal [lengthTypeId])
    let payload = binary.[1..]
    printfn "Operator payload: %A" payload
    match (binaryStrToDecimal [lengthTypeId]) with
    | 0 -> 
        let subPacketsLength = parseSubPacketLength payload |> binaryStrToDecimal
        printfn "Parsing subpackets by length. Length: %A" subPacketsLength
        let subpackets = payload.[0..subPacketsLength]
        parseSubPacketsByLength subpackets subPacketsLength 0

    | 1 -> 
        let numOfSubPackets = parseNumOfSubPackets binary
        printfn "Parsing subpackets by number of packets. Number: %A" numOfSubPackets


let rec parseSubPacketByLength (binary: string) (length: int) (read: int) =
    if read = length then ()
    else
        let (version, typeId) = (parseVersion binary, parseTypeId binary)
        printfn "Subpacket Version: %A Subpacket TypeId: %A" (binaryStrToDecimal version) (binaryStrToDecimal typeId)
        let payload = binary.[6..]
        match (binaryStrToDecimal typeId) with
        | 4 -> 
            let literal = parseLiteral payload
            printfn "Literal: %A " (binaryStrToDecimal literal)
            let literalLength = String.length literal
            let bitsForLiteral = (literalLength + (literalLength/4))
            let nextPayload = payload.[bitsForLiteral..]
            parseSubPacketByLength (nextPayload) length (read+bitsForLiteral)
        | _ -> 
            parseSubPacketOperator payload 

let parseOperator (binary: string) =
    let lengthTypeId = parseLengthTypeId binary
    printfn "Parsing Operator packet with LengthTypeId: %A" (binaryStrToDecimal [lengthTypeId])
    let payload = binary.[1..]
    printfn "Operator payload: %A" payload
    match (binaryStrToDecimal [lengthTypeId]) with
    | 0 -> 
        let subPacketsLength = parseSubPacketLength payload |> binaryStrToDecimal
        printfn "Parsing subpackets by length. Length: %A" subPacketsLength
        let subpackets = payload[0..subPacketsLength]

    | 1 -> 
        let numOfSubPackets = parseNumOfSubPackets binary
        printfn "Parsing subpackets by number of packets. Number: %A" numOfSubPackets

let rec parsePacket (binary: string) =
    let (version, typeId) = (parseVersion binary, parseTypeId binary)
    printfn "Version: %A TypeId: %A" (binaryStrToDecimal version) (binaryStrToDecimal typeId)
    let payload = binary.[6..]
    match (binaryStrToDecimal typeId) with
    | 4 -> printfn "Literal: %A " (parseLiteral payload)
    | _ -> parseOperator payload 

let solve1 () =
    let input = readLines("C:\\Users\\kevin\\source\\repos\\AdventOfCode\\Input\\Day16\\sample2.txt") |> Seq.head 
    let binary = input |> Seq.toList |> hexToBinary
    printfn "Input: %A Binary: %A" input binary
    parsePacket binary *)