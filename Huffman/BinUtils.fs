/// Provides binary conversion and manipulation utilities.
module BinUtils

open System

/// Formats an integer as a binary number left padded with zeroes
let toBin nbits (n: int) =
    let s = Convert.ToString(n, 2)
    s.PadLeft(nbits, '0')

/// Reverses the bits in a number of a given bit width
let reverseBits nBits num =
    let rec inner (n: int) a b =
        let aVal = n &&& a
        let bVal = n &&& b
        let aOn = aVal = a
        let bOn = bVal = b
        if b <= a then
            n
        elif aOn <> bOn then
            inner (n ^^^ a ^^^ b) (a <<< 1) (b >>> 1)
        else
            inner n (a <<< 1) (b >>> 1)
            
    let highBit = 2.0 ** (float nBits - 1.0) |> int
    inner num 1 highBit

/// Converts an integer to bytes in little endian
let intToBytes n =
    let topHalf = n >>> 16
    let bottomHalf = n &&& 0xffff
    [ byte (bottomHalf &&& 0xff)
      byte (bottomHalf >>> 8)
      byte (topHalf &&& 0xff)
      byte (topHalf >>> 8) ]

/// Converts the lower 16 bits of an intger to bytes in little endian
let intToBytesShort n =
    let bottomHalf = n &&& 0xffff
    [ byte (bottomHalf &&& 0xff)
      byte (bottomHalf >>> 8) ]

let bytesToInt (arr: byte[]) index =
    int arr.[index] |||
    (int arr.[index + 1] <<< 8) |||
    (int arr.[index + 2] <<< 16) |||
    (int arr.[index + 3] <<< 24)

/// Converts an integer up to the specified number of bits to bytes
let getBytes bits lastPlace flush =
    let numBytes =
        if flush
            then (lastPlace >>> 3) + 1
            else lastPlace >>> 3
    let rec inner place mask n out =
        if n = 0 then
            out
        else
            let nextByte = byte ((bits &&& mask) >>> place)
            inner
                (place + 8)
                (mask <<< 8)
                (n - 1)
                (nextByte :: out)
    inner 0 0xff numBytes []

/// Concatenates variable-length sequences of bits
let packBits bits =
    let rec inner place bits buf out =
        match bits with
        | _ when place >= 8 ->
            // Shift bytes from the buffer into out as long as there are 8 or more bits
            inner
                (place - 8)
                bits
                (buf >>> 8)
                (byte (buf &&& 0xff) :: out)
        | [] when place = 0 ->
            0, out
        | [] ->
            let out = byte buf :: out
            let paddingBits = 8 - place
            paddingBits, out
        | (codeLength, code)::xs ->
            inner
                (place + codeLength)
                xs
                (buf ||| (code <<< place))
                out
    let paddingBits, out = inner 0 bits 0 []
    let out = out |> List.rev |> Array.ofList
    paddingBits, out

/// Converts a byte array into a lazily evlauated sequence of bits
let toBitStream bytes =
    bytes
    |> Seq.collect (fun b ->
        [| b &&& 1uy = 1uy;
          b &&& 2uy = 2uy;
          b &&& 4uy = 4uy;
          b &&& 8uy = 8uy;
          b &&& 16uy = 16uy;
          b &&& 32uy = 32uy;
          b &&& 64uy = 64uy;
          b &&& 128uy = 128uy |])

/// Converts a sequence of 8 bools to a char
let bitsToChar bits =
    let inline value place bit =
        match bit with
        | false -> 0uy
        | true -> place
    if Seq.length bits < 8 then failwith "Length must be exactly 8"
    let [a; b; c; d; e; f; g; h] = List.ofSeq bits
    value 128uy a +
    value 64uy b +
    value 32uy c +
    value 16uy d +
    value 8uy e +
    value 4uy f +
    value 2uy g +
    value 1uy h
    |> char
