﻿open BinUtils
open CommandLineOptions
open Huffman
open MiscUtils
open System.Diagnostics
open System.IO

let defaultOptions =
    { Mode = Encode
      InputFile = require "input file"
      OutputFile = "huffman.out" }

let decode options =
    let inputFile = options.InputFile()
    let content = File.ReadAllBytes(inputFile)
    let contentStart = bytesToInt content 0
    let tree = content |> Array.skip 4 |> HuffmanTree.fromBytes
    let textLength = bytesToInt content contentStart

    let elapsedMillisec, decoded = time (fun () -> HuffmanTree.decode (contentStart + 4) textLength tree content)

    using
        (new BinaryWriter(File.Open(options.OutputFile, FileMode.Create)))
        (fun w -> w.Write(decoded))

    printfn "decoded in %dms" elapsedMillisec

    let decompressionRate = (float decoded.Length) / (float elapsedMillisec / 1000.0) |> int
    printfn "decompression rate %s/sec" (sizeDesc decompressionRate)

    let originalLength = content.Length
    let metadataLength = contentStart + 4
    let decodedLength = decoded.Length

    printfn "%g%% inflation ratio (%d bytes Huffman tree + %s compressed text -> %s)"
        ((float decodedLength) / (float originalLength) * 100.0)
        metadataLength
        (sizeDesc (originalLength - metadataLength))
        (sizeDesc decodedLength)

    0

let encode options =
    let inputFile = options.InputFile()
    let content = File.ReadAllBytes(inputFile)
    let tree = HuffmanTree.fromContent content

    let elapsedMillisec, (encodedLengthBits, encoded) = time (fun () -> HuffmanTree.encode tree content)
    let treeBytes = HuffmanTree.toBytes tree

    let originalLength = content.Length
    let metadataLength = treeBytes.Length + 8
    let encodedLength = encoded.Length
    let encodedLengthWithTable = encodedLength + metadataLength

    using
        (new BinaryWriter(File.Open(options.OutputFile, FileMode.Create)))
        (fun w ->
            w.Write(metadataLength)
            w.Write(treeBytes)
            w.Write("ZZZZ" |> Seq.map byte |> Array.ofSeq)
            w.Write(encodedLengthBits)
            w.Write(encoded))

    printfn "encoded in %dms" elapsedMillisec

    let compressionRate = (float content.Length) / (float elapsedMillisec / 1000.0) |> int
    printfn "compression rate %s/sec" (sizeDesc compressionRate)

    printfn "%g%% compression ratio (%s -> %d bytes Huffman tree + %s text)"
        ((1.0 - (float encodedLengthWithTable) / (float content.Length)) * 100.0)
        (sizeDesc originalLength)
        (metadataLength + 4)
        (sizeDesc encodedLength)

    0
    
[<EntryPoint>]
let main argv =
    let options = parseArgs defaultOptions argv
    match options.Mode with
    | Encode -> encode options
    | Decode -> decode options
