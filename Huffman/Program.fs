open BinUtils
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

    let sw = new Stopwatch()
    sw.Start()
    let decoded = HuffmanTree.decode (contentStart + 4) textLength tree content
    sw.Stop()

    using
        (new BinaryWriter(File.Open(options.OutputFile, FileMode.Create)))
        (fun w -> w.Write(decoded))

    printf "decoded in %dms" sw.ElapsedMilliseconds
    0

let encode options =
    let inputFile = options.InputFile()
    let content = File.ReadAllBytes(inputFile)
    let tree = HuffmanTree.fromContent content

    let sw = new Stopwatch()
    sw.Start()
    let encodedLength, encoded = HuffmanTree.encode tree content
    sw.Stop()

    let treeBytes = HuffmanTree.toBytes tree
    let encodedLengthWithTable = encoded.Length + treeBytes.Length

    using
        (new BinaryWriter(File.Open(options.OutputFile, FileMode.Create)))
        (fun w ->
            w.Write(treeBytes.Length + 8)
            w.Write(treeBytes)
            w.Write("ZZZZ" |> Seq.map byte |> Array.ofSeq)
            w.Write(encodedLength)
            w.Write(encoded))

    printfn
        "\nencoded in %dms\n%g%% compression ratio (%s -> %d bytes Huffman tree + %s text)\n"
        sw.ElapsedMilliseconds
        ((1.0 - (float encodedLengthWithTable) / (float content.Length)) * 100.0)
        (sizeDesc content.Length)
        treeBytes.Length
        (sizeDesc encoded.Length)

    0
    
[<EntryPoint>]
let main argv =
    let options = parseArgs defaultOptions argv
    match options.Mode with
    | Encode -> encode options
    | Decode -> decode options
