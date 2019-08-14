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
    let treeMillisec, tree = time (fun () -> content |> Array.skip 4 |> HuffmanTree.fromBytes)
    let numPaddingBits = int content.[contentStart]

    let elapsedMillisec, decoded = time (fun () -> HuffmanTree.decode (contentStart + 1) numPaddingBits tree content)

    using
        (new BinaryWriter(File.Open(options.OutputFile, FileMode.Create)))
        (fun w -> w.Write(decoded))

    printfn "decoded tree in %dms" treeMillisec
    printfn "decoded text in %dms" elapsedMillisec

    let decompressionRate = (float decoded.Length) / (float elapsedMillisec / 1000.0) |> int
    printfn "decompression rate %s/sec" (sizeDesc decompressionRate)

    let originalLength = content.Length
    let metadataLength = contentStart + 4
    let decodedLength = decoded.Length

    printfn "%1.3g%% inflation ratio (%d bytes Huffman tree + %s compressed text -> %s)"
        ((float decodedLength) / (float originalLength) * 100.0)
        metadataLength
        (sizeDesc (originalLength - metadataLength))
        (sizeDesc decodedLength)

    0

type FooBar =
    { Symbol: string
      Frequency: int
      Code: string }

let encode options =
    let inputFile = options.InputFile()
    let content = File.ReadAllBytes(inputFile)
    let treeMillisec, tree = time (fun () -> HuffmanTree.fromContent content)
    let codeTable = HuffmanTree.makeCodeTable tree

    // Show the code table with symbol frequencies
    query {
        for (symbol, freq) in Seq.countBy id content do
            join (s, code) in codeTable 
                on (symbol = byte s)
            sortByDescending freq
            thenBy code
            select {
                Symbol =
                    match symbol with
                    | 32uy -> "space"
                    | 13uy -> "CR"
                    | 10uy -> "LF"
                    | _ when symbol > 32uy && symbol < 127uy -> sprintf "'%c'" (char symbol)
                    | _ -> sprintf "0x%02x" symbol
                Frequency = freq
                Code = toBin (fst code) (snd code)
            }
    }
    |> Seq.iter (fun x -> printfn "%s\t%d\t%s" x.Symbol x.Frequency x.Code)

    let elapsedMillisec, (numPaddingBits, encoded) = time (fun () -> HuffmanTree.encode tree content)
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
            w.Write(numPaddingBits)
            w.Write(encoded))

    printfn "\nbuilt tree in %dms" treeMillisec
    printfn "encoded in %dms" elapsedMillisec

    let compressionRate = (float content.Length) / (float elapsedMillisec / 1000.0) |> int
    printfn "compression rate %s/sec" (sizeDesc compressionRate)

    printfn "%1.3g%% compression ratio (%s -> %d bytes Huffman tree + %s text)"
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
