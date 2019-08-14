open BinUtils
open CommandLineOptions
open Huffman
open MiscUtils
open System.IO

let defaultOptions =
    { Mode = Encode
      InputFile = require "input file"
      OutputFile = "huffman.out" }

let decode options =
    let inputFile = options.InputFile()
    let content = File.ReadAllBytes(inputFile)
    let result = HuffmanTree.decode content
    File.WriteAllBytes(options.OutputFile, result.Output)

    printfn "decoded tree in %dms" result.TreeParseMilliseconds
    printfn "decoded text in %dms" result.DecodeMilliseconds

    let decompressionRate = (float result.Output.Length) / (float result.DecodeMilliseconds / 1000.0) |> int
    printfn "decompression rate %s/sec" (sizeDesc decompressionRate)

    let originalLength = content.Length
    let decodedLength = result.Output.Length

    printfn "%1.3g%% inflation ratio (%d bytes Huffman tree + %s compressed text -> %s)"
        ((float decodedLength) / (float originalLength) * 100.0)
        result.MetadataLength
        (sizeDesc (originalLength - result.MetadataLength))
        (sizeDesc decodedLength)

    0

type CodeTableEntry =
    { Symbol: string
      Frequency: int
      Code: string }

let encode options =
    let inputFile = options.InputFile()
    let content = File.ReadAllBytes(inputFile)
    let result = HuffmanTree.encode content

    // Show the code table with symbol frequencies
    query {
        for (symbol, freq) in Seq.countBy id content do
            join (s, code) in result.CodeTable 
                on (symbol = byte s)
            let (CodeLength codeLength, HuffmanCode huffmanCode) = code
            sortByDescending freq
            thenBy huffmanCode
            select {
                Symbol =
                    match symbol with
                    | 32uy -> "space"
                    | 13uy -> "CR"
                    | 10uy -> "LF"
                    | _ when symbol > 32uy && symbol < 127uy -> sprintf "'%c'" (char symbol)
                    | _ -> sprintf "0x%02x" symbol
                Frequency = freq
                Code = toBin codeLength huffmanCode
            }
    }
    |> Seq.iter (fun x -> printfn "%s\t%d\t%s" x.Symbol x.Frequency x.Code)

    let originalLength = content.Length
    let metadataLength = result.Tree.Length + 8
    let encodedLength = result.Output.Length
    let encodedLengthWithTable = encodedLength + metadataLength

    using
        (new BinaryWriter(File.Open(options.OutputFile, FileMode.Create)))
        (fun w ->
            w.Write(metadataLength)
            w.Write(result.Tree)
            w.Write("ZZZZ" |> Seq.map byte |> Array.ofSeq)
            w.Write(result.PaddingBits)
            w.Write(result.Output))

    printfn "\nbuilt tree in %dms" result.TreeBuildMilliseconds
    printfn "encoded in %dms" result.EncodeMilliseconds

    let compressionRate = (float content.Length) / (float result.EncodeMilliseconds / 1000.0) |> int
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
