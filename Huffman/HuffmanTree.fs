namespace Huffman

open System

type CodeLength = CodeLength of int
type HuffmanCode = HuffmanCode of int
type CodeTableEntry = CodeLength * HuffmanCode

type HuffmanTree =
    | Leaf of char * int
    | Node of string * int * HuffmanTree * HuffmanTree

type DecodingResult = {
    /// The time in milliseconds it took to parse the tree.
    TreeParseMilliseconds: int

    /// The time in milliseconds it took to decode the content.
    DecodeMilliseconds: int
    
    /// The Huffman tree stored in the content.
    Tree: HuffmanTree

    /// The total number of bytes used to store metadata (including the Huffman tree).
    MetadataLength: int

    /// The decoded content.
    Output: byte[]
}

type EncodingResult = {
    /// The time in milliseconds it took to build the Huffman tree.
    TreeBuildMilliseconds: int

    /// The time in milliseconds it took to encode the content.
    EncodeMilliseconds: int 

    /// The generated code table.
    CodeTable: (char * CodeTableEntry) list

    /// The generated Huffman tree in packed binary format.
    Tree: byte[]

    /// The number of unused bits at the end of the encoded content.
    PaddingBits: byte

    /// The encoded content.
    Output: byte[]
}

module HuffmanTree =
    open BinUtils
    open MiscUtils

    let private getWeight tree =
        match tree with
        | Leaf (_, weight)
        | Node (_, weight, _, _) -> weight

    let private getNameAndWeight tree =
        match tree with
        | Leaf (c, weight) ->
            let name =
                match c with
                | ' ' -> "_"
                | _ -> string c
            (name, weight)
        | Node (name, weight, _, _) -> (name, weight)

    /// Creates a new node with the specified nodes as children.
    let private join left right =
        let (leftName, leftWeight) = getNameAndWeight left
        let (rightName, rightWeight) = getNameAndWeight right
        let name = leftName + rightName
        let weight = leftWeight + rightWeight
        Node (name, weight, left, right)

    /// Inserts a node into a node list in order of weight.
    let private insertNode = insert getWeight

    /// Converts a Huffman tree to packed binary format.
    let private toBytes tree =
        let rec inner subtree out =
            match subtree with
            | Leaf (symbol, _) ->
                let reverseSymbol = reverseBits 8 (int symbol)
                (8, reverseSymbol) :: (1, 0) :: out
            | Node (_, _, left, right) ->
                (1, 1) :: out
                |> inner left
                |> inner right
        inner tree []
        |> List.rev
        |> packBits
        |> snd

    /// Parses a Huffman tree from packed binary format.
    let private fromBytes (bytes: byte[]) =
        let rec inner bits =
            let next, bits = List.head bits, List.tail bits
            match next with
            | false ->
                let symbol = List.take 8 bits |> bitsToChar
                let bits = List.skip 8 bits
                bits, Leaf (symbol, 0)
            | true ->
                let bits, left = inner bits
                let bits, right = inner bits
                bits, Node ("", 0, left, right)
        bytes
        |> toBitStream
        |> List.ofSeq
        |> inner
        |> snd

    /// Creates a Huffman tree from a byte array.
    let private fromContent (str: byte[]) =
        let rec inner queue =
            match queue with
            | [tree] -> tree
            | a::b::xs ->
                (a, b)
                ||> join
                |> insertNode xs
                |> inner
        str
        |> Seq.map char
        |> Seq.countBy id
        |> Seq.sortBy snd
        |> Seq.map Leaf
        |> List.ofSeq
        |> inner

    /// Creates a code table from a Huffman tree.
    let private makeCodeTable tree =
        let rec inner subtree out code =
            match subtree with
            | Leaf (c, _) ->
                let codeLength = List.length code
                let intCode =
                    code
                    |> List.rev
                    |> List.mapi (fun i b ->
                        if b
                            then int (2.0 ** float i)
                            else 0)
                    |> List.sum
                (c, (CodeLength codeLength, HuffmanCode intCode)) :: out
            | Node (_, _, left, right) ->
                let out = inner left out (false :: code)
                let out = inner right out (true :: code)
                out
        inner tree [] []

    let private encodeBytes codeTable content =
        let codeDict =
            codeTable
            |> Seq.map (fun (symbol, (CodeLength codeLength, HuffmanCode code)) ->
                (byte symbol, (codeLength, code)))
            |> Map.ofSeq
        content
        |> Seq.map (fun c -> codeDict.[c])
        |> List.ofSeq
        |> packBits
            
    let private decodeBytes contentStart numPaddingBits root content =
        let rec inner subtree out bits =
            match bits, subtree with
            | [], Node _ ->
                failwith "Ran out of input"
            | [], Leaf (symbol, _) ->
                byte symbol :: out |> List.rev
            | _, Leaf (symbol, _) ->
                inner root (byte symbol :: out) bits
            | false::xs, Node (_, _, left, _) ->
                inner left out xs
            | true::xs, Node (_, _, _, right) ->
                inner right out xs

        let bits =
            content
            |> Seq.skip (contentStart + 1)
            |> toBitStream

        bits
        |> Seq.take (Seq.length bits - numPaddingBits)
        |> List.ofSeq
        |> inner root []
        |> Array.ofList

    /// Creates a Huffman tree for the content and encodes it.
    let encode content =
        let treeMillisec, tree = time (fun () -> fromContent content)
        let codeTable = makeCodeTable tree
        let elapsedMillisec, (paddingBits, bytesOut) = time (fun () -> encodeBytes codeTable content)
        let treeBytes = toBytes tree
        
        { TreeBuildMilliseconds = int treeMillisec
          EncodeMilliseconds = int elapsedMillisec
          CodeTable = codeTable
          Tree = treeBytes
          PaddingBits = byte paddingBits
          Output = bytesOut }

    /// Reads the Huffman tree from the content and uses it to decode the text.        
    let decode content =
        let contentStart = bytesToInt content 0
        let numPaddingBits = int content.[contentStart]
        let treeMillisec, tree = time (fun () -> content |> Array.skip 4 |> fromBytes)
        let elapsedMillisec, bytes = time (fun () -> decodeBytes contentStart numPaddingBits tree content)

        { TreeParseMilliseconds = int treeMillisec
          DecodeMilliseconds = int elapsedMillisec
          Tree = tree
          MetadataLength = contentStart + 4
          Output = bytes }
