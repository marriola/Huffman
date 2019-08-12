namespace Huffman

type HuffmanTree =
    | Leaf of char * int
    | Node of string * int * HuffmanTree * HuffmanTree

module HuffmanTree =
    open BinUtils
    open MiscUtils

    let private getLeft tree =
        match tree with
        | Leaf _ -> failwith "Leaves don't have children"
        | Node (_, _, left, _) -> left
    
    let private getRight tree =
        match tree with
        | Leaf _ -> failwith "Leaves don't have children"
        | Node (_, _, _, right) -> right

    let private getWeight tree =
        match tree with
        | Leaf (_, weight) -> weight
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
    let toBytes tree =
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

    /// Parses a Huffman tree from packed binary format.
    let fromBytes (bytes: byte[]) =
        let rec inner bits =
            let next, bits = Seq.head bits, Seq.tail bits
            match next with
            | false ->
                let symbol = Seq.take 8 bits |> bitsToChar
                let bits = Seq.skip 8 bits
                bits, Leaf (symbol, 0)
            | true ->
                let bits, left = inner bits
                let bits, right = inner bits
                bits, Node ("", 0, left, right)
        bytes
        |> List.ofArray
        |> toBitStream
        |> inner
        |> snd

    /// Creates a Huffman tree from a byte array.
    let fromContent (str: byte[]) =
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
    let makeCodeTable tree =
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
                (c, (codeLength, intCode)) :: out
            | Node (_, _, left, right) ->
                let out = inner left out (false :: code)
                let out = inner right out (true :: code)
                out
        inner tree [] []
        
    /// Encodes a string using a Huffman tree.
    let encode tree content =
        let codeTable =
            tree
            |> makeCodeTable
            |> Seq.map (fun (symbol, (codeLength, code)) ->
                (byte symbol, (codeLength, code)))
            |> Map.ofSeq
        let bits = Seq.map (fun c -> codeTable.[c]) content
        let bytesOut = packBits bits
        let totalBits =
            bits
            |> Seq.map (fun (n, _) -> n)
            |> Seq.sum
        totalBits, bytesOut
        
    let decode startPosition contentLength tree content =
        let rec inner contentLength subtree out bits =
            match bits, subtree with
            | [], _ ->
                failwith "Ran out of input"
            | _, Leaf (symbol, _) ->
                let out = (byte symbol) :: out
                match contentLength with
                | 0 -> List.rev out
                | _ -> inner contentLength tree out bits
            | x::xs, Node (_, _, _, right) when x ->
                inner (contentLength - 1) right out xs
            | _::xs, Node (_, _, left, _) ->
                inner (contentLength - 1) left out xs
        content
        |> Seq.skip startPosition
        |> toBitStream
        |> List.ofSeq
        |> inner contentLength tree []
        |> Array.ofList
