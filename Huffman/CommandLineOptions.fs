module CommandLineOptions

type OperationMode =
    | Encode
    | Decode

type Options =
    { Mode: OperationMode
      InputFile: unit -> string
      OutputFile: string }

let provide x = (fun () -> x)
let require s = (fun () -> failwithf "%s required" s)
      
let parseArgs defaultOptions (argv: string[]) =
    let rec inner options argv =
        match argv with
        | [] -> options
        | filename::[] ->
            let options = { options with InputFile = provide filename }
            options
        | "-o"::filename::xs ->
            let options = { options with OutputFile = filename }
            inner options xs
        | "-d"::xs ->
            let options = { options with Mode = Decode }
            inner options xs
        | x::_ ->
            failwithf "Unexpected argument '%s'" x
    argv
    |> List.ofArray
    |> inner defaultOptions
