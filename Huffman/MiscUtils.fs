module MiscUtils

open System.Diagnostics

/// Inserts an element into a list, with the order given by the specified function.
let insert f xs n =
    let nValue = f n
    let rec inner i = function
        | [] -> List.length xs
        | x::_ when (f x) > nValue -> i
        | _::xs -> inner (i + 1) xs
    let insertPoint = inner 0 xs
    xs.[0..insertPoint - 1] @ n :: xs.[insertPoint..xs.Length - 1]

/// Converts a file size in bytes to a human readable format.
let sizeDesc size =
    let GIGA = 1073741824.0
    let MEGA = 1048576.0
    let KILO = 1024.0
    let size = float size
    let (size, unit) =
        if size >= GIGA then size / GIGA, "GB"
        elif size >= MEGA then size / MEGA, "MB"
        elif size >= KILO then size / KILO, "KB"
        else size, "bytes"
    sprintf "%1.3g %s" size unit

let time fn =
    let sw = new Stopwatch()
    sw.Start()
    let result = fn()
    sw.Stop()
    sw.ElapsedMilliseconds, result
