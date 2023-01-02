open System.IO

let rec jump (offsets: int[]) (index: int) (step: int) (part: int) =
    let old_index = index
    let new_index = index + offsets[old_index]

    let increaseByOne i = offsets[i] <- offsets[i] + 1
    let decreaseByOne i = offsets[i] <- offsets[i] - 1

    let increaseOrDecreaseByOne i =
        match offsets[i] with
        | x when x >= 3 -> decreaseByOne i
        | _ -> increaseByOne i

    match part with
    | 1 -> increaseByOne old_index
    | 2 -> increaseOrDecreaseByOne old_index
    | _ -> failwith "Must be part 1 or 2"

    match new_index with
    | i when i < 0 || i >= offsets.Length -> step
    | _ -> jump offsets new_index (step + 1) part

let input () = File.ReadAllLines "d05.in" |> Array.ofSeq |> Array.map int

let p1 = jump (input ()) 0 1 1
printfn $"Part 1: {p1}"

let p2 = jump (input ()) 0 1 2
printfn $"Part 2: {p2}"
