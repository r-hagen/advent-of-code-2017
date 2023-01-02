open System.IO

let parseRow (row: string) =
    row.Split "\t" |> List.ofArray |> List.map int

let rows = File.ReadAllLines "d02.in" |> List.ofArray |> List.map parseRow

let subtractLargestSmallest x = (x |> List.max) - (x |> List.min)

let p1 = rows |> List.map subtractLargestSmallest |> List.sum

printfn $"Part 1: {p1}"

let divideEvenlyDivisible (numbers: int list) =
    let dividesEvenly x y = x <> y && x % y = 0

    let rec evenlyDivisible x nums =
        match nums with
        | [] -> None
        | y :: ys ->
            if dividesEvenly x y then Some(x / y)
            elif dividesEvenly y x then Some(y / x)
            else evenlyDivisible x ys

    numbers |> List.pick (fun n -> evenlyDivisible n numbers)

let p2 = rows |> List.map divideEvenlyDivisible |> List.sum

printfn $"Part 2: {p2}"
