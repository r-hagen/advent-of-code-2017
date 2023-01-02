open System
open System.Collections.Generic

let input = 361527

let spiral (n: int) =
    let k = Math.Ceiling((Math.Sqrt(n) - 1.0) / 2.0) |> int
    let mutable t = 2 * k + 1
    let mutable m = Math.Pow(t, 2) |> int
    t <- t - 1

    if n >= m - t then
        (k - (m - n), -k)
    else
        m <- m - t

        if n >= m - t then
            (-k, -k + (m - n))
        else
            m <- m - t

            if n >= m - t then
                (-k + (m - n), k)
            else
                (k, k - (m - n - t))

type Coord = int * int

let distance (p1: Coord) =
    let x1, y1 = p1
    Math.Abs(x1) + Math.Abs(y1)

printfn $"Part 1: {spiral input |> distance}"

let d = Dictionary<Coord, int>()
d[(0, 0)] <- 1

let neighborSum ((x, y): Coord) =
    [ (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) ]
    |> List.map (fun (nx, ny) -> (x + nx, y + ny))
    |> List.map (fun p -> if d.ContainsKey p then d[p] else 0)
    |> List.sum

let rec solve square =
    let coord = spiral square
    let sum = neighborSum coord
    d[coord] <- sum

    match sum with
    | f when f >= 361527 -> sum
    | _ -> solve (square + 1)

printfn $"Part 2: {solve 2}"
