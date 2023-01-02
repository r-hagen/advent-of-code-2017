open System.IO

let duplicates (p: string) =
    p.Split " " |> Array.countBy id |> Array.forall (fun (_, v) -> v <= 1)

let anagrams (p: string) =
    let sortString (s: string) =
        s.ToCharArray() |> Array.sort |> Array.map string |> String.concat ""

    p.Split " "
    |> Array.map sortString
    |> Array.countBy id
    |> Array.forall (fun (_, v) -> v <= 1)

let passphrases = File.ReadAllLines "d04.in"

let p1 = passphrases |> Array.filter duplicates |> Array.length
printfn $"Part 1: {p1}"

let p2 = passphrases |> Array.filter duplicates |> Array.filter anagrams |> Array.length
printfn $"Part 2: {p2}"
