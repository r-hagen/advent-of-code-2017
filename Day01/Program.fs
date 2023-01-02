open System.IO

let input = File.ReadAllText "d01.in"

let solve input part =
    let numbers = input |> List.ofSeq |> List.map string |> List.map int

    let matchesNext i =
        numbers[i] = numbers[(i + 1) % numbers.Length]

    let matchesHalfwayAround i =
        numbers[i] = numbers[(i + numbers.Length / 2) % numbers.Length]
        
    let matches = if part = 1 then matchesNext else matchesHalfwayAround

    numbers
    |> List.indexed
    |> List.map (fun (i, n) -> if matches i then n else 0)
    |> List.sum

printfn $"Part 1: {solve input 1}"
printfn $"Part 2: {solve input 2}"
