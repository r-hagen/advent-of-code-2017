open System.Collections.Generic
open System.IO

let parseLine (line: string) = line.Split " " |> Array.map int

let initMemory () = File.ReadAllText "d06.in" |> parseLine
let initSeen () = Dictionary<int list, int>()

type Part =
    | Part1
    | Part2

type Cycle = int
type Memory = int[]
type Cache = Dictionary<int list, int>

let rec solve (memory: Memory) (cache: Cache) (cycles: Cycle) (part: Part) =
    let rec mostBlocksIndex index max_index max_value =
        match index with
        | i when i >= memory.Length -> max_index
        | i when memory[i] > max_value -> mostBlocksIndex (i + 1) i memory[i]
        | _ -> mostBlocksIndex (index + 1) max_index max_value

    let mostIndex = mostBlocksIndex 0 0 0
    let mostBlocks = memory[mostIndex]
    memory[mostIndex] <- 0

    let rec distributeBlocks index blocks =
        let j = index % memory.Length

        match blocks with
        | b when b > 0 ->
            memory[j] <- memory[j] + 1
            distributeBlocks (index + 1) (blocks - 1)
        | _ -> ()

    distributeBlocks (mostIndex + 1) mostBlocks

    let key = memory |> List.ofArray

    if cache.ContainsKey key then
        if part = Part1 then cycles else cycles - cache[key]
    else
        cache.Add(key, cycles)
        solve memory cache (cycles + 1) part

let p1 = solve (initMemory ()) (initSeen ()) 1 Part1
printfn $"Part 1: {p1}"

let p2 = solve (initMemory ()) (initSeen ()) 1 Part2
printfn $"Part 2: {p2}"
