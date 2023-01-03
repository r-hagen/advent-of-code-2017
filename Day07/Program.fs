open System.Collections.Generic
open System.IO

type Program(name: string, weight: int, holding: string list) =
    let holding = holding

    member val Parent = None with get, set
    member val Children = [] with get, set

    member this.Name = name
    member this.Weight = weight

    member this.HoldDisc(dict: Dictionary<string, Program>) =
        if holding.Length > 0 then
            for childName in holding do
                let child = dict[childName]
                child.Parent <- Some(this)
                this.Children <- List.append this.Children [ child ]

let parseProgram (line: string) =
    let removeTrailingComma (s: string) = s.TrimEnd ','
    let parseWeight (w: string) = w.Substring(1, w.Length - 2) |> int

    let parts = line.Split " " |> List.ofArray
    let name = parts[0]
    let weight = parts[1] |> parseWeight

    let discs =
        match parts[2..] with
        | _ :: tail -> tail |> List.map removeTrailingComma
        | [] -> []

    Program(name, weight, discs)

let programDict = Dictionary<string, Program>()

File.ReadAllLines "d07.in"
|> List.ofArray
|> List.map parseProgram
|> List.iter (fun p -> programDict.Add(p.Name, p))

programDict.Values |> Seq.iter (fun program -> program.HoldDisc programDict)

let bottom = programDict.Values |> Seq.find (fun kvp -> kvp.Parent = None)

printfn $"Part 1: {bottom.Name}"

let rec solve (p: Program) =
    let rec stackWeight (stackBottom: Program) (sum: int) =
        let sum = sum + stackBottom.Weight

        match stackBottom.Children with
        | [] -> sum
        | children -> sum + (children |> List.map (fun c -> stackWeight c 0) |> List.sum)

    let areChildrenInBalance (p: Program) =
        p.Children
        |> List.map (fun c -> stackWeight c 0)
        |> List.distinct
        |> List.length = 1

    let rec correctUnbalanced (stackBottom: Program) =
        let stacksByWeight = stackBottom.Children |> List.groupBy (fun c -> stackWeight c 0)

        let unbalancedStackWeight, unbalancedProgram =
            stacksByWeight
            |> List.filter (fun (_, x) -> x.Length = 1)
            |> List.map (fun (c, p) -> c, p.Head)
            |> List.head

        if areChildrenInBalance unbalancedProgram then
            let targetStackWeight, _ =
                stacksByWeight
                |> List.filter (fun (x, _) -> x <> unbalancedStackWeight)
                |> List.head

            unbalancedProgram.Weight + (targetStackWeight - unbalancedStackWeight)
        else
            correctUnbalanced unbalancedProgram

    correctUnbalanced p

let p2 = solve bottom
printfn $"Part 2: {p2}"
