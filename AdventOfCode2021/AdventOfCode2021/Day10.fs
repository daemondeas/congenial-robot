module Day10
    let matches (blockStart: char) (blockClose: char) =
        match blockStart with
        | '(' -> blockClose = ')'
        | '[' -> blockClose = ']'
        | '{' -> blockClose = '}'
        | '<' -> blockClose = '>'
        | _   -> false

    let isOpeningChar c =
        c = '(' || c = '[' || c = '{' || c = '<'

    let rec findFaultInRow (row: char list) (current: System.Collections.Generic.Stack<char>) =
        match row with
        | []    -> None
        | x::xs ->
            if isOpeningChar x then
                current.Push x
                findFaultInRow xs current
            else
                if matches (current.Pop()) x then
                    findFaultInRow xs current
                else
                    Some x

    let getPoints c =
        match c with
        | Some ')' -> 3
        | Some ']' -> 57
        | Some '}' -> 1197
        | Some '>' -> 25137
        | _        -> 0

    let firstTask (args: string[]) =
        Array.map (fun (s: string) -> s.ToCharArray()) args
        |> Array.map Array.toList
        |> Array.map (fun r -> findFaultInRow r (new System.Collections.Generic.Stack<char>()))
        |> Array.map getPoints
        |> Array.fold (+) 0

    let rec getIncompleteRow (row: char list) (current: System.Collections.Generic.Stack<char>) =
        match row with
        | []    -> Some current
        | x::xs ->
            if isOpeningChar x then
                current.Push x
                getIncompleteRow xs current
            else
                if matches (current.Pop()) x then
                    getIncompleteRow xs current
                else
                    None

    let getScore (c: char) =
        match c with
        | '(' -> 1
        | '[' -> 2
        | '{' -> 3
        | '<' -> 4
        | _   -> 0

    let rec getRowPoints (row: System.Collections.Generic.Stack<char>) (currentScore: uint64) =
        match row.Count with
        | 0 -> currentScore
        | _ ->
            let scoreToAdd = getScore (row.Pop())
            getRowPoints row ((currentScore * (uint64 5)) + (uint64 scoreToAdd))

    let getRowScore (row: System.Collections.Generic.Stack<char> option) =
        match row with
        | None       -> uint64 0
        | Some stack -> getRowPoints stack (uint64 0)

    let secondTask (args: string[]) =
        Array.map (fun (s: string) -> s.ToCharArray()) args
        |> Array.map Array.toList
        |> Array.map (fun r -> getIncompleteRow r (new System.Collections.Generic.Stack<char>()))
        |> Array.map getRowScore
        |> Array.filter (fun i -> i <> (uint64 0))
        |> Array.sort
        |> fun a -> a.[(a.Length) / 2]
