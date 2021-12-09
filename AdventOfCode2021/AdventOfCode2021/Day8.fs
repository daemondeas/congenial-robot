module Day8
    type NumberMapping =
        { Pattern : string
          Number  : int }

    let lengthsOfOneFourSevenAndEight =
        [| 2; 3; 4; 7 |]

    let countOnesFoursSevensAndEights (output: string []) =
        Array.filter (fun (s: string) -> Array.contains s.Length lengthsOfOneFourSevenAndEight) output
        |> fun a -> a.Length

    let countOutputs (outputs: string[][]) =
        Array.map countOnesFoursSevensAndEights outputs |> Array.fold (+) 0

    let getOutputFromRow (row: string) =
        row.Split "|"
        |> fun (a: string[]) -> a.[1]
        |> fun (s: string) -> s.Trim()
        |> fun (s: string) -> s.Split " "

    let getOutputs (arg: string) =
        arg.Split "\n"
        |> Array.map getOutputFromRow

    let firstTask (arg: string) =
        getOutputs arg
        |> countOutputs

    let isLength n (s: string) =
        s.Length = n

    let first (ss: string[]) =
        ss.[0]

    let rec containsAll (o: char[]) (i: char list) =
        match i with
        | []    -> true
        | x::xs -> (Array.contains x o) && containsAll o xs

    let containsAllChars (s1: string) (s2: string) =
        let chars = s2.ToCharArray()
        s1.ToCharArray() |> Array.toList |> containsAll chars

    let removeFrom (s1: string) (s2: string) =
        s2.ToCharArray()
        |> Array.filter (fun (a: char) -> not (Array.contains a (s1.ToCharArray())))
        |> System.String

    let mapInputs (input: string[]) =
        let one = Array.filter (isLength 2) input |> first
        let four =  Array.filter (isLength 4) input |> first
        let seven = Array.filter (isLength 3) input |> first
        let eight = Array.filter (isLength 7) input |> first
        let three = Array.filter (isLength 5) input |> Array.filter (containsAllChars one) |> first
        let five = Array.filter (isLength 5) input |> Array.filter (fun (s: string) -> s <> three) |> Array.filter (containsAllChars (removeFrom one four)) |> first
        let two = Array.filter (isLength 5) input |> Array.filter (fun (s: string) -> s <> three && s <> five) |> first
        let six = Array.filter (isLength 6) input |> Array.filter (fun s -> not (containsAllChars one s)) |> first
        let nine = Array.filter (isLength 6) input |> Array.filter (containsAllChars four) |> first
        let zero = Array.filter (isLength 6) input |> Array.filter (fun (s: string) -> s <> six && s <> nine) |> first
        [
            {
                Pattern = zero
                Number  = 0
            };
            {
                Pattern = one
                Number  = 1
            };
            {
                Pattern = two
                Number  = 2
            };
            {
                Pattern = three
                Number  = 3
            };
            {
                Pattern = four
                Number  = 4
            };
            {
                Pattern = five
                Number  = 5
            };
            {
                Pattern = six
                Number  = 6
            };
            {
                Pattern = seven
                Number  = 7
            };
            {
                Pattern = eight
                Number  = 8
            };
            {
                Pattern = nine
                Number  = 9
            }
        ]

    let rec decode s inputs =
        match inputs with
        | []    -> -1
        | x::xs ->
            if containsAllChars s x.Pattern && containsAllChars x.Pattern s then
                x.Number
            else
                decode s xs

    let decodeOutput (ss: string[]) inputs =
        (decode ss.[0] inputs) * 1000 + (decode ss.[1] inputs) * 100 + (decode ss.[2] inputs) * 10 + (decode ss.[3] inputs)

    let getRowValue (s: string) =
        let parts = s.Split " | "
        mapInputs (parts.[0].Split " ")
        |> (decodeOutput (parts.[1].Split " "))

    let secondTask (arg: string) =
        arg.Split "\n"
        |> Array.map getRowValue
        |> Array.fold (+) 0
