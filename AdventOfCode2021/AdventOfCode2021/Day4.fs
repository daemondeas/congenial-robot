module Day4
    type BingoNumber =
        { Number : int
          Marked : bool }

    type BingoBoard =
        { Rows : BingoNumber[][] }

    let numberScore number =
        if number.Marked then
            0
        else
            number.Number

    let rowScore row =
        Array.map numberScore row |> (Array.fold (+) 0)

    let boardScore board =
        Array.map rowScore board.Rows |> (Array.fold (+) 0)

    let rec bingoOnRow row =
        match row with
        | []    -> true
        | x::xs -> x.Marked && (bingoOnRow xs)

    let rec bingoOnRows rows =
        match rows with
        | []    -> false
        | x::xs -> (bingoOnRow x) || (bingoOnRows xs)

    let bingo board =
        bingoOnRows (Array.toList board.Rows |> List.map Array.toList) || bingoOnRows (Common.columns board.Rows 0 |> List.map Array.toList)

    let checkNumber number boardNumber =
        if boardNumber.Number = number then
            {
                Number = number
                Marked = true
            }
        else
            boardNumber

    let markNumber number board =
        {
            Rows = Array.map (Array.map (checkNumber number)) board.Rows
        }

    let markNumbers boards number =
        List.map (markNumber number) boards

    let rec hasBingo boards =
        match boards with
        | []    -> None
        | b::bs ->
            if (bingo b) then
                Some (boardScore b)
            else
                hasBingo bs

    let rec playUntilBingo boards (numbers: int[]) index =
        let newBoards = markNumbers boards numbers.[index]
        let boardWithBingo = hasBingo newBoards
        match boardWithBingo with
        | None   -> playUntilBingo newBoards numbers (index + 1)
        | Some n -> n * numbers.[index]

    let parseNumbers (s: string) =
        s.Split "," |> Array.map int

    let parseNumber (n: string) =
        n.Trim() |> fun s ->
        {
            Number = int s
            Marked = false
        }

    let parseRow (r: string) =
        r.Trim() |>
        (fun s -> s.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)) |>
        Array.map parseNumber

    let parseBoard (b: string) =
        b.Split "\n" |> Array.map parseRow |> fun rs ->
            {
                Rows = rs
            }

    let firstTask args =
        playUntilBingo (List.tail args |> List.map parseBoard) (parseNumbers (List.head args)) 0

    let rec playUntilLastHasBingo boards (numbers: int[]) index =
        let newBoards = markNumbers boards numbers.[index] |> List.filter (fun b -> not (bingo b))
        match newBoards.Length with
        | 1 -> playUntilBingo newBoards numbers index
        | _ -> playUntilLastHasBingo newBoards numbers (index + 1)

    let secondTask args =
        playUntilLastHasBingo (List.tail args |> List.map parseBoard) (parseNumbers (List.head args)) 0