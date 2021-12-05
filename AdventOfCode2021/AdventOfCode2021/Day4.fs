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
        Array.map (markNumber number) boards

    let rec playUntilBingo boards (numbers: int[]) index =
        let newBoards = markNumbers boards numbers.[index]
        for board in newBoards do
            if (bingo board) then
                numbers.[index] * (boardScore board)
        playUntilBingo newBoards numbers (index + 1)
        