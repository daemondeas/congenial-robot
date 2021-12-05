module Common
    let column (rows: 'a[][]) (index: int) =
        Array.map (fun (x: 'a[]) -> x.[index]) rows

    let rec columns (rows: 'a[][]) index =
        if index >= (rows.[0].Length) then
            []
        else
            (column rows index)::(columns rows (index + 1))

    let count x xs =
        xs
        |> Seq.filter (fun x' -> x' = x)
        |> Seq.length

    let mostCommon x y xs =
        if (count x xs) > (count y xs) then
            x
        else
            y

    let leastCommon x y xs =
        if (count x xs) > (count y xs) then
            y
        else
            x