module Day3
    let getValue args conversion =
        Common.columns args 0
        |> List.map (conversion '0' '1')
        |> Array.ofList
        |> (fun (s: char[]) -> System.String.Concat(s))
        |> (fun s -> System.Convert.ToInt32(s, 2))

    let gamma args =
        getValue args Common.mostCommon

    let epsilon args =
        getValue args Common.leastCommon

    let firstTask args =
        (gamma args) * (epsilon args)

    let rec bitCriteriaFilter (remainingRows: char[][]) index conversion =
        if remainingRows.Length = 1 then
            System.Convert.ToInt32(System.String.Concat(remainingRows.[0]), 2)
        else
            let columns = Common.columns remainingRows 0
            let bit = conversion columns.[index]
            bitCriteriaFilter (Array.filter (fun c -> c.[index] = bit) remainingRows) (index + 1) conversion

    let oxygen args =
        bitCriteriaFilter args 0 (Common.mostCommon '0' '1')

    let leastCommonAndExists x y xs =
        if (((Common.leastCommon x y xs) = y) && (Array.contains y xs)) then
            y
        else
            x

    let co2Scrubber args =
        bitCriteriaFilter args 0 (leastCommonAndExists '0' '1')

    let secondTask args =
        (oxygen args) * (co2Scrubber args)