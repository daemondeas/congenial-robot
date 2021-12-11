module Day9
    let parseRow (arg: string) =
        arg.ToCharArray() |> Array.map int |> Array.map (fun i -> i - 48)

    let parse (args: string[]) =
        Array.map parseRow args

    let getValue (maxX: int) (maxY: int) (x: int) (y: int) (map: int[][]) =
        if x < 0 || y < 0 || x > maxX || y > maxY then
            10
        else
            map.[y].[x]

    let isLow (maxX: int) (maxY: int) (x: int) (y: int) (map: int[][]) =
        let value = getValue maxX maxY x y map
        value < (getValue maxX maxY (x - 1) y map) &&
        value < (getValue maxX maxY (x + 1) y map) &&
        value < (getValue maxX maxY x (y - 1) map) &&
        value < (getValue maxX maxY x (y + 1) map)

    let sumRisk (lowPoints: int list) =
        List.fold (+) (lowPoints.Length) lowPoints

    let nextX maxX x =
        if x = maxX then
            0
        else
            x + 1

    let nextY maxX x y =
        if x = maxX then
            y + 1
        else
            y

    let rec filterForLows (maxX: int) (maxY: int) (x: int) (y: int) (map: int[][]) (result: int list) =
        match y > maxY with
        | true -> result
        | _    ->
            if (isLow maxX maxY x y map) then
                let lowValue = getValue maxX maxY x y map
                filterForLows maxX maxY (nextX maxX x) (nextY maxX x y) map (lowValue::result)
            else
                filterForLows maxX maxY (nextX maxX x) (nextY maxX x y) map result

    let getLows (map: int[][]) =
        let maxY = map.Length - 1
        let maxX = map.[0].Length - 1
        filterForLows maxX maxY 0 0 map []

    let firstTask (args: string[]) =
        parse args |> getLows |> sumRisk
