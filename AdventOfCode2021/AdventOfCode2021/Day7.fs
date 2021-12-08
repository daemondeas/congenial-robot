module Day7
    let rec countFuel (pos: int) (fuel: int) (crabs: int list) =
        match crabs with
        | []    -> fuel
        | c::cs -> countFuel pos (fuel + (abs (pos - c))) cs

    let findSmallest (crabs: int[]) =
        let middle = crabs.Length / 2
        let crabList = Array.toList crabs
        min (min (countFuel (crabs.[middle - 1]) 0 crabList) (countFuel (crabs.[middle]) 0 crabList)) (countFuel (crabs.[middle + 1]) 0 crabList)

    let firstTask (arg: string) =
        arg.Split ","
        |> Array.map int
        |> Array.sort
        |> findSmallest

    let rec moveToPos pos curPos curCost =
        match pos - curPos with
        | 0 -> curCost
        | _ -> moveToPos pos (curPos + 1) (curCost + curPos + 1)

    let cost pos (crabs: int[]) =
        Array.map (fun c -> moveToPos (abs (c - pos)) 0 0) crabs
        |> Array.fold (+) 0

    let mean is =
        (Array.fold (+) 0 is) / is.Length

    let rec searchUpwards m is =
        match Array.contains m is with
        | true  -> m
        | false -> searchUpwards (m + 1) is

    let rec searchDownwards m is =
        match Array.contains m is with
        | true  -> m
        | false -> searchDownwards (m - 1) is

    let aroundMean is =
        let middle = searchUpwards (mean is) is
        ((searchDownwards (middle - 1) is), middle, (searchUpwards (middle + 1) is))

    let findCheapest is =
        let (a, b, c) = aroundMean is
        min (min (cost a is) (cost b is)) (cost c is)

    let secondTask (arg: string) =
        arg.Split ","
        |> Array.map int
        |> findCheapest
