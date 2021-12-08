module Day6
    let rec initialize (fish: uint64[]) (input: int list) =
        match input with
        | []    -> fish
        | x::xs ->
            fish.[x] <- fish.[x] + (uint64 1)
            initialize fish xs

    let dayPass (fish: uint64[]) =
        let respawn = fish.[0]
        fish.[0] <- fish.[1]
        fish.[1] <- fish.[2]
        fish.[2] <- fish.[3]
        fish.[3] <- fish.[4]
        fish.[4] <- fish.[5]
        fish.[5] <- fish.[6]
        fish.[6] <- fish.[7] + respawn
        fish.[7] <- fish.[8]
        fish.[8] <- respawn
        fish

    let rec daysPass days fish =
        match days with
        | 0 -> fish
        | _ -> daysPass (days - 1) (dayPass fish)

    let countFish (fish: uint64[]) =
        Array.fold (+) (uint64 0) fish

    let runSpawning (arg: string) days =
        arg.Split ","
        |> Array.toList
        |> (List.map int)
        |> (initialize (Array.zeroCreate 9))
        |> (daysPass days)
        |> countFish

    let firstTask (arg: string) =
        runSpawning arg 80

    let secondTask (arg: string) =
        runSpawning arg 256