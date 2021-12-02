module Day2
    type Direction =
        | Forward
        | Down
        | Up

    type Instruction =
        { Direction : Direction
          Distance  : int }

    let createDirection d =
        match d with
        | "forward" -> Forward
        | "down"    -> Down
        | "up"      -> Up

    let createInstruction (instr: string[]) =
        {
            Direction = createDirection (instr.[0])
            Distance  = int (instr.[1])
        }

    let rec travel hor dep instructions =
        match instructions with
        | []    -> (hor, dep)
        | x::xs ->
            match x.Direction with
            | Forward -> travel (hor + x.Distance) dep xs
            | Down    -> travel hor (dep + x.Distance) xs
            | Up      -> travel hor (dep - x.Distance) xs

    let firstTask (args: string[]) =
        Array.chunkBySize 2 args
        |> Array.map createInstruction
        |> Array.toList
        |> travel 0 0
        |> fun (x, y) -> x * y

    let rec travelWithAim hor dep aim instructions =
        match instructions with
        | []    -> (hor, dep)
        | x::xs ->
            match x.Direction with
            | Forward -> travelWithAim (hor + x.Distance) (dep + aim * x.Distance) aim xs
            | Down    -> travelWithAim hor dep (aim + x.Distance) xs
            | Up      -> travelWithAim hor dep (aim - x.Distance) xs

    let secondTask (args: string[]) =
        Array.chunkBySize 2 args
        |> Array.map createInstruction
        |> Array.toList
        |> travelWithAim 0 0 0
        |> fun (x, y) -> x * y