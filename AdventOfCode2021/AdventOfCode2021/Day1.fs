module Day1
    let rec deeper x xs amount =
        match xs with
        | []    -> amount
        | y::ys ->
            if y > x then
                deeper y ys (amount + 1)
            else
                deeper y ys amount

    let firstTask args = deeper System.Int32.MaxValue args 0