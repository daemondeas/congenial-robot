module Program

let day1 argv =
    let args = Array.map int argv
    Day1.secondTask (Array.toList args)

let day2 argv =
    Day2.secondTask argv

let day3 (argv: string[]) =
    Array.map Seq.toArray argv
    |> Day3.secondTask

[<EntryPoint>]
let main args =
    printfn "%d" (day3 args)
    0
