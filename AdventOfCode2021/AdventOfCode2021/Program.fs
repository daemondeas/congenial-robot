module Program

let day1 argv =
    let args = Array.map int argv
    Day1.secondTask (Array.toList args)

let day2 argv =
    Day2.secondTask argv

let day3 (argv: string[]) =
    Array.map Seq.toArray argv
    |> Day3.secondTask

let day4 (arg: string) =
    let args = arg.Split "\n\n" |> Array.toList
    Day4.secondTask args

[<EntryPoint>]
let main args =
    printfn "%d" (day4 Day4Input.input)
    0
