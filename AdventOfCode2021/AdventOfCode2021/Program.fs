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

let day6 arg =
    Day6.secondTask arg

let day7 arg =
    Day7.secondTask arg

let day8 arg =
    Day8.secondTask arg

[<EntryPoint>]
let main args =
    printfn "%d" (day8 Day8Input.input)
    0
