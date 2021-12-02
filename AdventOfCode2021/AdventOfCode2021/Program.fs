module Program

let day1 argv =
    let args = Array.map int argv
    Day1.secondTask (Array.toList args)

let day2 argv =
    Day2.secondTask argv

[<EntryPoint>]
let main args =
    printfn "%d" (day2 args)
    0
