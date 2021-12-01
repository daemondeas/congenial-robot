module Program

let day1 argv =
    let args = Array.map int argv
    Day1.firstTask (Array.toList args)

[<EntryPoint>]
let main args =
    printfn "%d" (day1 args)
    0
