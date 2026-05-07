module RobLang.Cli.Main

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        RobLang.hello file
        0
    | _ ->
        eprintfn "Usage: roblang <file>"
        1
