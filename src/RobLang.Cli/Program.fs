module RobLang.Cli.Main

open RobLang.Parser.Core

[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        try
            System.IO.File.ReadAllText file
            |> parse
            |> fun _ -> printfn "Parsed program"
            0
        with
          | err ->
            eprintfn $"Exception: {err}"
            1
    | _ ->
        eprintfn "Usage: roblang <file>"
        1
