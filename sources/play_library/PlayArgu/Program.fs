// For more information see https://aka.ms/fsharp-console-apps
open Argu

// 課題7
[<CliPrefix(CliPrefix.Dash)>]
type Arguments =
    | Hello

    interface IArgParserTemplate with
      member s.Usage =
        match s with
        | Hello -> "print Hello World"

[<EntryPoint>]
let main args =
  let parser = ArgumentParser.Create<Arguments>(programName="課題7")
  try
    let rs = parser.Parse args
    if rs.Contains Hello then
      printfn "Hello World"
    else
      printfn "I don't know" 
  with
    | :? ArguParseException as ex ->
      if ex.ErrorCode = ErrorCode.HelpText then
        printfn "%s" (parser.PrintUsage())
      else
        printfn "I don't know"
    | ex -> printfn "%s" ex.Message
  0
