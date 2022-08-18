// For more information see https://aka.ms/fsharp-console-apps
open Argu
open Deedle

// 課題8
[<CliPrefix(CliPrefix.Dash)>]
type Arguments =
    | Filter of col:string * value:string
    | Project of cols:string list

    interface IArgParserTemplate with
      member s.Usage =
        match s with
        | Filter _ -> "filter data frame"
        | Project _ -> "project data frame"

let filter (pred: ObjectSeries<string> -> bool) (df: Frame<int,string>) = 
  df.RowsDense
  |> Series.filterValues pred
  |> Frame.ofRows

let project (cols: list<string>) (df: Frame<int,string>) = df.Columns.[ cols ] 

[<EntryPoint>]
let main args =
  let df = Frame.ReadCsv "../../data/シラバス.csv"
  let parser = ArgumentParser.Create<Arguments>(programName="課題8")
  try
    match parser.Parse args with
      | p when p.Contains Filter ->
        let (col, value) = p.GetResult Filter
        (filter (fun row -> row.Get(col) = value) df).Print()
      | p when p.Contains Project -> 
         let cols = p.GetResult Project
         (project cols df).Print()
      | _ -> printfn "I don't know"
  with
    | :? ArguParseException as ex ->
      if ex.ErrorCode = ErrorCode.HelpText then
        printfn "%s" (parser.PrintUsage())
      else
        printfn "I don't know"
    | ex -> printfn "%s" ex.Message
  0
