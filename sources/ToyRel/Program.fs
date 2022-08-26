// For more information see https://aka.ms/fsharp-console-apps
open RadLine
open Parser
open FParsec
open Eval
open System


// コードをparseして得たStatementを評価する
let runStatement src =
  match (run pStatement src) with
    | Success(stmt, _, _) ->
      match (evalStatement stmt) with
        | Some (Common.Identifier relationName) ->
          printfn "Relation %s returned." relationName
        | None -> ()        
    | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

let lineEditor = LineEditor()
lineEditor.KeyBindings.Add<PreviousHistoryCommand>(ConsoleKey.P, ConsoleModifiers.Control)
lineEditor.KeyBindings.Add<NextHistoryCommand>(ConsoleKey.N, ConsoleModifiers.Control)
lineEditor.Prompt <- LineEditorPrompt(">", ".")

let rec repl() =
  let line = lineEditor.ReadLine(System.Threading.CancellationToken.None).Result
  match (run pQuit line) with
    | Success _ -> ()
    | Failure _ ->
      runStatement line
      repl()

[<EntryPoint>]
let main _ = 
  repl()
  0
