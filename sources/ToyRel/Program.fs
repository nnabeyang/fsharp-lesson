// For more information see https://aka.ms/fsharp-console-apps
open Parser
open FParsec
open Eval

// コードをparseして得たStatementを評価する
let run src =
  match (run pStatement src) with
    | Success(stmt, _, _) ->
      match (evalStatement stmt) with
        | Some (Common.Identifier relationName) ->
          printfn "Relation %s returned." relationName
        | None -> ()        
    | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

run "list"
run "abc=(project (project (シラバス) 専門, 学年, 場所) 専門, 学年)"
run "hoge=(シラバス)"
run "print hoge"
run "print abc"
run "list"
