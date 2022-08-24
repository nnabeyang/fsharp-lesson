// For more information see https://aka.ms/fsharp-console-apps
open Parser
open Eval

// コードをparseして得たStatementを評価する
let run src = evalStatement (parse src)

run "list"
run "project (project (シラバス) 専門, 学年, 場所) 専門, 学年"
run "hoge=(シラバス)"
run "print hoge"
run "list"
