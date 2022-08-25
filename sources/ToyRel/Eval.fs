module Eval
open Common
open Deedle
open System
open System.IO
open Relation

let random = Random()
let randomString length =
  String.init length (fun _ -> char (random.Next( (int 'a'), (int 'z') + 1)) |> sprintf "%c")
let randomBaseName() = "zz" + (randomString 4) |> Identifier.Identifier

let rec expression expr =
  match expr with
    | Project cols -> projectExpression cols
    | Expression.Identifier basename -> Relation.readCsv basename
and  projectExpression (expr : ProjectExpression) =
  let (ident, cols) = expr
  let df = match ident with
    | Project _ -> expression ident
    | _ -> expression ident
  Relation.project cols df

// 指定したRelationの内容を標準出力する
let printStatement ident =
  let df =
    Relation.readCsv ident
    |> Relation.toFrame
  df.Print()

// データベース内のRelationの一覧を標準出力する
let listStatement() =
  let relationList = 
    Directory.GetFiles databaseDir
    |> Seq.map Path.GetFileNameWithoutExtension
  printfn "%s" (relationList  |> String.concat Environment.NewLine)

// 指定した名称で式を評価して得たRelationを保存する
let assignmentStatement (assign: Assignment) =
  let (ident, expr) = assign
  Relation.save (expression expr) ident
  ident

// ランダムな名称で式を評価して得たRelationを保存する
let expressionStatement expr = assignmentStatement (randomBaseName(), expr)

// Statementを評価する
let evalStatement stmt =
  match stmt with
    | PrintStatement ident ->
      printStatement ident
      None
    | ExpressionStatement expr ->
      Some (expressionStatement expr)
    | AssignmentStatement assignment -> 
      Some (assignmentStatement assignment)
    | ListStatement ->
      listStatement()
      None
