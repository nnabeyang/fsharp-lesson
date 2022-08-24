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

let rec evalExpression expr =
  match expr with
    | Project cols -> evalProjectExpression cols
    | Expression.Identifier basename -> Relation.readCsv basename
and  evalProjectExpression (expr : ProjectExpression) =
  let (ident, cols) = expr
  let df = match ident with
    | Project _ -> evalExpression ident
    | _ -> evalExpression ident
  Relation.project cols df

// 指定したRelationの内容を標準出力する
let evalPrint ident =
  let df =
    Relation.readCsv ident
    |> Relation.toFrame
  df.Print()

// データベース内のRelationの一覧を標準出力する
let evalList() =
  let relationList = 
    Directory.GetFiles databaseDir
    |> Seq.map Path.GetFileNameWithoutExtension
  printfn "%s" (relationList  |> String.concat Environment.NewLine)

// 指定した名称で式を評価して得たRelationを保存する
let evalAssignment (assignment: Assignment) =
  let (ident, expr) = assignment
  let (Identifier.Identifier relationName) = ident
  Relation.save (evalExpression expr) ident
  printfn "Relation %s returned." relationName

// ランダムな名称で式を評価して得たRelationを保存する
let evalExprStatement expr = evalAssignment (randomBaseName(), expr)

// Statementを評価する
let evalStatement stmt =
  match stmt with
    | ExpressionStatement expr -> evalExprStatement expr
    | PrintStatement ident -> evalPrint ident
    | AssignmentStatement assignment -> evalAssignment assignment
    | ListStatement -> evalList()
