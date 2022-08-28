module Eval
open Common
open Deedle
open System
open Relation

type Result =
  | Creation of Identifier
  | Nothing
  | Failure of string

let random = Random()
let randomString length =
  String.init length (fun _ -> char (random.Next( (int 'a'), (int 'z') + 1)) |> sprintf "%c")
let randomBaseName() = "zz" + (randomString 4) |> Identifier.Identifier

let rec expression expr =
  match expr with
    | Project cols -> projectExpression cols
    | Difference binaryExpr -> differenceExpression binaryExpr
    | Identifier basename -> Relation.load basename
and  projectExpression (expr : ProjectExpression) =
  let (ident, cols) = expr
  let result = expression ident
  match result with
    | Result.Ok relation -> Relation.project cols relation
    | Result.Error _ -> result
and differenceExpression (binaryExpr: BinaryExpression) =
  let (left, right) = binaryExpr
  Relation.difference (expression left) (expression right)

// 指定したRelationの内容を標準出力する
let printStatement ident =
  Relation.print ident
  Nothing
// データベース内のRelationの一覧を標準出力する
let listStatement() =
  printfn "%s" (Relation.list() |> String.concat Environment.NewLine)
  Nothing

// 指定した名称で式を評価して得たRelationを保存する
let assignmentStatement (assign: Assignment) =
  let (ident, expr) = assign
  match (expression expr) with
    | Result.Ok relation ->
      Relation.save relation  ident
      Creation ident
    | Result.Error (Relation.DifferenceError errorMsg) -> Failure errorMsg

// ランダムな名称で式を評価して得たRelationを保存する
let expressionStatement expr = assignmentStatement (randomBaseName(), expr)

// 使用するDatabaseの切り替え
let useStatement newValue =
  database.Value <- newValue
  Nothing
  
// Statementを評価する
let evalStatement stmt =
  match stmt with
    | PrintStatement ident -> printStatement ident
    | ExpressionStatement expr -> expressionStatement expr
    | AssignmentStatement assignment -> assignmentStatement assignment
    | ListStatement -> listStatement()
    | UseStatement database -> useStatement database
