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
    | Expression.Identifier basename -> Relation.Success(Relation.readCsv basename)
and  projectExpression (expr : ProjectExpression) =
  let (ident, cols) = expr
  let result = expression ident
  match result with
    | Relation.Success relation -> Relation.Success (Relation.project cols relation)
    | Relation.Failure _ -> result
and differenceExpression (binaryExpr: BinaryExpression) =
  let (left, right) = binaryExpr
  let leftResult = expression left
  let rightResult = expression right
  match (leftResult, rightResult) with
    | (Relation.Success l, Relation.Success r) -> Relation.difference l r
    | (Relation.Failure _, _) -> leftResult
    | _ -> rightResult

// 指定したRelationの内容を標準出力する
let printStatement ident =
  let df =
    Relation.readCsv ident
    |> Relation.toFrame
  df.Print()
  Nothing
// データベース内のRelationの一覧を標準出力する
let listStatement() =
  printfn "%s" (Relation.list() |> String.concat Environment.NewLine)
  Nothing

// 指定した名称で式を評価して得たRelationを保存する
let assignmentStatement (assign: Assignment) =
  let (ident, expr) = assign
  let result = expression expr
  match result with
    | Relation.Success relation ->
      Relation.save relation  ident
      Creation ident
    | Relation.Failure errMsg -> Failure errMsg

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
