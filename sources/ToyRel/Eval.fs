module Eval
open Common
open System
open MyResult
open Relation
open Condition

type Result =
  | Creation of Identifier
  | Nothing
  | Failure of string

let print result =
  match result with
    | Creation (Common.Identifier relationName) ->
      printfn "Relation %s returned." relationName
    | Failure errorMsg -> printfn "Failure: %s" errorMsg        
    | Nothing -> ()

let random = Random()
let randomString length =
  String.init length (fun _ -> char (random.Next( (int 'a'), (int 'z') + 1)) |> sprintf "%c")
let randomBaseName() = "zz" + (randomString 4) |> Identifier.Identifier

// リレーションの名前を取得する。Expressionが式になっている場合は名前がないので"tmp"を返す
let relationName (expr: Expression) =
  match expr with
    | Identifier (Identifier.Identifier name) -> name
    | _ -> "tmp"

let rec expression expr =
  match expr with
    | Project cols -> projectExpression cols
    | Restrict (expr, cond) -> restrictExpression expr cond
    | Join (left, right, cond) -> joinExpression left right cond
    | Difference binaryExpr -> differenceExpression binaryExpr
    | Product binaryExpr -> productExpression binaryExpr
    | Union binaryExpr -> unionExpression binaryExpr
    | Intersect binaryExpr -> intersectExpression binaryExpr
    | Identifier basename -> Relation.load basename
and  projectExpression (expr : ProjectExpression) =
  let (ident, cols) = expr
  expression ident |> MyResult.bind (Relation.project cols)
and differenceExpression (binaryExpr: BinaryExpression) =
  let (left, right) = binaryExpr
  Relation.difference (expression left) (expression right)
and productExpression (binaryExpr: BinaryExpression) =
  let (left, right) = binaryExpr
  let rName = relationName right
  Relation.product (expression left) (expression right) rName
and unionExpression (binaryExpr : BinaryExpression) =
  let (left, right) = binaryExpr
  Relation.union (expression left) (expression right)
and intersectExpression (binaryExpr : BinaryExpression) =
  let (left, right) = binaryExpr
  Relation.intersect (expression left) (expression right)
and restrictExpression expr cond =
  match (expression expr) with
    | MyResult.Ok rel -> conditionalExpression rel cond
    | MyResult.Error e -> MyResult.Error e
and joinExpression (left: Expression) (right: Expression) (cond: ConditionalExpression) = MyResult.result {
  let! prod = productExpression (left, right)
  let! result = conditionalExpression prod cond
  return result
}
and conditionalExpression (rel: Relation.T) cond =
  match (Condition.condition rel cond) with
    | MyResult.Ok f -> Relation.restrict rel f
    | MyResult.Error e -> MyResult.Error e

// 指定したRelationの内容を標準出力する
let printStatement ident =
  Relation.print ident
  Nothing
let renameStatement ident colName newColName =
  let rel = Relation.rename ident colName newColName
  let ident = randomBaseName()
  Relation.save rel ident
  Creation ident
// データベース内のRelationの一覧を標準出力する
let listStatement() =
  printfn "%s" (Relation.list() |> String.concat Environment.NewLine)
  Nothing

// 指定した名称で式を評価して得たRelationを保存する
let assignmentStatement (assign: Assignment) =
  let (ident, expr) = assign
  match (expression expr) with
    | MyResult.Ok relation ->
      Relation.save relation  ident
      Creation ident
    | MyResult.Error err -> 
      match err with
        | TypeError errorMsg -> Failure (sprintf "TypeError: %s" errorMsg)
        | EvalError errorMsg -> Failure (sprintf "EvalError: %s" errorMsg)

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
    | Rename (ident, colName, newColName) -> renameStatement ident colName newColName
    | ExpressionStatement expr -> expressionStatement expr
    | AssignmentStatement assignment -> assignmentStatement assignment
    | ListStatement -> listStatement()
    | UseStatement database -> useStatement database
