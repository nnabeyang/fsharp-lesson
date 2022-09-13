module Condition
open Common
open Deedle
open MyResult
open Relation


let cmp (op: ComparisonOp) l r = fun row -> evalComp op (l row) (r row)
let cmpl op l r = fun row -> evalCompl op (l row) (r row)
let conditionalValue rel expr =
  match expr with
    | Literal v ->
      match v with
        | StrLiteral value -> (fun (_: ObjectSeries<string>) -> value |> StrValue)
        | IntLiteral value -> (fun (_: ObjectSeries<string>) -> value |> IntValue)
        | BoolLiteral value -> (fun (_: ObjectSeries<string>) -> value |> BoolValue)
      |> MyResult.Ok
    | ColumnName colName -> MyResult.result {
        let name = Relation.effectiveColumnName rel colName
        let! t = Relation.getTypeByColName rel colName
        let f = match t with
                  | StrType -> (fun (row: ObjectSeries<string>) -> (row.GetAs<string>(name)) |> StrValue)
                  | IntType -> (fun (row: ObjectSeries<string>) -> (row.GetAs<int>(name)) |> IntValue)
                  | DateTimeType -> (fun (row: ObjectSeries<string>) -> (row.GetAs<System.DateTime>(name)) |> DateTimeValue)
                  | BoolType -> (fun (row: ObjectSeries<string>) -> (row.GetAs<bool>(name)) |> BoolValue)
        return f
      }

let rec condition rel cond =
  match cond with
    | Value value -> singleValue rel value
    | Function func ->
      match func with
        | SimpleComparison (left, op, right) -> simple rel op left right
        | Comparison (left, op, right) -> comparison rel op left right
        | Logical (left, op, right) -> logical rel op left right
// リテラルだけカラム名だけの式は条件式として扱わない
and singleValue rel value =
  let valueTypeName = match value with
                      | Literal  _ -> "literal"
                      | ColumnName _ -> "column name"
  (sprintf "single %s is not an conditional expression." valueTypeName)
  |> EvalError
  |> MyResult.Error
and logical rel op left right = MyResult.result {
  let! l = condition rel left
  let! r = condition rel right
  let f = cmpl op l r
  return f
}
and simple rel op left right = MyResult.result {
    let! l = conditionalValue rel left
    let! r = conditionalValue rel right
    let! lType = Relation.getValueType rel left
    let! rType = Relation.getValueType rel right
    let! f =
      if lType <> rType then
        MyResult.Error (TypeError (sprintf "Type mismatch in conditional expression: %A <=> %A" lType rType))
      else
        (cmp op l r) |> MyResult.Ok
  return f
}
and comparison rel op left right = MyResult.result {
    let! l = condition rel left
    let! r = condition rel right
    let! lType = Relation.getType rel left
    let! rType = Relation.getType rel right
    let! f =
      if lType <> rType then
        MyResult.Error (TypeError (sprintf "Type mismatch in conditional expression: %A <=> %A" lType rType))
      else
        (cmp op l r) |> MyResult.Ok
  return f
}
