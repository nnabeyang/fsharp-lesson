module Condition
open Common
open Deedle
open MyResult
open Relation


let cmp (op: ComparisonOp) l r = fun row -> evalComp op (l row) (r row)
let cmpl op l r = fun row -> evalCompl op (l row) (r row)

let rec condition rel cond =
  match cond with
    | Value v -> unary rel v
    | Function func ->
      match func with
        | Comparison (left, op, right) -> comparison rel op left right
        | Logical (left, op, right) -> logical rel op left right
and unary rel expr =
  match expr with
    | Literal v ->
      match v with
        | StrLiteral value -> (fun (_: ObjectSeries<string>) -> value |> StrValue) |> ColFunc
        | IntLiteral value -> (fun (_: ObjectSeries<string>) -> value |> IntValue) |> ColFunc
        | BoolLiteral value -> (fun (_: ObjectSeries<string>) -> value) |> Filter
      |> MyResult.Ok
    | ColumnName colName -> MyResult.result {
        let name = Relation.columnName rel colName
        let! t = Relation.getTypeByColName rel colName
        let! f = match t with
                  | StrType -> (fun (row: ObjectSeries<string>) -> (row.GetAs<string>(name)) |> StrValue) |> ColFunc
                  | IntType -> (fun (row: ObjectSeries<string>) -> (row.GetAs<int>(name)) |> IntValue) |> ColFunc
                  | DateTimeType -> (fun (row: ObjectSeries<string>) -> (row.GetAs<System.DateTime>(name)) |> DateTimeValue) |> ColFunc
                  | BoolType -> (fun (row: ObjectSeries<string>) -> (row.GetAs<bool>(name))) |> Filter
                |> MyResult.Ok
        return f
      }
and logical rel op left right = MyResult.result {
  let! l = condition rel left
  let! r = condition rel right
  let! f =
    match (l, r) with
      | (Filter lf, Filter rf) -> (cmpl op lf rf) |> MyResult.Ok
      | (_, _) -> MyResult.Error (TypeError "non-boolean value is cannot be computed with logical operators.")
  return (Filter f)
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
      match (l, r) with
        | (Filter lf, Filter rf) -> (cmp op lf rf) |> MyResult.Ok
        | (ColFunc lf, ColFunc rf) -> (cmp op lf rf) |> MyResult.Ok
        | (_, _) ->
          // lTypeとrTypeが等しいときは両方ともFilterかColFuncになる
          MyResult.Error (TypeError "no reach")
  return (Filter f)
}
