module Condition
open Common
open Deedle
open MyResult
open Relation

let apply (func: RowFunc) (row: ObjectSeries<string>) =
  match func with
    | Filter f -> (f row) |> BoolLiteral
    | ColFunc f -> (f row)
let cmp op (left: RowFunc) (right: RowFunc) =
  fun (row: ObjectSeries<string>) ->
    match op with
      | Eq -> (apply left row) = (apply right row)
      | Ne -> (apply left row) <> (apply right row)
      | Lt -> (apply left row) <  (apply right row)
      | Gt -> (apply left row) > (apply right row)
      | Le -> (apply left row) <= (apply right row)
      | Ge -> (apply left row) >= (apply right row)
let cmpl op (left: Filter) (right: Filter) =
  fun (row: ObjectSeries<string>) ->
    match op with
      | And ->  (left row) && (right row)
      | Or -> (left row) || (right row)

let compare (op: BinaryOp) (left: RowFunc) (right: RowFunc) =
  match op with
    | ComparisonOp cop ->
      cmp cop left right
      |> Filter
      |> MyResult.Ok
    | LogicalOp lop ->
      match (left, right) with
        | (Filter lf, Filter rf) ->
          (cmpl lop lf rf)
          |> Filter
          |> MyResult.Ok
        | (_, _) -> MyResult.Error (TypeError "non-boolean value is cannot be computed with logical operators.")

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
        | BoolLiteral value -> (fun (_: ObjectSeries<string>) -> value) |> Filter
        | _ -> (fun (_: ObjectSeries<string>) -> v) |> ColFunc
      |> MyResult.Ok
    | ColumnName name -> MyResult.result {
      let! t = Relation.getTypeByColName rel name
      let! f = match t with
                | StrType -> (fun (row: ObjectSeries<string>) -> (row.GetAs<string>(name)) |> StrLiteral) |> ColFunc
                | IntType -> (fun (row: ObjectSeries<string>) -> (row.GetAs<int>(name)) |> IntLiteral) |> ColFunc
                | BoolType -> (fun (row: ObjectSeries<string>) -> (row.GetAs<bool>(name))) |> Filter
              |> MyResult.Ok
      return f
      }
and logical rel op left right = MyResult.result {
  let! l = condition rel left
  let! r = condition rel right
  let! f = compare op l r
  return f
}
and comparison rel op left right = MyResult.result {
  let! lType = Relation.getType rel left
  let! rType = Relation.getType rel right
  let! ff =
    if lType <> rType then
      MyResult.Error (TypeError (sprintf "Type mismatch in conditional expression: %A <=> %A" lType rType))
    else MyResult.result {
      let! l = condition rel left
      let! r = condition rel right
      let! f = compare op l r
      return f
    }
  return ff
}
