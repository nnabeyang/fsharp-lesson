module Condition
open Common
open Deedle
open MyResult
open Relation

let compare op (left: ObjectSeries<string> -> bool) (right: ObjectSeries<string> -> bool) =
  fun (row: ObjectSeries<string>) ->
    match op with
      | ComparisonOp cop ->
        match cop with
          | Eq -> (left row) = (right row)
          | Ne -> (left row) <> (right row)
          | Lt -> (left row) <  (right row)
          | Gt -> (left row) > (right row)
          | Le -> (left row) <= (right row)
          | Ge -> (left row) >= (right row)
      | LogicalOp lop ->
        match lop with
          | And ->  (left row) && (right row)
          | Or -> (left row) || (right row)
    |> BoolLiteral

let rec condition rel cond =
  match cond with
    | Value v -> unaryExpr rel v
    | Function func ->
      match func with
        | Comparison (left, op, right) -> comparison rel op left right
        | Logical (left, op, right) -> binaryExpr rel op left right
and unaryExpr rel expr =
  try
    match expr with
      | Literal v ->
        fun (_: ObjectSeries<string>) -> v
        |> RowFunc
        |> MyResult.Ok
      | ColumnName name -> MyResult.result {
        let! t = Relation.getTypeByColName rel name
        let! f = match t with
                  | p when p = typeof<string> -> fun (row: ObjectSeries<string>) -> (row.GetAs<string>(name) |> StrLiteral)
                  | p when p = typeof<int> -> fun (row: ObjectSeries<string>) -> (row.GetAs<int>(name) |> IntLiteral)
                  | p when p = typeof<bool> -> fun (row: ObjectSeries<string>) -> (row.GetAs<bool>(name) |> BoolLiteral)
                  | p -> raise (ToyRelTypeException (sprintf "%A is not supported" p))
                |> RowFunc
                |> MyResult.Ok
        return f
       }
  with
    | ToyRelTypeException errorMsg -> MyResult.Error (TypeError errorMsg)
and binaryExpr rel op left right = MyResult.result {
  let! l = condition rel left
  let! r = condition rel right
  let lf = filter l
  let rf = filter r
  let! f =
    compare op lf rf
    |> RowFunc
    |> MyResult.Ok
  return f
}
and comparison rel op left right = MyResult.result {
  let! lType = Relation.getType rel left
  let! rType = Relation.getType rel right
  let! f =
    if lType <> rType then
      MyResult.Error (TypeError (sprintf "Type mismatch in conditional expression: %A <=> %A" lType rType))
    else
      binaryExpr rel op left right
  return f
}
