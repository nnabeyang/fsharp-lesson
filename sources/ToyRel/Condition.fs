module Condition
open Common
open Deedle
open MyResult
open Relation

let compare op left right =
  let (RowFunc l) = left
  let (RowFunc r) = right
  let lf = fun row -> l row
  let rf = fun row -> r row
  fun (row: ObjectSeries<string>) ->
      match op with
        | Eq -> (lf row) = (rf row)
        | Ne -> (lf row) <> (rf row)
        | Lt -> (lf row) <  (rf row)
        | Gt -> (lf row) > (rf row)
        | Le -> (lf row) <= (rf row)
        | Ge -> (lf row) >= (rf row)
    |> BoolLiteral
  |> RowFunc
  |> MyResult.Ok

let rec condition rel cond =
  match cond with
    | Value v -> value rel v
    | Function func ->
      match func with
        | Comparison (left, op, right) -> comparison rel op left right
        | Logical (left, op, right) -> logical rel op left right
and value rel expr =
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
and logical rel op left right = MyResult.result {
  let! l = condition rel left
  let! r = condition rel right
  let lf = filter l
  let rf = filter r
  let! f = fun (row: ObjectSeries<string>) ->
               match op with
                  | And ->  (lf row) && (rf row)
                  | Or -> (lf row) || (rf row)
                |> BoolLiteral
           |> RowFunc
           |> MyResult.Ok
  return f
}
