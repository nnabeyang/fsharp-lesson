module Condition
open Common
open Deedle
open MyResult
open Relation

let getTypeByColName rel name =
  let df = Relation.toFrame rel
  let types = df.ColumnTypes |> Seq.toList
  try
    let idx = df.ColumnKeys |> Seq.findIndex (fun key -> key = name)
    types[idx] |> MyResult.Ok
  with
    | :? System.Collections.Generic.KeyNotFoundException -> MyResult.Error (EvalError "column name is wrong")

let getType rel cond =
  match cond with
    | Value value ->
      match value with
        | Literal literal ->
          match literal with
            | StrLiteral _ -> typeof<string>
            | IntLiteral _ -> typeof<int>
            | BoolLiteral _ -> typeof<bool>
          |> MyResult.Ok 
        | ColumnName name -> getTypeByColName rel name
    | Function _ -> typeof<bool> |> MyResult.Ok

let compare op left right =
  let (RowFunc l) = left
  let (RowFunc r) = right
  let lf = fun row -> l row
  let rf = fun row -> r row
  match op with
    | Eq -> fun (row: ObjectSeries<string>) -> ((lf row) = (rf row)) |> BoolLiteral
    | Ne -> fun (row: ObjectSeries<string>) -> ((lf row) <> (rf row)) |> BoolLiteral
    | Lt -> fun (row: ObjectSeries<string>) -> ((lf row) <  (rf row)) |> BoolLiteral
    | Gt -> fun (row: ObjectSeries<string>) -> ((lf row) > (rf row)) |> BoolLiteral
    | Le -> fun (row: ObjectSeries<string>) -> (lf row) <= (rf row) |> BoolLiteral
    | Ge -> fun (row: ObjectSeries<string>) -> ((lf row) >= (rf row)) |> BoolLiteral
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
        let! t = getTypeByColName rel name
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
  let! lType = getType rel left
  let! rType = getType rel right
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
  let! f =  match op with
              | And -> fun (row: ObjectSeries<string>) -> ((lf row) && (rf row)) |> BoolLiteral
              | Or -> fun (row: ObjectSeries<string>) -> ((lf row) || (rf row)) |> BoolLiteral
            |> RowFunc
            |> MyResult.Ok
  return f
}
