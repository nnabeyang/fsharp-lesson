module Condition
open Common
open Deedle
open MyResult
open Relation

let getType rel cond =
  match cond with
    | Value value ->
      match value with
        | Literal literal -> literal.GetType() |> MyResult.Ok
        | ColumnName name ->        
          let df = Relation.toFrame rel
          let types = df.ColumnTypes |> Seq.toList
          try
            let idx = df.ColumnKeys |> Seq.findIndex (fun key -> key = name)
            types[idx] |> MyResult.Ok
          with
            | :? System.Collections.Generic.KeyNotFoundException -> MyResult.Error (EvalError "column name is wrong")
    | Function _ -> typeof<bool> |> MyResult.Ok

let value<'T when 'T: comparison> expr =
  try
    match expr with
      | Literal literal ->
        let v = literal :?> 'T
        fun (_: ObjectSeries<string>) -> v
        |> MyResult.Ok
        | ColumnName name -> (fun (row: ObjectSeries<string>)-> row.GetAs<'T>(name)) |> MyResult.Ok
  with
    | :? System.InvalidCastException as ex -> MyResult.Error (TypeError ex.Message)

let rowFunc<'T when 'T: comparison> expr =
  // boolの場合はrowFuncBoolが呼ばれる
  assert (typeof<'T> <> typeof<bool>)
  match expr with
      | Value v -> value<'T> v
      | Function _ -> MyResult.Error (EvalError "no reach")

let compare<'T when 'T: comparison> op (left: ObjectSeries<string> -> 'T) (right: ObjectSeries<string> -> 'T) =
  match op with
    | Eq -> fun (row: ObjectSeries<string>) -> (left row) = (right row)
    | Ne -> fun (row: ObjectSeries<string>) -> (left row) <> (right row)
    | Lt -> fun (row: ObjectSeries<string>) -> (left row) <  (right row)
    | Gt -> fun (row: ObjectSeries<string>) -> (left row) > (right row)
    | Le -> fun (row: ObjectSeries<string>) -> (left row) <= (right row)
    | Ge -> fun (row: ObjectSeries<string>) -> (left row) >= (right row)
  |> Filter
  |> MyResult.Ok

let _comparison<'T when 'T: comparison> (op: ComparisonOp) (left: ConditionalExpression) (right: ConditionalExpression) = MyResult.result {
  let! l = rowFunc<'T> left
  let! r = rowFunc<'T> right
  let! f = compare<'T> op l r
  return f
}

let rec condition (rel: Relation.T) (cond: ConditionalExpression) =
  match cond with
    | Value (Literal objVal) -> literal objVal
    | Function func ->
      match func with
        | Comparison (left, op, right) -> comparison rel op left right
        | Logical (left, op, right) -> logical rel op left right
    | Value (ColumnName _) -> MyResult.Error (EvalError "column name is not a conditional expression.")
and literal (objVal: obj) =
  match objVal.GetType() with
    | p when p = typeof<bool> ->
      let boolVal = objVal :?> bool
      fun (_: ObjectSeries<string>) -> boolVal
      |> Filter
      |> MyResult.Ok
    | p -> MyResult.Error (EvalError (sprintf "%A value is not a conditional expression" p))
and comparison (rel: Relation.T) (op: ComparisonOp) (left: ConditionalExpression) (right: ConditionalExpression) = MyResult.result {
  let! lType = getType rel left
  let! rType = getType rel right
  let! f =
    if lType <> rType then
      MyResult.Error (TypeError (sprintf "Type mismatch in conditional expression: %A <=> %A" lType rType))
    else
      match lType with
        | p when p = typeof<bool> -> MyResult.result {
          let! l = rowFuncBool rel left
          let! r = rowFuncBool rel right
          let! f = compare<bool>  op l r
          return f
         }
        | p when p = typeof<int> -> _comparison<int> op left right
        | p when p = typeof<string> -> _comparison<string>  op left right
        | p  -> MyResult.Error (TypeError (sprintf "%A is not supported" p))
  return f
}
and rowFuncBool (rel: Relation.T) (expr: ConditionalExpression) =
  match expr with
    | Function _ ->
      match condition rel expr with
        | MyResult.Ok (Filter f) ->
          (fun x -> f x)
          |> MyResult.Ok
        | MyResult.Error e -> MyResult.Error e
    | Value v  -> value<bool> v
and logical (rel: Relation.T) (op: LogicalOp) (left: ConditionalExpression) (right: ConditionalExpression) = MyResult.result {
  let! (Filter l) = condition rel left
  let! (Filter r) = condition rel right
  let! f =  match op with
              | And -> fun (row: ObjectSeries<string>) -> (l row) && (r row)
              | Or -> fun (row: ObjectSeries<string>) -> (l row) || (r row)
            |> Filter
            |> MyResult.Ok
  return f
}
