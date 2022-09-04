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

let compare<'T when 'T: comparison> (op: ComparisonOp) (left: Filter) (right: Filter) =
  let (Filter l) = left
  let (Filter r) = right
  let lf = fun row -> (l row) :?> 'T
  let rf = fun row -> (r row) :?> 'T
  match op with
    | Eq -> fun (row: ObjectSeries<string>) -> ((lf row) = (rf row)) :> obj
    | Ne -> fun (row: ObjectSeries<string>) -> ((lf row) <> (rf row)) :> obj
    | Lt -> fun (row: ObjectSeries<string>) -> ((lf row) <  (rf row)) :> obj
    | Gt -> fun (row: ObjectSeries<string>) -> ((lf row) > (rf row)) :> obj
    | Le -> fun (row: ObjectSeries<string>) -> (lf row) <= (rf row) :> obj
    | Ge -> fun (row: ObjectSeries<string>) -> ((lf row) >= (rf row)) :> obj
  |> Filter
  |> MyResult.Ok

let rec condition  (rel: Relation.T) (cond: ConditionalExpression) =
  match cond with
    | Value v -> value v
    | Function func ->
      match func with
        | Comparison (left, op, right) -> comparison rel op left right
        | Logical (left, op, right) -> logical rel op left right
and value expr =
  try
    match expr with
      | Literal v ->
        fun (_: ObjectSeries<string>) -> v
        |> Filter
        |> MyResult.Ok
      | ColumnName name -> MyResult.result {
        let! f =
          (fun (row: ObjectSeries<string>)-> row.Get(name))
          |> Filter
          |> MyResult.Ok
        return f
       }
  with
    | :? System.InvalidCastException as ex -> MyResult.Error (TypeError ex.Message)
and comparison (rel: Relation.T) (op: ComparisonOp) (left: ConditionalExpression) (right: ConditionalExpression) = MyResult.result {
  let! lType = getType rel left
  let! rType = getType rel right
  let! ff =
    if lType <> rType then
      MyResult.Error (TypeError (sprintf "Type mismatch in conditional expression: %A <=> %A" lType rType))
    else MyResult.result {
      let! l = condition rel left
      let! r = condition rel right
      let! f = match lType with
                | p when p = typeof<int> -> compare<int> op l r
                | p when p = typeof<string> -> compare<string> op l r
                | p when p = typeof<bool> -> compare<bool> op l r
                | p -> MyResult.Error (TypeError (sprintf "%A is not supported" p))
      return f
    }
  return ff
}
and logical (rel: Relation.T) (op: LogicalOp) (left: ConditionalExpression) (right: ConditionalExpression) = MyResult.result {
  let! (Filter l) = condition rel left
  let! (Filter r) = condition rel right
  let lf = fun row -> (l row) :?> bool
  let rf = fun row -> (r row) :?> bool
  let! f =  match op with
              | And -> fun (row: ObjectSeries<string>) -> ((lf row) && (rf row)) :> obj
              | Or -> fun (row: ObjectSeries<string>) -> ((lf row) || (rf row)) :> obj
            |> Filter
            |> MyResult.Ok
  return f
}
