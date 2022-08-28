module Relation
open Common
open Deedle
open System.IO
open System.Collections.Generic

module Relation = 
  type T = Relation of Frame<int, string>
  type DifferenceError = DifferenceError of string 
  // Frameから重複を除いてRelationを作る
  let distinct (df: Frame<int, string>) =
      df.Rows.Values 
      |> Seq.toList
      |> Seq.distinct 
      |> Series.ofValues 
      |> Frame.ofRows
      |> Relation
  
  let toFrame (Relation df) = df
  
  let readCsv ident =
    Frame.ReadCsv (databasePath database.Value ident)
    |> distinct
  
  // Relationをロードする
  let load ident = readCsv ident |> Result.Ok
  
  // Relationを保存する。名前が衝突した場合は上書き保存する。  
  let save relation ident =
    let df = toFrame relation
    df.SaveCsv (databasePath database.Value ident)
  
  // Relationからcolsで指定したカラムだけ残したRelationを新たに作る
  let project (cols: list<string>) relation =
    let df = toFrame relation
    df.Columns.[ cols ] |> distinct |> Result.Ok

  // 現在のDatabaseのRelation名一覧を返す
  let list() =
    grab (databasePath database.Value (Identifier.Identifier "*"))
    |> Seq.map Path.GetFileNameWithoutExtension
  
  // 指定したRelationの内容を標準出力する
  let print ident =
    let df = readCsv ident |> toFrame
    df.Print()

  let checkColTypes (Relation df1) (Relation df2) =
    let cts1 = df1.ColumnTypes |> Seq.toList
    let cts2 = df2.ColumnTypes |> Seq.toList
    cts1 = cts2
  let checkColKeyNames (Relation df1) (Relation df2) =
    let keys1 = df1.ColumnKeys |> Seq.toList
    let keys2 = df2.ColumnKeys |> Seq.toList
    keys1 = keys2
  
  
  let takeDifference rel1 rel2 =
    if not (checkColTypes rel1 rel2) then
      Result.Error (DifferenceError "column types do not match")
    else if not (checkColKeyNames rel1 rel2) then
      Result.Error (DifferenceError "column names do not match")
    else
      let (Relation df2) = rel2
      let (Relation df1) = rel1
      let valueSet = df2.Rows.Values |> Seq.toList<ObjectSeries<string>> |> HashSet
      df1.Rows.Values
        |> Seq.filter (fun s -> not (valueSet.Contains s))
        |> Series.ofValues
        |> Frame.ofRows
        |> Relation
        |> Result.Ok

  let difference (left: Result<T, DifferenceError>) (right: Result<T, DifferenceError>) =
    match (left, right) with
      | (Ok l, Ok r) -> takeDifference l r
      | (Result.Error _, _) -> left
      | _ -> right
