module Relation
open Common
open Deedle
open System.IO
open System.Collections.Generic

module Relation = 
  type T = Relation of Frame<int, string>
  type Result =
    | Success of T
    | Failure of string

  // Frameから重複を除いてRelationを作る
  let distinct (df: Frame<int, string>) =
      df.Rows.Values 
      |> Seq.toList
      |> Seq.distinct 
      |> Series.ofValues 
      |> Frame.ofRows
      |> Relation
  
  let toFrame (Relation df) = df
  
  // csvファイルからRelationを作る
  let readCsv ident =
    Frame.ReadCsv (databasePath database.Value ident)
    |> distinct

  // relationを保存する。名前が衝突した場合は上書き保存する。
  let save relation ident =
    let df = toFrame relation
    df.SaveCsv (databasePath database.Value ident)
  // Relationからcolsで指定したカラムだけ残したRelationを新たに作る
  let project (cols: list<string>) relation =
    let df = toFrame relation
    df.Columns.[ cols ] |> distinct

  // 現在のDatabaseのRelation名一覧を返す
  let list() =
    grab (databasePath database.Value (Identifier.Identifier "*"))
    |> Seq.map Path.GetFileNameWithoutExtension

  let checkColTypes (Relation df1) (Relation df2) =
    let cts1 = df1.ColumnTypes |> Seq.toList
    let cts2 = df2.ColumnTypes |> Seq.toList
    cts1 = cts2
  let checkColKeyNames (Relation df1) (Relation df2) =
    let keys1 = df1.ColumnKeys |> Seq.toList
    let keys2 = df2.ColumnKeys |> Seq.toList
    keys1 = keys2
  let difference (rel1: T) (rel2: T) =
    if not (checkColTypes rel1 rel2) then
      Failure "column types do not match"
    else if not (checkColKeyNames rel1 rel2) then
      Failure "column names do not match"
    else
      let (Relation df2) = rel2
      let (Relation df1) = rel1
      let valueSet = df2.Rows.Values |> Seq.toList<ObjectSeries<string>> |> HashSet
      df1.Rows.Values
        |> Seq.filter (fun s -> not (valueSet.Contains s))
        |> Series.ofValues
        |> Frame.ofRows
        |> Relation
        |> Success
