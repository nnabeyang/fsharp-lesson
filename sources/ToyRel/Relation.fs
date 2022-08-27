module Relation
open Common
open Deedle
open System.IO

module Relation = 
  type T = Relation of Frame<int, string>

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
