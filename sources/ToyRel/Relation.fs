module Relation
open Common
open Deedle

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
  let readCsv (Identifier.Identifier basename) =
    let (Database databaseName) = database.Value
    let databaseDir = sprintf "%s/%s" databaseBaseDir databaseName
    Frame.ReadCsv (sprintf "%s/%s.csv" databaseDir basename)
    |> distinct

  // relationを保存する。名前が衝突した場合は上書き保存する。
  let save relation (Identifier.Identifier basename) =
    let (Database databaseName) = database.Value
    let databaseDir = sprintf "%s/%s" databaseBaseDir databaseName
    let df = toFrame relation
    df.SaveCsv (sprintf "%s/%s.csv" databaseDir basename)
 
  // Relationからcolsで指定したカラムだけ残したRelationを新たに作る
  let project (cols: list<string>) relation =
    let df = toFrame relation
    df.Columns.[ cols ] |> distinct
