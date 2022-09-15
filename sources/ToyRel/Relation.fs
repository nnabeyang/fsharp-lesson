module Relation
open Common
open MyResult
open Deedle
open System.IO
open System.Collections.Generic

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
  
  let readCsv ident =
    Frame.ReadCsv (databasePath database.Value ident)
    |> distinct
  
  // Relationをロードする
  let load ident = readCsv ident |> MyResult.Ok
  
  // Relationを保存する。名前が衝突した場合は上書き保存する。  
  let save relation ident =
    let df = toFrame relation
    df.SaveCsv ((databasePath database.Value ident), includeRowKeys = false)
  
  // Relationからcolsで指定したカラムだけ残したRelationを新たに作る
  let project (cols: list<string>) relation =
    let df = toFrame relation
    df.Columns.[ cols ] |> distinct |> MyResult.Ok

  // 現在のDatabaseのRelation名一覧を返す
  let list() =
    grab (databasePath database.Value (Identifier.Identifier "*"))
    |> Seq.map Path.GetFileNameWithoutExtension
  
  // 指定したRelationの内容を標準出力する
  let print ident =
    let df = readCsv ident |> toFrame
    df.Print()

  // 指定したRelationのカラム名の名前を変更した新しいRelationを新たに作る
  let rename ident colName newColName =
    let renameFunc name newName = fun s ->
      if s = name then
        newName
      else
        s
    let df = readCsv ident |> toFrame
    df.RenameColumns (renameFunc colName newColName)
    Relation df

  let checkColTypes (Relation df1) (Relation df2) =
    let cts1 = df1.ColumnTypes |> Seq.toList
    let cts2 = df2.ColumnTypes |> Seq.toList
    cts1 = cts2
  let checkColKeyNames (Relation df1) (Relation df2) =
    let keys1 = df1.ColumnKeys |> Seq.toList
    let keys2 = df2.ColumnKeys |> Seq.toList
    keys1 = keys2
  
  let checkUnionCompatible rel1 rel2 =
    if not (checkColTypes rel1 rel2) then
      Some (TypeError "column types do not match")
    else if not (checkColKeyNames rel1 rel2) then
      Some (EvalError "column names do not match")
    else
      None
      
  let takeDifference rel1 rel2 =
    let (Relation df2) = rel2
    let (Relation df1) = rel1
    let valueSet = df2.Rows.Values |> Seq.toList<ObjectSeries<string>> |> HashSet
    df1.Rows.Values
      |> Seq.filter (fun s -> not (valueSet.Contains s))
      |> Series.ofValues
      |> Frame.ofRows
      |> Relation
  
  let takeUnion rel1 rel2 =
    let (Relation df2) = rel2
    let (Relation df1) = rel1
    let  valueSet = df1.Rows.Values |> Seq.toList<ObjectSeries<string>> |> HashSet
    valueSet.UnionWith df2.RowsDense.Values
    let values = valueSet |> List
    values
      |> Series.ofValues
      |> Frame.ofRows
      |> Relation
  
  let takeIntersect rel1 rel2 =
    let (Relation df2) = rel2
    let (Relation df1) = rel1
    let  valueSet = df1.Rows.Values |> Seq.toList<ObjectSeries<string>> |> HashSet
    valueSet.IntersectWith df2.RowsDense.Values
    let values = valueSet |> List
    values
      |> Series.ofValues
      |> Frame.ofRows
      |> Relation
  
  let calcUnionCompatible left right (f: T -> T -> T) = MyResult.result {
    let! l = left
    let! r = right
    let! d =
      match (checkUnionCompatible l r) with
        | Some e -> MyResult.Error e
        | None -> f l r |> MyResult.Ok
    return d
  }

  // 2つのRelationのdifferenceをとった新しいReletionを作る
  let difference left right = calcUnionCompatible left right takeDifference
  // 2つのRelationのunionをとった新しいReletionを作る
  let union left right = calcUnionCompatible left right takeUnion
  // 2つのRelationのintersectをとった新しいReletionを作る
  let intersect left right = calcUnionCompatible left right takeIntersect

  // 2つのリレーション分のカラム名を取得する。
  // ただし、カラム名が重複する場合は右側のカラム名にprefixを付ける。
  let combinedColumns (Relation df1) (Relation df2) prefix =
    let columns1 = df1.Columns.Keys
    let keys1Set = columns1 |> HashSet

    let columns2 =
      df2.Columns.Keys
        |> Seq.map (fun name ->
          if (keys1Set.Contains name) then
            (sprintf "%s.%s" prefix name)
          else
            name
        )
    Seq.append columns1 columns2
  
  let calcProduct rel1 rel2 prefix =
      let (Relation df2) = rel2
      let (Relation df1) = rel1      
      let columns = combinedColumns rel1 rel2 prefix
      df1.RowsDense.Values |> Seq.toList
        |> List.map (fun row1 ->
          df2.RowsDense.Values |> Seq.toList
            |> List.map (fun row2 ->
              Series(columns, Seq.append row1.ValuesAll row2.ValuesAll)   
            )
          )
      |> List.concat
      |> Series.ofValues
      |> Frame.ofRows
      |> Relation
      |> MyResult.Ok
  
  // 2つのリレーションの積(直積)を取る
  let product left right rName = MyResult.result {
    let! l = left
    let! r = right
    let! d = calcProduct l r rName
    return d
  }

  let restrict rel  f =
    let df = toFrame rel
    df.RowsDense
      |> Series.filterValues(f)
      |> Frame.ofRows
      |> Relation
      |> MyResult.Ok

  // 指定したcolumnNameをRelation上のカラム名に変換する
  // columnNameもRelationのカラムもプレフィックス付きとプレフィックス無しのどちらもあり得るので、この変換が必要になる
  let effectiveColumnName (Relation df) columnName =
    let colSet = df.ColumnKeys |> HashSet
    let fullName = toString columnName
    if colSet.Contains fullName then
      fullName
    else
      dropPrefix columnName

  let findColumnIndex rel columnName =
    let (Relation df) = rel
    let name = effectiveColumnName rel columnName
    df.ColumnKeys |> Seq.findIndex (fun key -> key = name)

  let getTypeByColName rel name =
    let df = toFrame rel
    let types = df.ColumnTypes |> Seq.toList
    try
      let idx = findColumnIndex rel name
      match types[idx] with
        | p when p = typeof<int> -> IntType |> MyResult.Ok
        | p when p = typeof<string> -> StrType |> MyResult.Ok
        | p when p = typeof<bool> -> BoolType |> MyResult.Ok
        | p when p = typeof<System.DateTime> -> DateTimeType |> MyResult.Ok
        | p -> MyResult.Error (TypeError (sprintf "%A is not supported" p))
    with
      | :? System.Collections.Generic.KeyNotFoundException -> MyResult.Error (EvalError "column name is wrong")

  let getType rel value =
    match value with
      | Literal literal ->
        match literal with
          | StrLiteral _ -> StrType
          | IntLiteral _ -> IntType
          | BoolLiteral _ -> BoolType
        |> MyResult.Ok
      | ColumnName name -> getTypeByColName rel name
