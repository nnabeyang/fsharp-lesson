#r "nuget: Deedle"

open Deedle

let df = Frame.ReadCsv "../../data/シラバス.csv"
// 課題3
df.RowsDense
|> Series.filterValues(fun row -> row.Get("専門") = "数学")
|> Frame.ofRows
// 課題4
df.Columns.[ [|"場所";"学年"|] ]
