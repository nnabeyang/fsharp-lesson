#r "nuget: Deedle"

open Deedle

let df = Frame.ReadCsv "../../data/シラバス.csv"
// 課題3
df.RowsDense
|> Series.filterValues(fun row -> row.Get("専門") = "数学")
|> Frame.ofRows
