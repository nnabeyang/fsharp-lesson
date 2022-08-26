#r "nuget: FParsec"
#r "nuget: Deedle"
#load "Common.fs"
#load "Relation.fs"
#load "Parser.fs"
#load "Eval.fs"
open Deedle
open System.Text.RegularExpressions
open Common
open Parser
open Eval


// 課題0
(*
identifierの仕様
- 1文字目数字は禁止する
- カタカナ、漢字、英数字と"_"をサポートする
*)

let firstIdentifierRegex = "[_a-zA-Z]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs}"
let identifierRegex = sprintf "(%s)([0-9]|%s)*" firstIdentifierRegex firstIdentifierRegex
// マッチするパターン
// テキストに書かれているパターン
// identifierRegexForTestはRegex.IsMatchで全体がマッチするか確認するために使用する。
let identifierRegexForTest = sprintf "^%s$" identifierRegex
Regex.IsMatch("abc", identifierRegexForTest)
Regex.IsMatch("_abc123", identifierRegexForTest)
Regex.IsMatch("abc_123", identifierRegexForTest)
Regex.IsMatch("専門", identifierRegexForTest)
Regex.IsMatch("フロア", identifierRegexForTest)
// 追加パターン
Regex.IsMatch("ABC", identifierRegexForTest) //アルファベット大文字
Regex.IsMatch("ひらがな", identifierRegexForTest) //ひらがな
// マッチしないパターン
// テキストに書かれているパターン
Regex.IsMatch("123", identifierRegexForTest)
Regex.IsMatch("abc.def", identifierRegexForTest)
Regex.IsMatch("abc*", identifierRegexForTest)
Regex.IsMatch("abc:def", identifierRegexForTest)
Regex.IsMatch("abc def", identifierRegexForTest)
Regex.IsMatch("abc*", identifierRegexForTest)
Regex.IsMatch("(abc)", identifierRegexForTest)
Regex.IsMatch("abc+def", identifierRegexForTest)
// 追加パターン
Regex.IsMatch("*あかさ", identifierRegexForTest) // "_"以外の記号から始まる
Regex.IsMatch("", identifierRegexForTest) // 空文字列

// 課題2
let distinct (df: Frame<int, string>) =
  df.Rows.Values 
  |> Seq.toList
  |> Seq.distinct 
  |> Series.ofValues 
  |> Frame.ofRows

let df = Frame.ReadCsv "./database/master/シラバス.csv"
(distinct df.Columns.[["専門";"学年"]]).Print()

let run src =
  match (FParsec.CharParsers.run pStatement src) with
    | FParsec.CharParsers.ParserResult.Success(stmt, _, _) ->
      match (evalStatement stmt) with
        | Some (Common.Identifier relationName) ->
          printfn "Relation %s returned." relationName
        | None -> ()        
    | FParsec.CharParsers.ParserResult.Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

run "list"
run "hoge=(Employee)"
run "abc=(project (project (Employee) Name, DeptName) DeptName)"
run "print hoge"
run "print abc"

// wikipediaデータベースのデータで、Employeeの名前の一覧を表示してみましょう。
run "use master"
run "q1 = (project (Employee) Name)"
run "print q1"
// tandp.mdの図書館データベースについて、この図書館に所蔵されている本の著者の一覧を表示しましょう。
run "use tandp"
run "q2 = (project (book) author)"
run "print q2"
// tandp.mdの在庫管理データベースについて、商品を作っている生産者の一覧を表示しましょう。
run "use tandp"
run "q3 = (project (goods) producer)"
run "print q3"
// tandp.mdの在庫管理データベースについて、どこかの支社に一度でも配送したことなる生産者の一覧を表示しましょう。
run "use tandp"
run "q3 = (project (delivery) producer)"
run "print q3"
