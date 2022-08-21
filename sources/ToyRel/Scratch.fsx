#r "nuget: FParsec"
open FParsec
open System.Text.RegularExpressions

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
