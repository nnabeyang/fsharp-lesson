// For more information see https://aka.ms/fsharp-console-apps
open FParsec
// 課題1
(*
identifierの仕様
- 1文字目数字は禁止する
- カタカナ、漢字、英数字と"_"をサポートする
*)

let firstIdentifierRegex = "[_a-zA-Z]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs}"
let identifierRegex = sprintf "(%s)([0-9]|%s)*" firstIdentifierRegex firstIdentifierRegex

let test p str =
  match run p str with
   | Success(result, _, _) -> printfn "Success: %A" result
   | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let str s = pstring s
let str_ws s = str s .>> spaces

let pIdentifier = regex identifierRegex
let pSBracketColumn =
  let notSBracket s = s <> '[' && s <> ']'
  (str "[") >>. many1Satisfy notSBracket .>> (str "]")
let pColumn = pIdentifier <|> pSBracketColumn
let pColumnList =
  let column_ws = pColumn .>> spaces
  sepBy column_ws (str_ws ",")

type Expression =
  | Identifier of string
  | Project of Expression * string list

let pExpression, pExpressionRef = createParserForwardedToRef<Expression, unit>()
let pProjectExpression = (str_ws "project") >>. (pExpression .>> spaces) .>>. pColumnList |>> Project
pExpressionRef.Value <- (str "(") >>. (pProjectExpression <|> (pIdentifier |>> Identifier)) .>> (str ")")

test pProjectExpression "project (project (シラバス) 専門, 学年, 場所) 専門, 学年"
// => Success: Project (Project (Identifier "シラバス", ["専門"; "学年"; "場所"]), ["専門"; "学年"])
