// For more information see https://aka.ms/fsharp-console-apps
open FParsec
open Deedle
open System
// 課題6
let random = Random()
let randomString (length: int) =
  String.init length (fun _ -> char (random.Next( (int 'a'), (int 'z') + 1)) |> sprintf "%c")
let databaseDir = "./database/master"
(*
identifierの仕様
- 1文字目数字は禁止する
- カタカナ、漢字、英数字と"_"をサポートする
*)

let firstIdentifierRegex = "[_a-zA-Z]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs}"
let identifierRegex = sprintf "(%s)([0-9]|%s)*" firstIdentifierRegex firstIdentifierRegex

let test p str =
  match run p str with
   | ParserResult.Success(result, _, _) -> printfn "Success: %A" result
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
  | Project of ProjectExpression
and ProjectExpression = Expression * string list

module Relation = 
  type T = Relation of Frame<int, string>

  let distinct (df: Frame<int, string>) =
      df.Rows.Values 
      |> Seq.toList
      |> Seq.distinct 
      |> Series.ofValues 
      |> Frame.ofRows
      |> Relation
  
  let toFrame relation =
    let (Relation df) = relation
    df
  
  let readCsv location = Frame.ReadCsv location |> distinct

  let save (relation: T) (basename: string) =
        let df = toFrame relation
        df.SaveCsv (sprintf "%s/%s" databaseDir basename)
 
  let project (cols: list<string>) (relation: T) =
    let df = toFrame relation
    df.Columns.[ cols ] |> distinct

let pExpression, pExpressionRef = createParserForwardedToRef<Expression, unit>()
let pProjectExpression = (str_ws "project") >>. (pExpression .>> spaces) .>>. pColumnList |>> Project
pExpressionRef.Value <- 
  ((str "(") >>. (pProjectExpression <|> (pIdentifier |>> Identifier)) .>> (str ")"))
  <|> pProjectExpression

let rec evalExpression expr =
  match expr with
    | Project cols -> evalProjectExpression cols
    | Identifier name -> Relation.readCsv (sprintf "./database/master/%s.csv" name)
and  evalProjectExpression (expr : ProjectExpression) =
  let (ident, cols) = expr
  let df = match ident with
    | Project _ -> evalExpression ident
    | Identifier name -> Relation.readCsv (sprintf "./database/master/%s.csv" name)
  Relation.project cols df
      
let runExpression (str: string) =
  match (run pExpression str) with
    | ParserResult.Success(expr, _, _) ->
      let df = Relation.toFrame (evalExpression expr)
      df.Print()
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

runExpression "project (シラバス) 専門, 学年, 場所"
runExpression "project (project (シラバス) 専門, 学年, 場所) 専門, 学年"

let df = Frame.ReadCsv "./database/master/シラバス.csv"
let relation = Relation.distinct (df.Columns.[["学年";"場所"]])
Relation.save relation ("zz" + (randomString 4) + ".csv")
