// For more information see https://aka.ms/fsharp-console-apps
open FParsec
open Deedle
open System
// 課題7
let random = Random()
let randomString length =
  String.init length (fun _ -> char (random.Next( (int 'a'), (int 'z') + 1)) |> sprintf "%c")
type Identifier = Identifier of string
let randomBaseName() = "zz" + (randomString 4) |> Identifier.Identifier
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

type Statement =
  | ExpressionStatement of Expression
  | PrintStatement of Identifier

module Relation = 
  type T = Relation of Frame<int, string>

  let distinct (df: Frame<int, string>) =
      df.Rows.Values 
      |> Seq.toList
      |> Seq.distinct 
      |> Series.ofValues 
      |> Frame.ofRows
      |> Relation
  
  let toFrame (Relation df) = df
  
  let readCsv location = Frame.ReadCsv location |> distinct

  // relationを保存する。名前が衝突した場合は上書き保存する。
  let save relation (Identifier.Identifier basename) =
        let df = toFrame relation
        df.SaveCsv (sprintf "%s/%s.csv" databaseDir basename)
 
  let project (cols: list<string>) relation =
    let df = toFrame relation
    df.Columns.[ cols ] |> distinct

let pExpression, pExpressionRef = createParserForwardedToRef<Expression, unit>()
let pProjectExpression = (str_ws "project") >>. (pExpression .>> spaces) .>>. pColumnList |>> Project
pExpressionRef.Value <- 
  ((str "(") >>. (pProjectExpression <|> (pIdentifier |>> Expression.Identifier)) .>> (str ")"))
  <|> pProjectExpression

let pPrintStatement = (str_ws "print") >>. (pIdentifier |>> Identifier.Identifier |>> PrintStatement)
let pExpressionStatement = pExpression |>> ExpressionStatement
let pStatement = pPrintStatement <|> pExpressionStatement

let rec evalExpression expr =
  match expr with
    | Project cols -> evalProjectExpression cols
    | Expression.Identifier name -> Relation.readCsv (sprintf "%s/%s.csv" databaseDir name)
and  evalProjectExpression (expr : ProjectExpression) =
  let (ident, cols) = expr
  let df = match ident with
    | Project _ -> evalExpression ident
    | Expression.Identifier name -> Relation.readCsv (sprintf "%s/%s.csv" databaseDir name)
  Relation.project cols df

let runPrint (Identifier.Identifier basename) =
  let df =
    Relation.readCsv (sprintf "%s/%s.csv" databaseDir basename)
    |> Relation.toFrame
  df.Print()

let runExpression expr =
  let baseName = randomBaseName()
  let (Identifier.Identifier basename) = baseName
  Relation.save (evalExpression expr) baseName
  printfn "Relation %s returned." basename

let runStatement (str: string) =
  match (run pStatement str) with
    | ParserResult.Success(stmt, _, _) ->
      match stmt with
        | ExpressionStatement expr -> runExpression expr
        | PrintStatement ident -> runPrint ident
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

runStatement "project (シラバス) 専門, 学年, 場所)"
runStatement "project (project (シラバス) 専門, 学年, 場所) 専門, 学年"
// 次の"zzahza"の部分は存在するidentに書き換える
runStatement "print zzahza"
