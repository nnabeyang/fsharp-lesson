// For more information see https://aka.ms/fsharp-console-apps
open FParsec
open Deedle
open System
open System.IO

// 課題9
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
  | Identifier of Identifier
  | Project of ProjectExpression
and ProjectExpression = Expression * string list

type Statement =
  | ExpressionStatement of Expression
  | PrintStatement of Identifier
  | AssignmentStatement of Assignment
  | ListStatement
and
  Assignment = Identifier * Expression

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
  
  let readCsv (Identifier.Identifier basename) =
    Frame.ReadCsv (sprintf "%s/%s.csv" databaseDir basename)
    |> distinct

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
  ((str "(") >>. (pProjectExpression <|> (pIdentifier |>> Identifier.Identifier |>> Expression.Identifier)) .>> (str ")"))
  <|> pProjectExpression

let pPrintStatement = (str_ws "print") >>. (pIdentifier |>> Identifier.Identifier |>> PrintStatement)
let pListStatement = str "list" |>> fun _ -> ListStatement
let pAssignmentStatement =
  ((pIdentifier .>> spaces) |>> Identifier.Identifier) .>>. (str_ws "=" >>. pExpression) |>> AssignmentStatement
let pExpressionStatement = pExpression |>> ExpressionStatement
let pStatement = 
  pPrintStatement
  <|> pListStatement
  <|> pExpressionStatement 
  <|> pAssignmentStatement

let rec evalExpression expr =
  match expr with
    | Project cols -> evalProjectExpression cols
    | Expression.Identifier basename -> Relation.readCsv basename
and  evalProjectExpression (expr : ProjectExpression) =
  let (ident, cols) = expr
  let df = match ident with
    | Project _ -> evalExpression ident
    | _ -> evalExpression ident
  Relation.project cols df

let runPrint ident =
  let df =
    Relation.readCsv ident
    |> Relation.toFrame
  df.Print()

// データベース内のリレーションの一覧を標準出力する
let runList() =
  let relationList = 
    Directory.GetFiles databaseDir
    |> Seq.map Path.GetFileNameWithoutExtension
  printfn "%s" (relationList  |> String.concat Environment.NewLine)

let runAssignment (assignment: Assignment) =
  let (ident, expr) = assignment
  let (Identifier.Identifier relationName) = ident
  Relation.save (evalExpression expr) ident
  printfn "Relation %s returned." relationName

let runExpression expr = runAssignment (randomBaseName(), expr)

let runStatement (str: string) =
  match (run pStatement str) with
    | ParserResult.Success(stmt, _, _) ->
      match stmt with
        | ExpressionStatement expr -> runExpression expr
        | PrintStatement ident -> runPrint ident
        | AssignmentStatement assignment -> runAssignment assignment
        | ListStatement -> runList()
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

runStatement "list"
runStatement "project (project (シラバス) 専門, 学年, 場所) 専門, 学年"
runStatement "hoge=(シラバス)"
runStatement "print hoge"
runStatement "list"
