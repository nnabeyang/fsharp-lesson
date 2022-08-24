module Parser
open Common
open FParsec

let str s = pstring s
let str_ws s = str s .>> spaces

(*
identifierの仕様
- 1文字目数字は禁止する
- カタカナ、漢字、英数字と"_"をサポートする
*)
let firstIdentifierRegex = "[_a-zA-Z]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs}"
let identifierRegex = sprintf "(%s)([0-9]|%s)*" firstIdentifierRegex firstIdentifierRegex

let pIdentifier = regex identifierRegex
let pSBracketColumn =
  let notSBracket s = s <> '[' && s <> ']'
  (str "[") >>. many1Satisfy notSBracket .>> (str "]")
let pColumn = pIdentifier <|> pSBracketColumn
let pColumnList =
  let column_ws = pColumn .>> spaces
  sepBy column_ws (str_ws ",")

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

let parse src = 
  match (run pStatement src) with
    | Success(stmt, _, _) -> stmt
    | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg
