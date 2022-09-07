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
let pInt = (regex "[\-\+]?[0-9]+") |>> int |>> (fun x -> x) |>> IntLiteral
let pBool = (str "true" <|> str "false") |>> (fun x -> x = "true") |>> BoolLiteral
let pCondStr =
    let normalChar = satisfy (fun c -> c <> '\"')
    between (pstring "\"") (pstring "\"") (many1Chars normalChar)
    |>> StrLiteral
let pCondOprand: Parser<ConditionalExpression, unit> =
  (
    (pInt  <|> pCondStr <|> pBool) |>> Literal
    <|> (pColumn |>> ColumnName)
  )
  |>> Value
let oppc = new OperatorPrecedenceParser<ConditionalExpression, unit, unit>()
let pCondExpression = oppc.ExpressionParser
oppc.TermParser <- (pCondOprand .>> spaces) <|> between (str_ws "(") (str_ws ")") pCondExpression
oppc.AddOperator(InfixOperator("<", spaces, 2, Associativity.Left, fun x y -> Comparison (x, Lt, y) |> Function))
oppc.AddOperator(InfixOperator("<=", spaces, 2, Associativity.Left, fun x y -> Comparison (x, Le, y) |> Function))
oppc.AddOperator(InfixOperator("=", spaces, 1, Associativity.Left, fun x y -> Comparison (x, Eq, y) |> Function))
oppc.AddOperator(InfixOperator(">", spaces, 2, Associativity.Left, fun x y -> Comparison (x, Gt, y) |> Function))
oppc.AddOperator(InfixOperator(">=", spaces, 2, Associativity.Left, fun x y -> Comparison (x, Ge, y) |> Function))
oppc.AddOperator(InfixOperator("<>", spaces, 1, Associativity.Left, fun x y -> Comparison (x, Ne, y) |> Function))

let oppl = new OperatorPrecedenceParser<ConditionalExpression, unit, unit>()
let pLogical = oppl.ExpressionParser
oppl.TermParser <- (pCondExpression .>> spaces) <|> between (str_ws "(") (str_ws ")") pLogical
oppl.AddOperator(InfixOperator("and", spaces, 1, Associativity.Left, fun x y -> Logical (x, And, y) |> Function))
oppl.AddOperator(InfixOperator("or", spaces, 1, Associativity.Left, fun x y -> Logical (x, Or, y) |> Function))

let pTerm, pTermRef = createParserForwardedToRef<Expression, unit>()
let pProjectExpression = (str_ws "project") >>. (pTerm .>> spaces) .>>. pColumnList |>> Project
let pRestrictExpression = (str_ws "restrict") >>. (pTerm .>> spaces) .>>. ((str_ws "(") >>. pLogical .>> (str_ws ")")) |>> Restrict
let pIdentifierExpression = pIdentifier |>> Identifier.Identifier |>> Expression.Identifier
let pTermExpression = pProjectExpression <|> pRestrictExpression <|>  pIdentifierExpression
pTermRef.Value <- between (str_ws "(") (str_ws ")")  pTermExpression <|> pProjectExpression <|> pRestrictExpression

let oppe = new OperatorPrecedenceParser<Expression, unit, unit>()
let pExpression = oppe.ExpressionParser
oppe.TermParser <- pTerm
oppe.AddOperator(InfixOperator("difference", spaces, 1, Associativity.Left, fun x y -> Difference (x,y)))

let pUseStatement = (str_ws "use") >>. (pIdentifier |>> Database.Database |>> UseStatement)
let pPrintStatement = (str_ws "print") >>. (pIdentifier |>> Identifier.Identifier |>> PrintStatement)
let pListStatement = str "list" |>> fun _ -> ListStatement
let pAssignmentStatement =
  ((pIdentifier .>> spaces) |>> Identifier.Identifier) .>>. (str_ws "=" >>. pExpression) |>> AssignmentStatement
let pExpressionStatement = pExpression |>> ExpressionStatement
let pStatement =
  pPrintStatement
  <|> pUseStatement
  <|> pListStatement
  <|> pExpressionStatement 
  <|> pAssignmentStatement

let pQuit: Parser<string, unit> = str "quit"
