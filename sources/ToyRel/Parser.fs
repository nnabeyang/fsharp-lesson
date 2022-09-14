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
let identifierRegex = sprintf "(%s)([0-9\.]|%s)*" firstIdentifierRegex firstIdentifierRegex
let prefixRegex = sprintf "(%s)([0-9]|%s)*" firstIdentifierRegex firstIdentifierRegex

let pIdentifier = regex identifierRegex
let pSBracketColumn =
  let notSBracket s = s <> '[' && s <> ']'
  (str "[") >>. many1Satisfy notSBracket .>> (str "]")
let pSimpleColumn = pIdentifier <|> pSBracketColumn
let pPrefix = regex prefixRegex <|> pSBracketColumn
let pPrefixedColumn = pPrefix .>> str "." .>>. pSimpleColumn
let pColumn =
  attempt pPrefixedColumn |>> Prefixed
  <|> attempt (pSimpleColumn |>> Simple)
let pColumnList =
  let column_ws = pSimpleColumn .>> spaces
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
let makeComparison (x: ConditionalExpression, op: ComparisonOp, y: ConditionalExpression) =
  match (x, y) with
    | (Value vx, Value vy) -> SimpleCondition (vx, op, vy)
    | _ -> ComplexCondition (x, ComparisonOp op, y)
  |> Function
oppc.TermParser <- (pCondOprand .>> spaces) <|> between (str_ws "(") (str_ws ")") pCondExpression
oppc.AddOperator(InfixOperator("<", spaces, 2, Associativity.Left, fun x y -> makeComparison(x, Lt, y)))
oppc.AddOperator(InfixOperator("<=", spaces, 2, Associativity.Left, fun x y -> makeComparison(x, Le, y)))
oppc.AddOperator(InfixOperator("=", spaces, 1, Associativity.Left, fun x y ->  makeComparison(x, Eq, y)))
oppc.AddOperator(InfixOperator(">", spaces, 2, Associativity.Left, fun x y ->  makeComparison(x, Gt, y)))
oppc.AddOperator(InfixOperator(">=", spaces, 2, Associativity.Left, fun x y ->  makeComparison(x, Ge, y)))
oppc.AddOperator(InfixOperator("<>", spaces, 1, Associativity.Left, fun x y ->  makeComparison(x, Ne, y)))

let oppl = new OperatorPrecedenceParser<ConditionalExpression, unit, unit>()
let pLogical = oppl.ExpressionParser
oppl.TermParser <- (pCondExpression .>> spaces) <|> between (str_ws "(") (str_ws ")") pLogical
oppl.AddOperator(InfixOperator("and", spaces, 1, Associativity.Left, fun x y -> ComplexCondition (x, LogicalOp And, y) |> Function))
oppl.AddOperator(InfixOperator("or", spaces, 1, Associativity.Left, fun x y -> ComplexCondition (x, LogicalOp Or, y) |> Function))

let pTerm, pTermRef = createParserForwardedToRef<Expression, unit>()
let pProjectExpression = (str_ws "project") >>. (pTerm .>> spaces) .>>. pColumnList |>> Project
let pRestrictExpression = (str_ws "restrict") >>. (pTerm .>> spaces) .>>. ((str_ws "(") >>. pLogical .>> (str_ws ")")) |>> Restrict
let pJoinExpression =
  pipe3 ((str_ws "join") >>. (pTerm .>> spaces)) (pTerm .>> spaces) ((str_ws "(") >>. pLogical .>> (str_ws ")")) (fun x y z -> Join (x, y, z))
let pIdentifierExpression = pIdentifier |>> Identifier.Identifier |>> Expression.Identifier
let pTermExpression = pProjectExpression <|> pRestrictExpression <|>  pIdentifierExpression
pTermRef.Value <- between (str_ws "(") (str_ws ")")  pTermExpression <|> pProjectExpression <|> pRestrictExpression <|> pJoinExpression

let oppe = new OperatorPrecedenceParser<Expression, unit, unit>()
let pExpression = oppe.ExpressionParser
oppe.TermParser <- pTerm
oppe.AddOperator(InfixOperator("difference", spaces, 1, Associativity.Left, fun x y -> Difference (x,y)))
oppe.AddOperator(InfixOperator("product", spaces, 1, Associativity.Left, fun x y -> Product (x,y)))
oppe.AddOperator(InfixOperator("union", spaces, 1, Associativity.Left, fun x y -> Union (x,y)))

let pUseStatement = (str_ws "use") >>. (pIdentifier |>> Database.Database |>> UseStatement)
let pPrintStatement = (str_ws "print") >>. (pIdentifier |>> Identifier.Identifier |>> PrintStatement)
let pRenameStatement = (str_ws "rename") >>. ((str_ws "(") >>. pPrefixedColumn .>> (str_ws ")")) .>>. pSimpleColumn |>> (fun ((x, y), z) -> Rename (Identifier.Identifier x, y, z))
let pListStatement = str "list" |>> fun _ -> ListStatement
let pAssignmentStatement =
  ((pIdentifier .>> spaces) |>> Identifier.Identifier) .>>. (str_ws "=" >>. pExpression) |>> AssignmentStatement
let pExpressionStatement = pExpression |>> ExpressionStatement
let pStatement =
  pPrintStatement
  <|> pRenameStatement
  <|> pUseStatement
  <|> pListStatement
  <|> pExpressionStatement 
  <|> pAssignmentStatement

let pQuit: Parser<string, unit> = str "quit"
