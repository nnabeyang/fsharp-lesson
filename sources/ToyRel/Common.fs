module Common
open System.IO

type ValueType =
  | IntType
  | StrType
  | BoolType
  | DateTimeType

type BinaryOp =
  | LogicalOp of LogicalOp
  | ComparisonOp of ComparisonOp
and LogicalOp = And | Or
and ComparisonOp = Eq | Ne | Lt | Gt | Le | Ge

let evalCompl op l r =
  match op with
    | And -> l && r
    | Or -> l || r
let evalComp op l r =
  match op with
    | Eq -> l = r
    | Ne -> l <> r
    | Lt -> l <  r
    | Gt -> l > r
    | Le -> l <= r
    | Ge -> l >= r

type ConditionalLiteral =
  | IntLiteral of int
  | StrLiteral of string
  | BoolLiteral of bool
type ConditionalValue =
  | IntValue of int
  | StrValue of string
  | BoolValue of bool
  | DateTimeValue of System.DateTime

type ConditionalExpression =
  | Value of Value
  | Function of Function
and Value =
  | Literal of ConditionalLiteral
  | ColumnName of ColumnName
and ColumnName =
  | Prefixed of string * string
  | Simple of string
and Function =
  | SimpleCondition of Value * ComparisonOp * Value
  | ComplexCondition of ConditionalExpression * BinaryOp * ConditionalExpression

type Identifier = Identifier of string
type Database = Database of string

let toString column =
  match column with
    | Prefixed (prefix, name) ->
      sprintf "%s.%s" prefix name
    | Simple name -> name

let dropPrefix column =
  match column with
    | Prefixed (_, name) -> name
    | Simple name -> name

type Expression =
  | Identifier of Identifier
  | Project of ProjectExpression
  | Difference of BinaryExpression
  | Product of BinaryExpression
  | Union of BinaryExpression
  | Restrict of Expression * ConditionalExpression
  | Join of Expression * Expression * ConditionalExpression
and ProjectExpression = Expression * string list
and BinaryExpression = Expression * Expression

type Statement =
  | ExpressionStatement of Expression
  | PrintStatement of Identifier
  | Rename of Identifier * string * string
  | AssignmentStatement of Assignment
  | ListStatement
  | UseStatement of Database
and
  Assignment = Identifier * Expression

type ToyRelError =
  | EvalError of string
  | TypeError of string

let databaseBaseDir = "database"
let databaseFileExt = ".csv"
let database = ref (Database "master")

let grab (path: string) =
  let fileName = Path.GetFileName path
  let dirName = Path.GetDirectoryName path
  Directory.GetFiles (dirName, fileName)
let joinPath = List.fold (fun path entry -> Path.Combine(path, entry)) ""

let databasePath (Database databaseName) (Identifier.Identifier ident) =
  joinPath ["."; databaseBaseDir; databaseName; ident + databaseFileExt]
