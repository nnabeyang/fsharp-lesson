module Common
open System.IO
open Deedle

type LogicalOp = And | Or
type ComparisonOp = Eq | Ne | Lt | Gt | Le | Ge
type Filter = Filter of (ObjectSeries<string> -> obj)
type ConditionalExpression =
  | Value of Value
  | Function of Function
and Value =
  | Literal of obj
  | ColumnName of string
and Function =
  | Comparison of ConditionalExpression * ComparisonOp * ConditionalExpression
  | Logical of ConditionalExpression * LogicalOp * ConditionalExpression

type Identifier = Identifier of string
type Database = Database of string

type Expression =
  | Identifier of Identifier
  | Project of ProjectExpression
  | Difference of BinaryExpression
  | Restrict of Expression * ConditionalExpression
and ProjectExpression = Expression * string list
and BinaryExpression = Expression * Expression

type Statement =
  | ExpressionStatement of Expression
  | PrintStatement of Identifier
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
