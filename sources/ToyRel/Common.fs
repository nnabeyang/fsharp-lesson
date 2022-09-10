module Common
open System.IO
open Deedle

type ValueType =
  | IntType
  | StrType
  | BoolType

type LogicalOp = And | Or
type ComparisonOp = Eq | Ne | Lt | Gt | Le | Ge

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

// RowFuncはrestrictの条件式を構成する式やリテラルを
// Relationの行を引数に取る関数として保持する。
type RowFunc =
  // ColFuncはrestrictの条件式の中で単体では条件式になれないものを表現する。
  // ColFuncには次の2種類がある。
  // 1. カラム名
  // 2. 定数
  // ただし、これらの型がboolの場合は条件式になれるのでFilterになる。
  // 例: 次のようなケースでは"cell_price"が1.の場合になり、"3"が2.のケースになる。
  // restrict (auction) (sell_price > 3)
  | ColFunc of ColFunc
  // Filterはrestrictの条件式の中で、それ単体で条件式になれるものを表現する。
  // ColFuncで挙げた例を使うと"cell_price > 3"はFilterになる。
  | Filter of Filter
and ColFunc = ObjectSeries<string> -> ConditionalValue
and Filter = (ObjectSeries<string> -> bool)
type ConditionalExpression =
  | Value of Value
  | Function of Function
and Value =
  | Literal of ConditionalLiteral
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
  | Product of BinaryExpression
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
