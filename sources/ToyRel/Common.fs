module Common

type Identifier = Identifier of string
type Database = Database of string

type Expression =
  | Identifier of Identifier
  | Project of ProjectExpression
and ProjectExpression = Expression * string list

type Statement =
  | ExpressionStatement of Expression
  | PrintStatement of Identifier
  | AssignmentStatement of Assignment
  | ListStatement
  | UseStatement of Database
and
  Assignment = Identifier * Expression

let databaseBaseDir = "./database"
let database = ref (Database "master")
