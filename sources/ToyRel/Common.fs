module Common

type Identifier = Identifier of string

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

let databaseDir = "./database/master"
