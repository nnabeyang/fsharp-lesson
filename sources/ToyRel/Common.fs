module Common
open System.IO

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
