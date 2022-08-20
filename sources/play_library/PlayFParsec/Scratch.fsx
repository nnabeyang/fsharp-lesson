#r "nuget: FParsec"
#r "nuget: Deedle"
open FParsec
open Deedle

let test p str =
  match run p str with
   | FParsec.CharParsers.ParserResult.Success(result, _, _) -> printfn "Success: %A" result
   | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

// 課題10
let pColumn =
    let normalChar = satisfy (fun c -> c <> ']')    
    between (pstring "[") (pstring "]") (many1Chars normalChar)

test pColumn "[場所]"
test pColumn "[]"

// 課題10.1
let pidentifier  =
  let isIdentifierFirstChar c = isLetter c
  let isIdentifierChar c = isLetter c || isDigit c
  many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"

test pidentifier "abc"
test pidentifier "i18n"
test pidentifier "12"

// 課題15
type Expression = 
  | ProjectExpression of string list
  | FilterExpression of col:string * value:string

let pProject =
  let ws = spaces
  let str_ws s = pstring s .>> ws
  let column_ws = pColumn .>> ws
  let pColumns = str_ws "(" >>. sepBy column_ws (str_ws ",") .>> str_ws ")"
  str_ws "project" >>. pColumns |>> ProjectExpression .>> ws

test pProject "project([場所],  [学年])"
test pProject "project ([場所][学年]"

let pFilter =
  let ws = spaces
  let str_ws s = pstring s .>> ws
  let pColName =
    let normalChar = satisfy (fun c -> c <> ']')    
    between (pstring "[") (pstring "]") (many1Chars normalChar)
  let pColValue =
    let normalChar = satisfy (fun c -> c <> '\"')    
    between (pstring "\"") (pstring "\"") (many1Chars normalChar)  
  let pBody = pipe2 (pColName .>> ws) (pstring "=" >>. ws >>. pColValue) (fun x  z -> FilterExpression (x, z))
  str_ws "filter" >>. str_ws "(" >>.  pBody .>> str_ws ")"

test pFilter "filter([専門] = \"数学\")"
test pFilter "filter([専門] = \"数学\""

let pExpression = pFilter <|> pProject
test pExpression "filter([専門] = \"数学\")"
test pExpression "project([場所],  [学年])"

let filter (pred: ObjectSeries<string> -> bool) (df: Frame<int,string>) =
  df.RowsDense
  |> Series.filterValues pred
  |> Frame.ofRows

let project (cols: list<string>) (df: Frame<int,string>) = df.Columns.[ cols ]

let runExpr (str: string) (df: Frame<int, string>)=
  match (run pExpression str) with
   | FParsec.CharParsers.ParserResult.Success(result, _, _) ->
     match result with
       | ProjectExpression cols -> (project cols df).Print()
       | FilterExpression (col, value) -> 
         (filter (fun row -> row.Get(col) = value) df).Print()   
   | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let df = Frame.ReadCsv "../../data/シラバス.csv"
runExpr "project([場所], [学年])" df
runExpr "filter([専門] = \"数学\")" df
