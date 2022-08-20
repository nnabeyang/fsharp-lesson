#r "nuget: FParsec"
open FParsec

let test p str =
  match run p str with
   | Success(result, _, _) -> printfn "Success: %A" result
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
