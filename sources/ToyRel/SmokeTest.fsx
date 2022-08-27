#r "nuget: FParsec"
#r "nuget: Deedle"
open FParsec
#load "Common.fs"
#load "Relation.fs"
#load "Parser.fs"
#load "Eval.fs"
open Parser
open Eval

let test p str =
  match run p str with
   | ParserResult.Success(result, _, _) -> printfn "Success: %A" result
   | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let run src =
  match (run pStatement src) with
    | Success(stmt, _, _) ->
      match (evalStatement stmt) with
        | Some (Common.Identifier relationName) ->
          printfn "Relation %s returned." relationName
        | None -> ()        
    | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg


test pStatement "list"
test pStatement "project (Employee) DeptName"
test pStatement "project (Employee) Name, DeptName"
test pStatement "(project (Employee) DeptName)"
test pStatement "(project (Employee) DeptName) difference (project (Dept) DeptName)"
test pStatement "(project (Employee) DeptName) difference (project (Dept) DeptName) difference (project (Dept) DeptName)"
test pStatement "abc = (project (Employee) DeptName) difference (project (Dept) DeptName)"

run "list"
run "use wikipedia"
run "hoge=(Employee)"
run "print hoge"
run "abc=(project (project (Employee) Name, DeptName) DeptName)"
run "print abc"

// wikipediaデータベースのデータで、Employeeの名前の一覧を表示してみましょう。
run "use wikipedia"
run "q1 = (project (Employee) Name)"
run "print q1"
// tandp.mdの図書館データベースについて、この図書館に所蔵されている本の著者の一覧を表示しましょう。
run "use tandp"
run "q2 = (project (book) author)"
run "print q2"
// tandp.mdの在庫管理データベースについて、商品を作っている生産者の一覧を表示しましょう。
run "use tandp"
run "q3 = (project (goods) producer)"
run "print q3"
// tandp.mdの在庫管理データベースについて、どこかの支社に一度でも配送したことなる生産者の一覧を表示しましょう。
run "use tandp"
run "q3 = (project (delivery) producer)"
run "print q3"

run "use wikipedia"
run "diff = (project (Employee) DeptName) difference (project (Dept) DeptName)"
run "print diff"
