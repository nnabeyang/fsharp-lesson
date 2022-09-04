#r "nuget: FParsec"
#r "nuget: Deedle"
open FParsec
#load "Common.fs"
#load "MyResult.fs"
#load "Relation.fs"
#load "Condition.fs"
#load "Parser.fs"
#load "Eval.fs"
open Parser
open Eval

let run src =
  match (run pStatement src) with
    | ParserResult.Success(stmt, _, _) -> Eval.print (evalStatement stmt)
    | ParserResult.Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

// 11. いろいろprojectを実行してみよう
// wikipediaデータベースのデータで、Employeeの名前の一覧を表示してみましょう。
run "use wikipedia"
run "q11_1 = (project (Employee) Name)"
run "print q11_1"
(*
     Key Name    
0 -> 0   Harry   
1 -> 1   Sally   
2 -> 2   George  
3 -> 3   Harriet 
4 -> 4   Mary 
*)

// tandp.mdの図書館データベースについて、この図書館に所蔵されている本の著者の一覧を表示しましょう。
run "use tandp"
run "q11_2 = (project (book) author)"
run "print q11_2"
(*
     Key author    
0 -> 0   JOYCE     
1 -> 1   GREENE    
2 -> 2   ORWELL    
3 -> 3   LEM       
4 -> 4   GOLDING   
5 -> 5   KING      
6 -> 6   HEMINGWAY 
*)

// tandp.mdの在庫管理データベースについて、商品を作っている生産者の一覧を表示しましょう。
run "use tandp"
run "q11_3 = (project (goods) producer)"
run "print q11_3"
(*
     Key producer   
0 -> 0   CLASSICS   
1 -> 1   60S CLOTHS 
2 -> 2   MODERNA  
*)

// tandp.mdの在庫管理データベースについて、どこかの支社に一度でも配送したことなる生産者の一覧を表示しましょう。
run "use tandp"
run "q11_4 = (project (delivery) producer)"
run "print q11_4"
(*
     Key producer   
0 -> 0   CLASSICS   
1 -> 1   60S CLOTHS 
2 -> 2   MODERNA   
*)

// 14. いろいろdifferenceを試してみよう

// "differenceの説明"に載っている式
run "use wikipedia"
run "q14_0 = (project (Employee) DeptName) difference (project (Dept) DeptName)"
run "print q14_0"
(*
     Key DeptName        
0 -> 0   Human Resources 
*)

// tandp.mdの図書館データベースで、図書館にまったく本が存在しないsubjectの一覧を取り出す
run "use tandp"
run "q14_1 = (project (subject) class) difference (project (index) class)"
run "print q14_1"
(*
     Key class 
0 -> 0   C4    
1 -> 1   C5    
2 -> 2   C6
*)
// wikipediaデータベースでEmployeeの居ない部署を取り出す
run "use wikipedia"
run "q14_2 = (project (Dept) DeptName) difference (project (Employee) DeptName)"
run "print q14_2"
(*
     Key DeptName   
0 -> 0   Production 
*)

// wikipediaデータベースで(project (Employee) DeptName) difference (project (Dept) Manager)を実行して、
// Union compatibleじゃない（カラムが違う）エラーが出ることを確認する
run "use wikipedia"
run "(project (Employee) DeptName) difference (project (Dept) Manager)"
//Failure: EvalError: column names do not match

// wikipediaデータベースで(project (Employee) EmpId) difference (project (EmployeeTypeMismatch) EmpId)を実行して、
// Union compatibleじゃない（型が違う）エラーが出ることを確認する
run "use wikipedia"
run "(project (Employee) EmpId) difference (project (EmployeeTypeMismatch) EmpId)"
//Failure: TypeError: column types do not match

// 15. restrictを動かしてみる

// "restrictの実装"に載っている式
run "use tandp"
run "q15_0 = restrict (auction) (sell_price>purchase_price)"
run "print q15_0"
(*
     Key reference date_bought purchase_price date_sold sell_price 
0 -> 1   R020      02-12-43    4              17-10-88  145        
1 -> 3   R048      15-05-68    3              16-03-89  8          
2 -> 4   R049      15-05-68    3              16-03-89  8   
*)

// tandp.mdの図書館データベースで、indexから作者がヘミングウェイでクラスがc3のものを取り出しましょう
run "use tandp"
run "q15_1 = restrict (index) ((author = \"HEMINGWAY\") and (class = \"C3\"))"
run "print q15_1"
(*
     Key author    title                  class shelf 
0 -> 7   HEMINGWAY DEATH IN THE AFTERNOON C3    22  
*)

// tandp.mdの在庫管理データベースについて、L1支社に現在まだ在庫としてある商品（INSTOCK）のsell_priceとcost_priceの一覧を取り出しましょう。
run "use tandp"
run "q15_2 = project (restrict (stock) (date_out = \"INSTOCK\")) sell_price, cost_price"
run "print q15_2"
(*
     Key   sell_price cost_price 
0 -> False 15.50      9.25       
1 -> True  13.50      6.25  
*)

// restrictのエラー

// 条件式がカラム名になっている場合
run "use tandp"
run "res = restrict (auction) (sell_price)"
//Failure: EvalError: column name is not a conditional expression.

// 条件式にbool以外のリテラルになっている場合
run "use tandp"
run "res = restrict (auction) (\"hello\")"
//Failure: EvalError: System.String value is not a conditional expression

// 条件式の型が違う場合(cell_priceは整数)
run "use tandp"
run "res = restrict (auction) (sell_price = \"hello\")"
//Failure: TypeError: Type mismatch in conditional expression: System.Int32 <=> System.String

// 存在しないカラム名で式を書いた場合
run "use tandp"
run "restrict (auction) (no_such_col > 0)"
//Failure: EvalError: column name is wrong
