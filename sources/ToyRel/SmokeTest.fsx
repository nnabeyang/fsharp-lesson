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

// 条件式がカラムの値がboolにキャストできない場合
run "use tandp"
run "res = restrict (auction) (date_bought)"
// Failure: EvalError: non-boolean value is not a conditional expression.

// 条件式にbool以外のリテラルになっている場合
run "use tandp"
run "res = restrict (auction) (\"hello\")"
//Failure: TypeError: non-boolean value is not a conditional expression.

// 条件式にbool以外のリテラルになっている場合
run "use tandp"
run "res = restrict (stock) (date_out = \"INSTOCK\" and 1)"
// Failure: TypeError: non-boolean value is cannot be computed with logical operators.

// 条件式の型が違う場合(cell_priceは整数)
run "use tandp"
run "res = restrict (auction) (sell_price = \"hello\")"
//Failure: TypeError: Type mismatch in conditional expression: IntType <=> StrType

// 存在しないカラム名で式を書いた場合
run "use tandp"
run "restrict (auction) (no_such_col > 0)"
//Failure: EvalError: column name is wrong

// 16: productの例
run "use wikipedia"
run "ex16_0 = (Employee) product (Dept)"
run "print ex16_0"
(*
      Key Name    EmpId DeptName        Dept.DeptName Manager 
0  -> 0   Harry   3415  Finance         Finance       George  
1  -> 1   Harry   3415  Finance         Sales         Harriet 
2  -> 2   Harry   3415  Finance         Production    Charles 
3  -> 3   Sally   2241  Sales           Finance       George  
4  -> 4   Sally   2241  Sales           Sales         Harriet 
5  -> 5   Sally   2241  Sales           Production    Charles 
6  -> 6   George  3401  Finance         Finance       George  
7  -> 7   George  3401  Finance         Sales         Harriet 
8  -> 8   George  3401  Finance         Production    Charles 
9  -> 9   Harriet 2202  Sales           Finance       George  
10 -> 10  Harriet 2202  Sales           Sales         Harriet 
11 -> 11  Harriet 2202  Sales           Production    Charles 
12 -> 12  Mary    1257  Human Resources Finance       George  
13 -> 13  Mary    1257  Human Resources Sales         Harriet 
14 -> 14  Mary    1257  Human Resources Production    Charles 
*)

// カラム名が被らない例
run "use wikipedia"
run "ex16_1 = (Employee) product (Completed)"
run "print ex16_1"
(*
      Key Name    EmpId DeptName        Student Task      
0  -> 0   Harry   3415  Finance         Fred    Database1 
1  -> 1   Harry   3415  Finance         Fred    Database2 
2  -> 2   Harry   3415  Finance         Fred    Compiler1 
3  -> 3   Harry   3415  Finance         Eugene  Database1 
4  -> 4   Harry   3415  Finance         Eugene  Compiler1 
5  -> 5   Harry   3415  Finance         Sarah   Database1 
6  -> 6   Harry   3415  Finance         Sarah   Database2 
7  -> 7   Sally   2241  Sales           Fred    Database1 
8  -> 8   Sally   2241  Sales           Fred    Database2 
9  -> 9   Sally   2241  Sales           Fred    Compiler1 
10 -> 10  Sally   2241  Sales           Eugene  Database1 
11 -> 11  Sally   2241  Sales           Eugene  Compiler1 
12 -> 12  Sally   2241  Sales           Sarah   Database1 
13 -> 13  Sally   2241  Sales           Sarah   Database2 
14 -> 14  George  3401  Finance         Fred    Database1 
:     ... ...     ...   ...             ...     ...       
20 -> 20  George  3401  Finance         Sarah   Database2 
21 -> 21  Harriet 2202  Sales           Fred    Database1 
22 -> 22  Harriet 2202  Sales           Fred    Database2 
23 -> 23  Harriet 2202  Sales           Fred    Compiler1 
24 -> 24  Harriet 2202  Sales           Eugene  Database1 
25 -> 25  Harriet 2202  Sales           Eugene  Compiler1 
26 -> 26  Harriet 2202  Sales           Sarah   Database1 
27 -> 27  Harriet 2202  Sales           Sarah   Database2 
28 -> 28  Mary    1257  Human Resources Fred    Database1 
29 -> 29  Mary    1257  Human Resources Fred    Database2 
30 -> 30  Mary    1257  Human Resources Fred    Compiler1 
31 -> 31  Mary    1257  Human Resources Eugene  Database1 
32 -> 32  Mary    1257  Human Resources Eugene  Compiler1 
33 -> 33  Mary    1257  Human Resources Sarah   Database1 
34 -> 34  Mary    1257  Human Resources Sarah   Database2 
*)

// 右側にリレーション名が無い場合
run "use tandp"
run "ex16_2 = (restrict (stock) (stock > 1300)) product (project (restrict (stock) (date_out = \"INSTOCK\")) sell_price, cost_price)"
run "print ex16_2"
(*
     Key branch stock size colour sell_price cost_price date_in            date_out tmp.sell_price tmp.cost_price 
0 -> 0   L2     2921  M    BLACK  25.00      15.20      1989/04/17 0:00:00 17APR89  15.50          9.25           
1 -> 1   L2     2921  M    BLACK  25.00      15.20      1989/04/17 0:00:00 17APR89  13.50          6.25           
2 -> 2   L2     2933  L    NAVY   13.50      6.25       1989/05/28 0:00:00 16JUN89  15.50          9.25           
3 -> 3   L2     2933  L    NAVY   13.50      6.25       1989/05/28 0:00:00 16JUN89  13.50          6.25           
4 -> 4   L2     2934  M    NAVY   13.50      6.25       1989/05/28 0:00:00 INSTOCK  15.50          9.25           
5 -> 5   L2     2934  M    NAVY   13.50      6.25       1989/05/28 0:00:00 INSTOCK  13.50          6.25           
6 -> 6   L2     2967  S    BEIGE  18.75      8.25       1989/02/16 0:00:00 25MAR89  15.50          9.25           
7 -> 7   L2     2967  S    BEIGE  18.75      8.25       1989/02/16 0:00:00 25MAR89  13.50          6.25           
8 -> 8   P2     4201  L    BROWN  16.95      9.90       1989/05/18 0:00:00 16JUN89  15.50          9.25           
9 -> 9   P2     4201  L    BROWN  16.95      9.90       1989/05/18 0:00:00 16JUN89  13.50          6.25   
*)
