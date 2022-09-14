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

// プレフィックス付きのカラム名の例
run "use tandp"
run "q15_01 = restrict (auction) (auction.sell_price>purchase_price)"
run "print q15_01"
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

// 単一の式
run "use tandp"
run "res = restrict (auction) (true)"
// Failure: EvalError: sigle literal is not an conditional expression.

// 条件式がカラムの値がboolにキャストできない場合
run "use tandp"
run "res = restrict (auction) (date_bought)"
// Failure: EvalError: single column name is not an conditional expression.

// 条件式にbool以外のリテラルになっている場合
run "use tandp"
run "res = restrict (auction) (\"hello\")"
//Failure: TypeError: single literal is not a conditional expression.

// 条件式にbool以外のリテラルになっている場合
run "use tandp"
run "res = restrict (stock) (date_out = \"INSTOCK\" and 1)"
// Failure: TypeError: single literal is not a conditional expression.

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
     branch stock size colour sell_price cost_price date_in            date_out tmp.sell_price tmp.cost_price 
0 -> L2     2921  M    BLACK  25.00      15.20      1989/04/17 0:00:00 17APR89  15.50          9.25           
1 -> L2     2921  M    BLACK  25.00      15.20      1989/04/17 0:00:00 17APR89  13.50          6.25           
2 -> L2     2933  L    NAVY   13.50      6.25       1989/05/28 0:00:00 16JUN89  15.50          9.25           
3 -> L2     2933  L    NAVY   13.50      6.25       1989/05/28 0:00:00 16JUN89  13.50          6.25           
4 -> L2     2934  M    NAVY   13.50      6.25       1989/05/28 0:00:00 INSTOCK  15.50          9.25           
5 -> L2     2934  M    NAVY   13.50      6.25       1989/05/28 0:00:00 INSTOCK  13.50          6.25           
6 -> L2     2967  S    BEIGE  18.75      8.25       1989/02/16 0:00:00 25MAR89  15.50          9.25           
7 -> L2     2967  S    BEIGE  18.75      8.25       1989/02/16 0:00:00 25MAR89  13.50          6.25           
8 -> P2     4201  L    BROWN  16.95      9.90       1989/05/18 0:00:00 16JUN89  15.50          9.25           
9 -> P2     4201  L    BROWN  16.95      9.90       1989/05/18 0:00:00 16JUN89  13.50          6.25   
*)

// 17. joinの例
run "use wikipedia"
run "ex17_1 = join (Employee) (Dept) (Employee.DeptName = Dept.DeptName)"
run "print ex17_1"
(*
     Key Name    EmpId DeptName Dept.DeptName Manager 
0 -> 0   Harry   3415  Finance  Finance       George  
1 -> 4   Sally   2241  Sales    Sales         Harriet 
2 -> 6   George  3401  Finance  Finance       George  
3 -> 10  Harriet 2202  Sales    Sales         Harriet 
*)

// プレフィックスを省略した例
run "use wikipedia"
run "ex17_2 = join (Employee) (Dept) (DeptName = Dept.DeptName)"
run "print ex17_2"
(*
     Key Name    EmpId DeptName Dept.DeptName Manager 
0 -> 0   Harry   3415  Finance  Finance       George  
1 -> 4   Sally   2241  Sales    Sales         Harriet 
2 -> 6   George  3401  Finance  Finance       George  
3 -> 10  Harriet 2202  Sales    Sales         Harriet 
*)

// joinしてからproject取った例
run "use wikipedia"
run "ex17_3 = project join (Employee) (Dept) (DeptName = Dept.DeptName) DeptName, Dept.DeptName"
run "print ex17_3"
(*
     DeptName Dept.DeptName 
0 -> Finance  Finance       
1 -> Sales    Sales
*)

// joinを動かしてみる
// 4.3.1 商品を提供している全producerを調べよ
run "use tandp"
run "q17_1 = project (goods) producer"
run "print q17_1"
(*
     producer   
0 -> CLASSICS   
1 -> 60S CLOTHS 
2 -> MODERNA    
*)
// 4.3.2 支社に配送している全producerを調べよ
run "use tandp"
run "q17_2 = project (delivery) producer"
run "print q17_2"
(*
     producer   
0 -> CLASSICS   
1 -> 60S CLOTHS 
2 -> MODERNA
*)

// 4.3.3 L1支社に配送されてまだin stockな状態の全商品の、sell_priceとcost_priceを以下の２つの方法で調べよ
// P1にL1支社でin stockなrowの一覧を入れ、次にP1からsell_price, cost_priceを取り出して表示する、という２つのクエリに分けるやり方
run "use tandp"
run "P1 = restrict (stock) (branch = \"L1\" and date_out = \"INSTOCK\")"
run "print P1"
(*
     branch stock size colour sell_price cost_price date_in date_out 
0 -> L1     1004  M    WHITE  15.50      9.25       20DEC88 INSTOCK  
*)
run "q17_3 = project (P1) sell_price, cost_price"
run "print q17_3"
(*
     sell_price cost_price 
0 -> 15.50      9.25 
*)

//上と同じものをかっこを使ってネストして一文にしたやり方
run "use tandp"
run "q17_3_2 = project (restrict (stock) (branch = \"L1\" and date_out = \"INSTOCK\")) sell_price, cost_price"
run "print q17_3_2"
(*
     sell_price cost_price 
0 -> 15.50      9.25  
*)

// 4.3.4 以下の条件を満たすproducer, product_code, descriptionを表示せよ：
// 全てのブランチで、届いた日と同じ日に売れたもの。（以下ヒントを書くので、自分で好きに書いたあとにヒントの通りにも書いてみて下さい）
// まずは自分で好きに書いてみる。
run "use tandp"
// date_outに"INSTOCK"が混ざっているとタイプがStringになるので除去
run "q17_4_tmp0 = restrict (stock) (date_out <> \"INSTOCK\")" 
// q17_4_tmp: 届いた日と売れた日が同じ商品の商品コード
run "q17_4_tmp = project join (restrict (q17_4_tmp0) (date_in = date_out)) (delivery) (branch = delivery.branch and stock = delivery.stock) product_code"
run "print q17_4_tmp"
(*
     product_code 
0 -> WOODSTOCK    
1 -> FINESSE      
2 -> 199K   
*)
run "q17_4 = project join (q17_4_tmp) (goods) (q17_4_tmp.product_code = goods.product_code) producer, product_code, description"
run "print q17_4"
(*
     producer   product_code description 
0 -> 60S CLOTHS WOODSTOCK    JEANS       
1 -> 60S CLOTHS FINESSE      DRESS       
2 -> MODERNA    199K         JACKET 
*)

run "use tandp"
// r1にstockのうちdate_inとdate_outが等しいものだけを入れる
run "r0 = restrict (stock) (date_out <>\"INSTOCK\")"
// date_in, date_outが変換されてしまう
run "r1 = restrict (r0) (date_in = date_out)"
run "print r1"
(*
     branch stock size colour sell_price cost_price date_in            date_out           
0 -> L2     2921  M    BLACK  25.00      15.20      1989/04/17 0:00:00 1989/04/17 0:00:00 
1 -> P1     1026  L    BLUE   25.50      12.00      1989/05/12 0:00:00 1989/05/12 0:00:00 
2 -> NY     1020  L    RED    17.45      7.85       1989/04/19 0:00:00 1989/04/19 0:00:00 
*)
// r2でr1とdeliveryを、ブランチとストックが同じようにjoin
run "r2 = join (r1) (delivery) (r1.branch = delivery.branch and r1.stock = delivery.stock)"
run "print r2"
(*
     branch stock size colour sell_price cost_price date_in            date_out           producer   product_code delivery.branch delivery.stock 
0 -> L2     2921  M    BLACK  25.00      15.20      1989/04/17 0:00:00 1989/04/17 0:00:00 60S CLOTHS WOODSTOCK    L2              2921           
1 -> P1     1026  L    BLUE   25.50      12.00      1989/05/12 0:00:00 1989/05/12 0:00:00 60S CLOTHS FINESSE      P1              1026           
2 -> NY     1020  L    RED    17.45      7.85       1989/04/19 0:00:00 1989/04/19 0:00:00 MODERNA    199K         NY              1020           
*)
// r3にr2とgoodsをjoin
run "r3 = join (r2) (goods) (r2.product_code = goods.product_code)"
run "print r3"
(*
0 -> L2     2921  M    BLACK  25.00      15.20      1989/04/17 0:00:00 1989/04/17 0:00:00 60S CLOTHS WOODSTOCK    L2              2921           60S CLOTHS     WOODSTOCK          JEANS       
1 -> P1     1026  L    BLUE   25.50      12.00      1989/05/12 0:00:00 1989/05/12 0:00:00 60S CLOTHS FINESSE      P1              1026           60S CLOTHS     FINESSE            DRESS       
2 -> NY     1020  L    RED    17.45      7.85       1989/04/19 0:00:00 1989/04/19 0:00:00 MODERNA    199K         NY              1020           MODERNA        199K               JACKET  
*)
// r4でr3をproject
run "r4 = project (r3) producer, product_code, description"
run "print r4"
(*
     producer   product_code description 
0 -> 60S CLOTHS WOODSTOCK    JEANS       
1 -> 60S CLOTHS FINESSE      DRESS       
2 -> MODERNA    199K         JACKET 
*)

// このやり方は最後にprojetをやっているので効率が悪い。もっと早くprojectを行うようにクエリを直すとどうなるか？
run "use tandp"
run "r1 = project (restrict (project (stock) branch, stock, date_in, date_out) (date_in = date_out)) branch, stock"
run "r2 = project join (r1) (delivery) (r1.branch = delivery.branch and r1.stock = delivery.stock) product_code"
run "r3 = project join (r2) (goods) (r2.product_code = goods.product_code) producer, product_code, description"
run "print r3"
(*
     producer   product_code description 
0 -> 60S CLOTHS WOODSTOCK    JEANS       
1 -> 60S CLOTHS FINESSE      DRESS       
2 -> MODERNA    199K         JACKET   
*)

// 4.3.5 以下の条件を満たすbranch, size, colour, sell_priceの一覧を表示せよ：
// まだ売れてないdress全て
// branch, stock: goodsとjoinするのに必要
// branch, size, colur, cell_price: 最終結果に必要
run "r1 = project (restrict (stock) (date_out = \"INSTOCK\")) branch, stock, size, colour, sell_price"
run "print r1"
(*
     branch stock size colour sell_price 
0 -> L1     1004  M    WHITE  15.50      
1 -> L2     2934  M    NAVY   13.50   
*)

// product: goodsとjoinしてドレスに絞るのに必要
// branch, size, colour, cell_price: 最終結果に必要
run "r2 = project join (r1) (delivery) ((r1.branch = delivery.branch) and (r1.stock = delivery.stock)) product_code, branch, size, colour, sell_price"
run "print r2"
(*
     product_code branch size colour sell_price 
0 -> 403          L1     M    WHITE  15.50      
1 -> DI4          L2     M    NAVY   13.50  
*)
run "r3 = project join (restrict (goods) (description = \"DRESS\")) (r2) (product_code = r2.product_code) branch, size, colour, sell_price"
run "print r3"
(*
    branch size colour sell_price 
0 -> L2     M    NAVY   13.50 
*)

run "use master"
run "rename (シラバス.専門) 科目"
//rename後のRelation名を指定する
// run "print zzworc"

// Unionの例
run "use wikipedia"
run "emp_dept = project (Employee) DeptName"
run "dept_dept = project (Dept) DeptName"
run "r1 = (emp_dept) union (dept_dept)"
run "print r1"
(*
     DeptName        
0 -> Finance         
1 -> Sales           
2 -> Human Resources 
3 -> Production  
*)

// Intersectの例
run "use wikipedia"
run "emp_dept = project (Employee) DeptName"
run "dept_dept = project (Dept) DeptName"
run "r1 = (emp_dept) intersect (dept_dept)"
run "print r1"
(*
     DeptName 
0 -> Finance  
1 -> Sales   
*)