open Helpers

(* Statements *)

let%expect_test "empty program" =
  print_program "";
  [%expect {| [] |}]

let%expect_test "deterministic assignment" =
  print_program "x = 1;";
  [%expect {| [(LetDet ("x", (Literal (Discrete 1))))] |}]

let%expect_test "random assignment" =
  print_program "x ~ Uniform(0.0, 1.0);";
  [%expect
    {|
    [(LetRand ("x",
        (Dist ("Uniform", [(Literal (Continuous 0.)); (Literal (Continuous 1.))]
           ))
        ))
      ]
    |}]

let%expect_test "observe" =
  print_program "observe x > 0.5;";
  [%expect
    {| [(Observe (BinOp (Gt, (Var "x"), (Literal (Continuous 0.5)))))] |}]

let%expect_test "sample" =
  print_program "sample x;";
  [%expect {| [(Sample "x")] |}]

let%expect_test "multiple statements" =
  print_program "p ~ Uniform(0.0, 1.0);\nc ~ Bernoulli(p);\nsample p;";
  [%expect
    {|
    [(LetRand ("p",
        (Dist ("Uniform", [(Literal (Continuous 0.)); (Literal (Continuous 1.))]
           ))
        ));
      (LetRand ("c", (Dist ("Bernoulli", [(Var "p")])))); (Sample "p")]
    |}]

(* Expressions *)

let%expect_test "literals and variables" =
  print_expr "1";
  print_expr "1.5";
  print_expr "true";
  print_expr "y";
  [%expect
    {|
    (Literal (Discrete 1))
    (Literal (Continuous 1.5))
    (Literal (Binary true))
    (Var "y")
    |}]

let%expect_test "* binds tighter than +" =
  print_expr "1 + 2 * 3";
  print_expr "1 * 2 + 3";
  [%expect
    {|
    (BinOp (Add, (Literal (Discrete 1)),
       (BinOp (Mul, (Literal (Discrete 2)), (Literal (Discrete 3))))))
    (BinOp (Add, (BinOp (Mul, (Literal (Discrete 1)), (Literal (Discrete 2)))),
       (Literal (Discrete 3))))
    |}]

let%expect_test "parentheses override precedence" =
  print_expr "(1 + 2) * 3";
  [%expect
    {|
    (BinOp (Mul, (BinOp (Add, (Literal (Discrete 1)), (Literal (Discrete 2)))),
       (Literal (Discrete 3))))
    |}]

let%expect_test "chained + and -" =
  print_expr "1 + 2 + 3";
  print_expr "1 - 2 - 3";
  print_expr "1 + 2 - 3";
  [%expect
    {|
    (BinOp (Add, (BinOp (Add, (Literal (Discrete 1)), (Literal (Discrete 2)))),
       (Literal (Discrete 3))))
    (BinOp (Sub, (BinOp (Sub, (Literal (Discrete 1)), (Literal (Discrete 2)))),
       (Literal (Discrete 3))))
    (BinOp (Sub, (BinOp (Add, (Literal (Discrete 1)), (Literal (Discrete 2)))),
       (Literal (Discrete 3))))
    |}]

let%expect_test "chained * and /" =
  print_expr "1 * 2 * 3";
  print_expr "8 / 4 / 2";
  [%expect
    {|
    (BinOp (Mul, (BinOp (Mul, (Literal (Discrete 1)), (Literal (Discrete 2)))),
       (Literal (Discrete 3))))
    (BinOp (Div, (BinOp (Div, (Literal (Discrete 8)), (Literal (Discrete 4)))),
       (Literal (Discrete 2))))
    |}]

let%expect_test "sum of products" =
  print_expr "1 * 2 + 3 * 4";
  [%expect
    {|
    (BinOp (Add, (BinOp (Mul, (Literal (Discrete 1)), (Literal (Discrete 2)))),
       (BinOp (Mul, (Literal (Discrete 3)), (Literal (Discrete 4))))))
    |}]

let%expect_test "comparison has lowest precedence" =
  print_expr "a + 1 > b * 2";
  [%expect
    {|
    (BinOp (Gt, (BinOp (Add, (Var "a"), (Literal (Discrete 1)))),
       (BinOp (Mul, (Var "b"), (Literal (Discrete 2))))))
    |}]

let%expect_test "equality" =
  print_expr "a == 1";
  [%expect {| (BinOp (Eq, (Var "a"), (Literal (Discrete 1)))) |}]

let%expect_test "distributions" =
  print_expr "Foo()";
  print_expr "Normal(mu + 1.0, 2.0 * s)";
  [%expect
    {|
    (Dist ("Foo", []))
    (Dist ("Normal",
       [(BinOp (Add, (Var "mu"), (Literal (Continuous 1.))));
         (BinOp (Mul, (Literal (Continuous 2.)), (Var "s")))]
       ))
    |}]

(* The expected output depends on how you represent negation in the AST, so
   it is left empty: once it parses, check the output and run [dune promote]. *)
let%expect_test "unary minus" =
  print_expr "-1.0";
  print_program "x ~ Normal(-1.0, 1.0);";
  [%expect {||}]

(* Errors *)

let%expect_test "missing semicolon" =
  print_program "x = 1";
  [%expect {| error: Failure("wrong token") |}]

let%expect_test "missing right-hand side" =
  print_program "x = ;";
  [%expect {| error: Failure("wrong token") |}]

let%expect_test "non-distribution after ~" =
  print_program "x ~ 1;";
  [%expect {| error: Failure("expected a dist on the rhs of ~") |}]

let%expect_test "unclosed parenthesis" =
  print_program "x = (1 + 2;";
  [%expect {| error: Failure("wrong token") |}]

let%expect_test "missing comma in arguments" =
  print_program "x ~ Uniform(1 2);";
  [%expect {| error: Failure("expected , or ) but got other token") |}]

let%expect_test "observe without expression" =
  print_program "observe;";
  [%expect {| error: Failure("wrong token") |}]

let%expect_test "statement starting with a literal" =
  print_program "1 = x;";
  [%expect {| error: Failure("wrong token") |}]
