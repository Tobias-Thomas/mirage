open Helpers

(* Statements *)

let%expect_test "empty program" =
  print_program "";
  [%expect {| { stmts = []; observations = [] } |}]

let%expect_test "deterministic assignment" =
  print_program "x = 1;";
  [%expect
    {| { stmts = [(LetDet ("x", (Literal (Discrete 1))))]; observations = [] } |}]

let%expect_test "random assignment" =
  print_program "x ~ Uniform(0.0, 1.0);";
  [%expect
    {|
    { stmts =
      [(LetRand ("x",
          (PUniform ((Literal (Continuous 0.)), (Literal (Continuous 1.))))))
        ];
      observations = [] }
    |}]

let%expect_test "observe" =
  print_program "observe x 2.3;";
  print_program "observe n 3;";
  print_program "observe c true;";
  [%expect
    {|
    { stmts = []; observations = [("x", (Continuous 2.3))] }
    { stmts = []; observations = [("n", (Discrete 3))] }
    { stmts = []; observations = [("c", (Binary true))] }
    |}]

let%expect_test "observe a negative value" =
  print_program "observe y -1.5;";
  [%expect {| { stmts = []; observations = [("y", (Continuous -1.5))] } |}]

let%expect_test "multiple statements" =
  print_program "p ~ Uniform(0.0, 1.0);\nc ~ Bernoulli(p);\nobserve c true;";
  [%expect
    {|
    { stmts =
      [(LetRand ("p",
          (PUniform ((Literal (Continuous 0.)), (Literal (Continuous 1.))))));
        (LetRand ("c", (PBernoulli (Var "p"))))];
      observations = [("c", (Binary true))] }
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
  print_dist "Uniform(0.0, 1.0)";
  print_dist "Exponential(2.0)";
  print_dist "Bernoulli(0.3)";
  print_dist "Normal(0.0, 1.0)";
  print_dist "Gaussian(0.0, 1.0)";
  print_dist "Gamma(2.0, 3.0)";
  print_dist "Beta(0.5, 0.5)";
  [%expect
    {|
    (PUniform ((Literal (Continuous 0.)), (Literal (Continuous 1.))))
    (PExponential (Literal (Continuous 2.)))
    (PBernoulli (Literal (Continuous 0.3)))
    (PNormal ((Literal (Continuous 0.)), (Literal (Continuous 1.))))
    (PNormal ((Literal (Continuous 0.)), (Literal (Continuous 1.))))
    (PGamma ((Literal (Continuous 2.)), (Literal (Continuous 3.))))
    (PBeta ((Literal (Continuous 0.5)), (Literal (Continuous 0.5))))
    |}]

let%expect_test "distribution arguments can be expressions" =
  print_dist "Uniform(mu + 1.0, 2.0 * s)";
  print_dist "Bernoulli((p))";
  [%expect
    {|
    (PUniform ((BinOp (Add, (Var "mu"), (Literal (Continuous 1.)))),
       (BinOp (Mul, (Literal (Continuous 2.)), (Var "s")))))
    (PBernoulli (Var "p"))
    |}]

let%expect_test "unary minus" =
  print_expr "-1.0";
  print_program "x ~ Uniform(-1.0, 1.0);";
  [%expect
    {|
    (UnOp (Neg, (Literal (Continuous 1.))))
    { stmts =
      [(LetRand ("x",
          (PUniform ((UnOp (Neg, (Literal (Continuous 1.)))),
             (Literal (Continuous 1.))))
          ))
        ];
      observations = [] }
    |}]

let%expect_test "unary minus binds tighter than binary operators" =
  print_expr "-2 * 3";
  print_expr "-x + 1";
  [%expect
    {|
    (BinOp (Mul, (UnOp (Neg, (Literal (Discrete 2)))), (Literal (Discrete 3))))
    (BinOp (Add, (UnOp (Neg, (Var "x"))), (Literal (Discrete 1))))
    |}]

let%expect_test "unary minus on compound operands" =
  print_expr "--x";
  print_expr "-(a + b)";
  [%expect
    {|
    (UnOp (Neg, (UnOp (Neg, (Var "x")))))
    (UnOp (Neg, (BinOp (Add, (Var "a"), (Var "b")))))
    |}]

let%expect_test "unary minus next to binary minus" =
  print_expr "1 - -2";
  print_expr "x-1";
  [%expect
    {|
    (BinOp (Sub, (Literal (Discrete 1)), (UnOp (Neg, (Literal (Discrete 2))))))
    (BinOp (Sub, (Var "x"), (Literal (Discrete 1))))
    |}]

(* Errors *)

let%expect_test "missing semicolon" =
  print_program "x = 1";
  [%expect {| error: Failure("wrong token") |}]

let%expect_test "missing right-hand side" =
  print_program "x = ;";
  [%expect {| error: Failure("wrong token") |}]

let%expect_test "non-distribution after ~" =
  print_program "x ~ 1;";
  [%expect {| error: Failure("expected a Distribution name") |}]

let%expect_test "distribution after =" =
  print_program "x = Uniform(0.0, 1.0);";
  [%expect {| error: Failure("wrong token") |}]

let%expect_test "unclosed parenthesis" =
  print_program "x = (1 + 2;";
  [%expect {| error: Failure("wrong token") |}]

let%expect_test "missing comma in arguments" =
  print_program "x ~ Uniform(1 2);";
  [%expect {| error: Failure("wrong token") |}]

let%expect_test "unknown distribution" =
  print_program "x ~ Foo();";
  print_program "x ~ Poisson(1.0);";
  print_program "x ~ uniform(0.0, 1.0);";
  [%expect
    {|
    error: Failure("unknown distribution name")
    error: Failure("unknown distribution name")
    error: Failure("expected a Distribution name")
    |}]

let%expect_test "wrong number of distribution arguments" =
  print_program "x ~ Uniform(1.0);";
  print_program "x ~ Uniform(0.0, 1.0, 2.0);";
  print_program "x ~ Exponential();";
  print_program "x ~ Bernoulli(0.1, 0.2);";
  [%expect
    {|
    error: Failure("wrong token")
    error: Failure("wrong token")
    error: Failure("wrong token")
    error: Failure("wrong token")
    |}]

let%expect_test "malformed observations" =
  print_program "observe;";
  print_program "observe x;";
  print_program "observe 2.3;";
  print_program "observe x y;";
  print_program "observe x 1 + 2;";
  [%expect
    {|
    error: Failure("expected string but got other token")
    error: Failure("wrong token")
    error: Failure("expected string but got other token")
    error: Failure("The observed value must be a concrete value")
    error: Failure("wrong token")
    |}]

let%expect_test "statement starting with a literal" =
  print_program "1 = x;";
  [%expect {| error: Failure("wrong token") |}]
