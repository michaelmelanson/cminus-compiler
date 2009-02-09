
module Main where

    import Compiler.Scanner
    import Compiler.Parser

    import Text.ParserCombinators.Parsec

    data Result = Correct
                  | ParseError ParseError
                  | Incorrect [Toplevel]

    instance Show Result where
            show Correct = "correct"
            show (ParseError err) = "parse error: " ++ show err
            show (Incorrect actual) = "incorrect output: " ++ show actual

    tests = [
     ("/* comment */ int a;",
      [ToplevelVar (Variable Int "a")]),

     ("int a(void) { /* foo */ }",
      [ToplevelFunc (Function Int "a" [] (CompoundStatement [] []))]),

     ("int a(void) { return; }",
      [ToplevelFunc (Function Int "a" []
                     (CompoundStatement [] [ReturnStatement]))]),

     ("int a(void) { return 0; }",
      [ToplevelFunc (Function Int "a" []
                     (CompoundStatement []
                      [ValueReturnStatement (ValueExpr (IntValue 0))]))]),

     ("int a(void) { return x; }",
      [ToplevelFunc (Function Int "a" []
                     (CompoundStatement []
                      [ValueReturnStatement (ValueExpr (VariableRef "x"))]))]),


     ("int a(void) { a = x; }",
      [ToplevelFunc (Function Int "a" []
                     (CompoundStatement []
                      [ExpressionStatement
                       (AssignmentExpr (VariableRef "a")
                        (ValueExpr (VariableRef "x")))]))]),

     ("int a(void) { a[1] = x; }",
      [ToplevelFunc (Function Int "a" []
                     (CompoundStatement []
                      [ExpressionStatement
                       (AssignmentExpr (ArrayRef "a" (ValueExpr (IntValue 1)))
                        (ValueExpr (VariableRef "x")))]))]),

     ("int a(void) { a = 2*x+1; }",
      [ToplevelFunc (Function Int "a" []
                     (CompoundStatement []
                      [ExpressionStatement
                       (AssignmentExpr (VariableRef "a")
                        (ArithmeticExpr Add
                         (ArithmeticExpr Multiply
                          (ValueExpr (IntValue 2))
                          (ValueExpr (VariableRef "x")))
                         (ValueExpr (IntValue 1))))]))]),


     ("int a(void) { a = x[0]; }",
      [ToplevelFunc (Function Int "a" []
                     (CompoundStatement []
                      [ExpressionStatement
                       (AssignmentExpr (VariableRef "a")
                        (ValueExpr (ArrayRef "x" (ValueExpr (IntValue 0)))))]))]),

     ("int a(void) { int x; x = 0; }",
      [ToplevelFunc (Function Int "a" []
                     (CompoundStatement [Variable Int "x"]
                      [ExpressionStatement
                       (AssignmentExpr (VariableRef "x")
                        (ValueExpr (IntValue 0)))]))]),

     ("int a(void) { while(1) { } }",
      [ToplevelFunc (Function Int "a" []
                     (CompoundStatement []
                      [IterationStatement (ValueExpr (IntValue 1))
                       (CompoundStatement [] [])]))]),

     ("int a(void) { if(1) { } }",
          [ToplevelFunc (Function Int "a" []
                         (CompoundStatement []
                          [SelectionStatement (ValueExpr (IntValue 1))
                           (CompoundStatement [] [])
                           NullStatement]))]),

     ("int a(void) { if(1) { } else { }}",
          [ToplevelFunc (Function Int "a" []
                         (CompoundStatement []
                          [SelectionStatement (ValueExpr (IntValue 1))
                           (CompoundStatement [] [])
                           (CompoundStatement [] [])]))]),

     ("int a(void) { int a; }",
          [ToplevelFunc (Function Int "a" []
                         (CompoundStatement
                          [Variable Int "a"] 
                          []))]),

     ("int a(void) { int a; if(0) {} }",
          [ToplevelFunc (Function Int "a" []
                         (CompoundStatement
                          [Variable Int "a"]
                          [SelectionStatement (ValueExpr (IntValue 0))
                           (CompoundStatement [] [])
                           NullStatement]))]),

     ("int a[10];",
         [ToplevelVar (Variable (Array Int 10) "a")]),


     ("int a(int x[]) {}",
         [ToplevelFunc (Function Int "a"
                        [Variable (Pointer Int) "x"]
                        (CompoundStatement [] []))]),

     ("void main(void) {}",
         [ToplevelFunc (Function Void "main"
                        []
                        (CompoundStatement [] []))]),

     ("void main(void) { foo(2,3); }",
         [ToplevelFunc (Function Void "main"
                        []
                        (CompoundStatement []
                         [ExpressionStatement
                          (ValueExpr 
                           (FunctionCall "foo"
                            [ValueExpr (IntValue 2),
                             ValueExpr (IntValue 3)]))]))])]


    executeTests xs = map execute xs
        where execute (code, expected) = (code, result)
                  where result = let out = parse program "" code
                                 in case out of
                                      Left err -> ParseError err
                                      Right result ->
                                          if result == expected
                                          then Correct
                                          else Incorrect result
                     
                  
    showResult (code, result) = code ++ ": " ++ (show result) ++ "\n"

    main = mapM putStr (map showResult (executeTests tests))