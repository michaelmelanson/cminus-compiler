
module Main where

    import Compiler.Syntax
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
      [GlobalVariable (Variable Int "a")]),

     ("int a(void) { /* foo */ }",
      [Function Int "a" [] (CompoundStatement [] [])]),

     ("int a(void) { return; }",
      [Function Int "a" []
                     (CompoundStatement [] [ReturnStatement])]),

     ("int a(void) { return 0; }",
      [Function Int "a" []
                     (CompoundStatement []
                      [ValueReturnStatement (ValueExpr (IntValue 0))])]),

     ("int a(void) { return x; }",
      [Function Int "a" []
                     (CompoundStatement []
                      [ValueReturnStatement (ValueExpr (VariableRef "x"))])]),


     ("int a(void) { a = x; }",
      [Function Int "a" []
                     (CompoundStatement []
                      [ExpressionStatement
                       (AssignmentExpr (VariableRef "a")
                        (ValueExpr (VariableRef "x")))])]),

     ("int a(void) { a[1] = x; }",
      [Function Int "a" []
                     (CompoundStatement []
                      [ExpressionStatement
                       (AssignmentExpr (ArrayRef "a" (ValueExpr (IntValue 1)))
                        (ValueExpr (VariableRef "x")))])]),

     ("int a(void) { a = 2*x+1; }",
      [Function Int "a" []
                     (CompoundStatement []
                      [ExpressionStatement
                       (AssignmentExpr (VariableRef "a")
                        (ArithmeticExpr Add
                         (ArithmeticExpr Multiply
                          (ValueExpr (IntValue 2))
                          (ValueExpr (VariableRef "x")))
                         (ValueExpr (IntValue 1))))])]),


     ("int a(void) { a = x[0]; }",
      [Function Int "a" []
                     (CompoundStatement []
                      [ExpressionStatement
                       (AssignmentExpr (VariableRef "a")
                        (ValueExpr (ArrayRef "x" (ValueExpr (IntValue 0)))))])]),

     ("int a(void) { int x; x = 0; }",
      [Function Int "a" []
                     (CompoundStatement [Variable Int "x"]
                      [ExpressionStatement
                       (AssignmentExpr (VariableRef "x")
                        (ValueExpr (IntValue 0)))])]),

     ("int a(void) { while(1) { } }",
      [Function Int "a" []
                     (CompoundStatement []
                      [IterationStatement (ValueExpr (IntValue 1))
                       (CompoundStatement [] [])])]),

     ("int a(void) { if(1) { } }",
      [Function Int "a" []
                         (CompoundStatement []
                          [SelectionStatement (ValueExpr (IntValue 1))
                           (CompoundStatement [] [])
                           NullStatement])]),

     ("int a(void) { if(1) { } else { }}",
      [Function Int "a" []
                         (CompoundStatement []
                          [SelectionStatement (ValueExpr (IntValue 1))
                           (CompoundStatement [] [])
                           (CompoundStatement [] [])])]),

     ("int a(void) { int a; }",
      [Function Int "a" []
                         (CompoundStatement
                          [Variable Int "a"] 
                          [])]),

     ("int a(void) { int a; if(0) {} }",
      [Function Int "a" []
                         (CompoundStatement
                          [Variable Int "a"]
                          [SelectionStatement (ValueExpr (IntValue 0))
                           (CompoundStatement [] [])
                           NullStatement])]),

     ("int a[10];",
      [GlobalVariable (Variable (Array Int 10) "a")]),


     ("int a(int x[]) {}",
      [Function Int "a" [Variable (Pointer Int) "x"]
       (CompoundStatement [] [])]),

     ("void main(void) {}",
      [Function Void "main" []
       (CompoundStatement [] [])]),

     ("void main(void) { foo(2,3); }",
      [Function Void "main" []
       (CompoundStatement []
        [ExpressionStatement
         (ValueExpr 
          (FunctionCall "foo"
           [ValueExpr (IntValue 2),
            ValueExpr (IntValue 3)]))])])]


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