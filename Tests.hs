
module Main where

    import Compiler.Syntax
    import Compiler.Scanner
    import Compiler.Parser

    import Text.ParserCombinators.Parsec

    data Result = Correct
                  | ParseError ParseError
                  | Incorrect [Positioned Symbol]

    instance Show Result where
            show Correct = "correct"
            show (ParseError err) = "parse error: " ++ show err
            show (Incorrect actual) = "incorrect output: " ++ show actual

    tests :: [(String, [Positioned Symbol])]
    tests = [
     ("/* comment */ int a;",
      [AnyPosition $ VarSymbol (Variable Int "a")]),

     ("/* comment */ int a(void) { /* foo */ }",
      [AnyPosition $ FuncSymbol $ Function Int "a" [] (AnyPosition (CompoundStatement [] []))]),

     ("int a(void) { return; }",
      [AnyPosition $ FuncSymbol $ Function Int "a" []
                     (AnyPosition $ CompoundStatement [] [AnyPosition ReturnStatement])]),

     ("int a(void) { return 0; }",
      [AnyPosition $ FuncSymbol $ Function Int "a" []
                     (AnyPosition $ CompoundStatement []
                      [AnyPosition $ ValueReturnStatement (ValueExpr (IntValue 0))])]),

     ("int a(void) { return x; }",
      [AnyPosition $ FuncSymbol $ Function Int "a" []
                     (AnyPosition $ CompoundStatement []
                      [AnyPosition $ ValueReturnStatement (ValueExpr (VariableRef "x"))])]),


     ("int a(void) { a = x; }",
      [AnyPosition $ FuncSymbol $ Function Int "a" []
                     (AnyPosition $ CompoundStatement []
                      [AnyPosition $ ExpressionStatement
                       (AssignmentExpr (VariableRef "a")
                        (ValueExpr (VariableRef "x")))])]),

     ("int a(void) { a[1] = x; }",
      [AnyPosition $ FuncSymbol $ Function Int "a" []
                     (AnyPosition $ CompoundStatement []
                      [AnyPosition $ ExpressionStatement
                       (AssignmentExpr (ArrayRef "a" (ValueExpr (IntValue 1)))
                        (ValueExpr (VariableRef "x")))])]),

     ("int a(void) { a = 2*x+1; }",
      [AnyPosition $ FuncSymbol $ Function Int "a" []
                     (AnyPosition $ CompoundStatement []
                      [AnyPosition $ ExpressionStatement
                       (AssignmentExpr (VariableRef "a")
                        (ArithmeticExpr Add
                         (ArithmeticExpr Multiply
                          (ValueExpr (IntValue 2))
                          (ValueExpr (VariableRef "x")))
                         (ValueExpr (IntValue 1))))])]),


     ("int a(void) { a = x[0]; }",
      [AnyPosition $ FuncSymbol $ Function Int "a" []
                     (AnyPosition $ CompoundStatement []
                      [AnyPosition $ ExpressionStatement
                       (AssignmentExpr (VariableRef "a")
                        (ValueExpr (ArrayRef "x" (ValueExpr (IntValue 0)))))])]),

     ("int a(void) { int x; x = 0; }",
      [AnyPosition $ FuncSymbol $ Function Int "a" []
                     (AnyPosition $ CompoundStatement [AnyPosition $ Variable Int "x"]
                      [AnyPosition $ ExpressionStatement
                       (AssignmentExpr (VariableRef "x")
                        (ValueExpr (IntValue 0)))])]),

     ("int a(void) { while(1) { } }",
      [AnyPosition $ FuncSymbol $ Function Int "a" []
                     (AnyPosition $ CompoundStatement []
                      [AnyPosition $ IterationStatement (ValueExpr (IntValue 1))
                       (AnyPosition $ CompoundStatement [] [])])]),

     ("int a(void) { if(1) { } }",
      [AnyPosition $ FuncSymbol $ Function Int "a" []
                         (AnyPosition $ CompoundStatement []
                          [AnyPosition $ SelectionStatement (ValueExpr (IntValue 1))
                           (AnyPosition $ CompoundStatement [] [])
                           (AnyPosition NullStatement)])]),

     ("int a(void) { if(1) { } else { }}",
      [AnyPosition $ FuncSymbol $ Function Int "a" []
                         (AnyPosition $ CompoundStatement []
                          [AnyPosition $ SelectionStatement (ValueExpr (IntValue 1))
                           (AnyPosition $ CompoundStatement [] [])
                           (AnyPosition $ CompoundStatement [] [])])]),

     ("int a(void) { int a; }",
      [AnyPosition $ FuncSymbol $ Function Int "a" []
                         (AnyPosition $ CompoundStatement
                          [AnyPosition $ Variable Int "a"] 
                          [])]),

     ("int a(void) { int a; if(0) {} }",
      [AnyPosition $ FuncSymbol $ Function Int "a" []
                         (AnyPosition $ CompoundStatement
                          [AnyPosition $ Variable Int "a"]
                          [AnyPosition $ SelectionStatement (ValueExpr (IntValue 0))
                           (AnyPosition $ CompoundStatement [] [])
                           (AnyPosition $ NullStatement)])]),

     ("int a[10];",
      [AnyPosition $ VarSymbol (Variable (Array Int 10) "a")]),

     ("int a(int x) {}",
      [AnyPosition $ FuncSymbol $ Function Int "a" [AnyPosition $ Variable Int "x"]
       (AnyPosition $ CompoundStatement [] [])]),


     ("int a(int x[]) {}",
      [AnyPosition $ FuncSymbol $ Function Int "a" [AnyPosition $ Variable (Pointer Int) "x"]
       (AnyPosition $ CompoundStatement [] [])]),

     ("int a(int x[10]) {}",
      [AnyPosition $ FuncSymbol $ Function Int "a" [AnyPosition $ Variable (Array Int 10) "x"]
       (AnyPosition $ CompoundStatement [] [])]),

     ("void main(void) {}",
      [AnyPosition $ FuncSymbol $ Function Void "main" []
       (AnyPosition $ CompoundStatement [] [])]),

     ("void main(void) { int a; }",
      [AnyPosition $ FuncSymbol $ Function Void "main" []
       (AnyPosition $ CompoundStatement [AnyPosition $ Variable Int "a"] [])]),

     ("void main(void) { int a[10]; }",
      [AnyPosition $ FuncSymbol $ Function Void "main" []
       (AnyPosition $ CompoundStatement [AnyPosition $ Variable (Array Int 10) "a"] [])]),

     ("void main(void) { foo(2,3); }",
      [AnyPosition $ FuncSymbol $ Function Void "main" []
       (AnyPosition $ CompoundStatement []
        [AnyPosition $ ExpressionStatement
         (ValueExpr 
          (FunctionCall "foo"
           [ValueExpr (IntValue 2),
            ValueExpr (IntValue 3)]))])]),

     ("int x[10]; int minloc(int a[], int low, int high) { int e; int f; int g; }",
      [AnyPosition $ VarSymbol $ Variable (Array Int 10) "x",
       (AnyPosition $ FuncSymbol $ Function Int "a"
        [AnyPosition $ Variable (Pointer Int) "b",
         AnyPosition $ Variable Int "c",
         AnyPosition $ Variable Int "d"]
        (AnyPosition $ CompoundStatement
         [AnyPosition $ Variable Int "e",
          AnyPosition $ Variable Int "f",
          AnyPosition $ Variable Int "g"] []))])]


    executeTests xs = map execute xs
        where execute (code, expected) = (code, result)
                  where result = let out = parse program "" code
                                 in case out of
                                      Left err -> ParseError err
                                      Right (Program result) ->
                                          if result == expected
                                          then Correct
                                          else Incorrect result
                     
                  
    showResult (code, result) = code ++ ": " ++ (show result) ++ "\n"

    main = mapM putStr (map showResult (executeTests tests))