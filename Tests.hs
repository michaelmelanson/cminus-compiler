
module Main where

    import Compiler.Syntax
    import Compiler.Scanner
    import Compiler.Parser
    import Compiler.CodeGeneration.InstructionSet
    import Compiler.CodeGeneration.StatementCompilation
    import Compiler.CodeGeneration.CompilationState

    import Text.ParserCombinators.Parsec

    data Result = Correct
                  | ParseError      String ParseError
                  | IncorrectAST    String [Positioned Symbol]
                  | IncorrectInstrs [Positioned Symbol] [Instruction]

    instance Show Result where
            show Correct = "correct\n"
            show (ParseError code err) = show code ++ ": parse error: " ++ show err ++ "\n"
            show (IncorrectAST code actual) = code ++ ": incorrect tree " ++ show actual ++ "\n"
            show (IncorrectInstrs code actual) = show code ++ ": incorrect instructions:\n" ++ unlines (map show actual) ++ "\n"

    syntax_tests :: [(String, [Positioned Symbol])]
    syntax_tests = [
     ("/* comment */ int a;",
      [AnyPosition $ VarSymbol Unknown (Variable Int "a")]),

     ("/* comment */ int a(void) { /* foo */ }",
      [AnyPosition $ FuncSymbol Unknown $ Function Int "a" [] (AnyPosition (CompoundStatement [] []))]),

     ("int a(void) { return; }",
      [AnyPosition $ FuncSymbol Unknown $ Function Int "a" []
                     (AnyPosition $ CompoundStatement [] [AnyPosition ReturnStatement])]),

     ("int a(void) { return 0; }",
      [AnyPosition $ FuncSymbol Unknown $ Function Int "a" []
                     (AnyPosition $ CompoundStatement []
                      [AnyPosition $ ValueReturnStatement (ValueExpr (IntValue 0))])]),

     ("int a(void) { return x; }",
      [AnyPosition $ FuncSymbol Unknown $ Function Int "a" []
                     (AnyPosition $ CompoundStatement []
                      [AnyPosition $ ValueReturnStatement (ValueExpr (VariableRef "x"))])]),


     ("int a(void) { a = x; }",
      [AnyPosition $ FuncSymbol Unknown $ Function Int "a" []
                     (AnyPosition $ CompoundStatement []
                      [AnyPosition $ ExpressionStatement
                       (AssignmentExpr (VariableRef "a")
                        (ValueExpr (VariableRef "x")))])]),

     ("int a(void) { a[1] = x; }",
      [AnyPosition $ FuncSymbol Unknown $ Function Int "a" []
                     (AnyPosition $ CompoundStatement []
                      [AnyPosition $ ExpressionStatement
                       (AssignmentExpr (ArrayRef "a" (ValueExpr (IntValue 1)))
                        (ValueExpr (VariableRef "x")))])]),

     ("int a(void) { a = 2*x+1; }",
      [AnyPosition $ FuncSymbol Unknown $ Function Int "a" []
                     (AnyPosition $ CompoundStatement []
                      [AnyPosition $ ExpressionStatement
                       (AssignmentExpr (VariableRef "a")
                        (ArithmeticExpr Add
                         (ArithmeticExpr Multiply
                          (ValueExpr (IntValue 2))
                          (ValueExpr (VariableRef "x")))
                         (ValueExpr (IntValue 1))))])]),


     ("int a(void) { a = x[0]; }",
      [AnyPosition $ FuncSymbol Unknown $ Function Int "a" []
                     (AnyPosition $ CompoundStatement []
                      [AnyPosition $ ExpressionStatement
                       (AssignmentExpr (VariableRef "a")
                        (ValueExpr (ArrayRef "x" (ValueExpr (IntValue 0)))))])]),

     ("int a(void) { int x; x = 0; }",
      [AnyPosition $ FuncSymbol Unknown $ Function Int "a" []
                     (AnyPosition $ CompoundStatement [AnyPosition $ Variable Int "x"]
                      [AnyPosition $ ExpressionStatement
                       (AssignmentExpr (VariableRef "x")
                        (ValueExpr (IntValue 0)))])]),

     ("int a(void) { while(1) { } }",
      [AnyPosition $ FuncSymbol Unknown $ Function Int "a" []
                     (AnyPosition $ CompoundStatement []
                      [AnyPosition $ IterationStatement (ValueExpr (IntValue 1))
                       (AnyPosition $ CompoundStatement [] [])])]),

     ("int a(void) { if(1) { } }",
      [AnyPosition $ FuncSymbol Unknown $ Function Int "a" []
                         (AnyPosition $ CompoundStatement []
                          [AnyPosition $ SelectionStatement (ValueExpr (IntValue 1))
                           (AnyPosition $ CompoundStatement [] [])
                           (AnyPosition NullStatement)])]),

     ("int a(void) { if(1) { } else { }}",
      [AnyPosition $ FuncSymbol Unknown $ Function Int "a" []
                         (AnyPosition $ CompoundStatement []
                          [AnyPosition $ SelectionStatement (ValueExpr (IntValue 1))
                           (AnyPosition $ CompoundStatement [] [])
                           (AnyPosition $ CompoundStatement [] [])])]),

     ("int a(void) { int a; }",
      [AnyPosition $ FuncSymbol Unknown $ Function Int "a" []
                         (AnyPosition $ CompoundStatement
                          [AnyPosition $ Variable Int "a"] 
                          [])]),

     ("int a(void) { int a; if(0) {} }",
      [AnyPosition $ FuncSymbol Unknown $ Function Int "a" []
                         (AnyPosition $ CompoundStatement
                          [AnyPosition $ Variable Int "a"]
                          [AnyPosition $ SelectionStatement (ValueExpr (IntValue 0))
                           (AnyPosition $ CompoundStatement [] [])
                           (AnyPosition $ NullStatement)])]),

     ("int a[10];",
      [AnyPosition $ VarSymbol Unknown (Variable (Array Int 10) "a")]),

     ("int a(int x) {}",
      [AnyPosition $ FuncSymbol Unknown $ Function Int "a" [AnyPosition $ Variable Int "x"]
       (AnyPosition $ CompoundStatement [] [])]),


     ("int a(int x[]) {}",
      [AnyPosition $ FuncSymbol Unknown $ Function Int "a"
                       [AnyPosition $ Variable (Pointer Int) "x"]
       (AnyPosition $ CompoundStatement [] [])]),

     ("int a(int x[10]) {}",
      [AnyPosition $ FuncSymbol Unknown $ Function Int "a" [AnyPosition $ Variable (Array Int 10) "x"]
       (AnyPosition $ CompoundStatement [] [])]),

     ("void main(void) {}",
      [AnyPosition $ FuncSymbol Unknown $ Function Void "main" []
       (AnyPosition $ CompoundStatement [] [])]),

     ("void main(void) { int a; }",
      [AnyPosition $ FuncSymbol Unknown $ Function Void "main" []
       (AnyPosition $ CompoundStatement [AnyPosition $ Variable Int "a"] [])]),

     ("void main(void) { int a[10]; }",
      [AnyPosition $ FuncSymbol Unknown $ Function Void "main" []
       (AnyPosition $ CompoundStatement [AnyPosition $ Variable (Array Int 10) "a"] [])]),

     ("void main(void) { foo(2,3); }",
      [AnyPosition $ FuncSymbol Unknown $ Function Void "main" []
       (AnyPosition $ CompoundStatement []
        [AnyPosition $ ExpressionStatement
         (ValueExpr 
          (FunctionCall "foo"
           [ValueExpr (IntValue 2),
            ValueExpr (IntValue 3)]))])]),

     ("int x[10]; int minloc(int a[], int low, int high) { int e; int f; int g; }",
      [AnyPosition $ VarSymbol Unknown $ Variable (Array Int 10) "x",
       (AnyPosition $ FuncSymbol Unknown $ Function Int "minloc"
        [AnyPosition $ Variable (Pointer Int) "a",
         AnyPosition $ Variable Int "low",
         AnyPosition $ Variable Int "high"]
        (AnyPosition $ CompoundStatement
         [AnyPosition $ Variable Int "e",
          AnyPosition $ Variable Int "f",
          AnyPosition $ Variable Int "g"] []))])]


    generation_tests :: [([Positioned Symbol], [Instruction])]
    generation_tests = [
             ([AnyPosition $ FuncSymbol Unknown $ Function Int "gcd"
                               [AnyPosition $ Variable Int "u",
                                AnyPosition $ Variable Int "v"]
               (AnyPosition $ CompoundStatement []
                [AnyPosition $ SelectionStatement (ArithmeticExpr Equal (ValueExpr (VariableRef "v"))
                                                                        (ValueExpr (IntValue 0)))
                 (AnyPosition $ ValueReturnStatement (ValueExpr (VariableRef "u")))
                 (AnyPosition $ ValueReturnStatement $ ValueExpr $
                  FunctionCall "gcd"
                  [ValueExpr (VariableRef "v"),
                   ArithmeticExpr Subtract (ValueExpr (VariableRef "u"))
                                           (ArithmeticExpr Divide (ValueExpr (VariableRef "u"))
                                                                  (ArithmeticExpr Multiply (ValueExpr (VariableRef "v"))
                                                                                           (ValueExpr (VariableRef "v"))))])
                ])],
              [])
            ]


    executeSyntaxTests xs = map execute xs
        where execute (code, expected) = let out = parse program "" code
                                         in case out of
                                              Left err -> ParseError code err
                                              Right (Program result) ->
                                                  if result == expected
                                                  then Correct
                                                  else IncorrectAST code result

    executeGenerationTests xs = map execute xs
        where execute (code, expected) = let output = withCompiler $ compile $ Program code
                                         in if output == expected
                                            then Correct
                                            else IncorrectInstrs code output

    main = mapM putStr (map show results)
        where results = (executeSyntaxTests     syntax_tests    ) ++
                        (executeGenerationTests generation_tests)