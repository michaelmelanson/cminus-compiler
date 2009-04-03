module Compiler.CodeGeneration.ExpressionEvaluation(evaluate) where
    import Compiler.CodeGeneration.CompilationState
    import Compiler.CodeGeneration.InstructionSet
    import Compiler.CodeGeneration.RuntimeEnvironment
    import Compiler.SymbolTable
    import Compiler.Syntax

    evaluate :: Expression -> Compiler Register
    evaluate (ArithmeticExpr op lhs rhs) = 
        do t1 <- evaluate lhs
           t2 <- evaluate rhs

           t3 <- claimRegister
           emit $ inst t3 t1 t2
                
           freeRegister t1
           freeRegister t2

           emit $ OUT t3
           return t3

        where inst = operToInst op

    -- |A direct variable reference
    evaluate (ValueExpr (VariableRef v)) =
        do t1 <- claimRegister
           a <- currentAddress
           emit $ LD t1 (SymbolRef a v)
           return t1

    evaluate (ValueExpr (IntValue x)) =
        do t1 <- claimRegister
           emit $ LDC t1 x
           return t1

    evaluate (ValueExpr (ArrayRef name indexExpr)) =
        do 
           -- Evaluate the index
           t1 <- evaluate indexExpr

           -- Get the array base address. We calculate this after
           -- evaluating the index because that may use many registers
           -- and we don't want to tie up one for this.
           t2 <- claimRegister
           a <- currentAddress 
           emit $ LDA t2 (SymbolRef a name)  
                 
           -- Get a pointer to the referenced element
           t3 <- claimRegister
           emit $ ADD t3 t2 t1

           freeRegister t1
           freeRegister t2

           -- Get the element
           t4 <- claimRegister
           emit $ LDA t4 (Memory 0 t3)

           freeRegister t3

           return t4

    evaluate (ValueExpr (FunctionCall name argExprs)) = 
        withBlockRet $ do
          frameBase <- currentStackPointer
          incrementStackPointer 3
          comment ("setting up function call to " ++ show name ++ " with frame base " ++ show frameBase)
          -- Evaluate each of the arguments and push them to the stack
          sequence_ $ map processArg argExprs

          -- Call the function
          symbols <- currentSymbolTable
          if functionReturnsVoid name symbols
            then do call_void_function name frameBase
                    return 0     -- this will never be used

            else do result <- call_value_function name frameBase
                    return result

        where processArg arg = do r <- evaluate arg
                                  push_arg r
                                  freeRegister r


    -- Catch any missing cases above
    evaluate x = error ("I don't know how to evaluate " ++ show x)

    operToInst Add      = ADD
    operToInst Subtract = SUB
    operToInst Divide   = DIV
    operToInst Multiply = MUL

