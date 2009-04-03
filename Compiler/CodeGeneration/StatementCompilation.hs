
module Compiler.CodeGeneration.StatementCompilation where
    import Control.Monad

    import Compiler.Syntax
    import Compiler.SymbolTable
    import Compiler.CodeGeneration.CompilationState
    import Compiler.CodeGeneration.ExpressionEvaluation
    import Compiler.CodeGeneration.InstructionSet
    import Compiler.CodeGeneration.RuntimeEnvironment

    import Debug.Trace

    class Compilable a where
        compile :: a -> Compiler () 

    instance Compilable Program where
        compile (Program syms) =
            withBlock $ do program_prelude
                           mapM compile syms
                           program_finale

    instance Compilable Symbol where
        compile (FuncSymbol _ f) = compile f
        compile _ = return ()

    instance Compilable Function where
        compile f@(Function t _ params block) =
            withFunction f params $ do compile block

    instance Compilable Statement where
        compile (ExpressionStatement (AssignmentExpr (VariableRef v) rhs)) = 
            do t1 <- evaluate rhs
               assign v t1
               freeRegister t1

        compile (ExpressionStatement (AssignmentExpr (ArrayRef name indexExpr) rhs)) = 
            do t1 <- evaluate indexExpr

               r <- claimRegister
               addr <- currentAddress
               emit $ LDA r (SymbolRef addr name) -- Get array base
               emit $ ADD r r t1 

               freeRegister t1

               t2 <- evaluate rhs
               emit $ ST t2 (Memory 0 r)

               freeRegister t2
               freeRegister r

        compile (ExpressionStatement e) = do evaluate e
                                             return ()


        compile (CompoundStatement locals stmts) =
            withBlock $ do allocate_locals (map deposition locals)
                           sequence_ $ map compile stmts

        compile (IterationStatement cond body) = 
            withBlock $ do label "condition"
                           if_false cond "after"
                           compile body
                           jump_to "condition"
                           label "after"

        compile (SelectionStatement cond then_clause else_clause) = 
            withBlock $ do if_false cond "else"
                           compile then_clause
                           jump_to "after"
                           label "else"
                           compile else_clause
                           label "after"
        
        compile (ValueReturnStatement expr) =
            do t1 <- evaluate expr
               return_register t1

        compile ReturnStatement = do return_void

        compile NullStatement = return ()


    -- Strip off positioning information when assembling
    instance (Compilable a, Show a) => Compilable (Positioned a) where
        compile (Positioned _ a) = compile a
        compile (AnyPosition a) = compile a



    if_false (ArithmeticExpr op lhs rhs) label = 
        do t1 <- evaluate lhs
           t2 <- evaluate rhs

           emit $ OUT t1
           emit $ OUT t2

           t3 <- claimRegister   -- get a place to keep the result
           emit $ SUB t3 t1 t2   -- do the comparison

           freeRegister t1
           freeRegister t2

           a <- currentAddress
           emit $ (toInst op) t3 (LabelRef a label)
           freeRegister t3

        where toInst Less           = JGE
              toInst LessOrEqual    = JGT
              toInst Greater        = JLE
              toInst GreaterOrEqual = JLT
              toInst Equal          = JNE
              toInst NotEqual       = JEQ
    
