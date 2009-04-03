
module Compiler.CodeGeneration.RuntimeEnvironment where
    import Compiler.Syntax
    import Compiler.SymbolTable
    import Compiler.CodeGeneration.CompilationState
    import Compiler.CodeGeneration.InstructionSet
    import Compiler.TypeChecking.Nameable

    frame = Memory (-1) fp

    assign name reg   = do addr <- currentAddress
                           comment ("assigning to " ++ show name)
                           emit $ ST reg (SymbolRef addr name)

    save_frame        = emit $ ST ac frame
    restore_frame     = emit $ LD pc frame
    jump_to label     = do a <- currentAddress
                           comment ("jump to label " ++ show label ++
                                    " (this is address " ++ show a ++ ")")
                           emit $ LD pc (LabelRef a label)
    return_register r = do comment "stash the return value in the stack"
                           emit $ ST r (Memory 0 fp)
                           return_void
    return_void = do comment "return to caller"
                     emit $ LD pc (Memory (-1) fp)

    push_arg r        = do comment ("push function arg in register " ++ show r)
                           pos <- allocateStack 1
                           case pos of 
                             Stack off -> emit $ ST r (Memory off fp)

    call_void_function name frameBase = 
        do 
          comment ("call function " ++ name ++ " with frame base " ++ show frameBase)
          r <- claimRegister
          emit $ LDA r  (Memory 4 pc)
          emit $ ST  r  (Memory (frameBase-1) fp) -- push return address
          freeRegister r
          emit $ ST  fp (Memory (frameBase-2) fp) -- push ofp
          emit $ LDA fp (Memory frameBase fp) -- push frame


          a <- currentAddress
          comment "jump to function body"
          emit $ LDA pc (SymbolRef a name) -- jump to fun loc
          emit $ LD  fp (Memory (-2) fp)    -- pop frame

    call_value_function name frameBase =
        do 
          -- Call the function as if it were void
          call_void_function name frameBase

          -- Get its return value
          r <- claimRegister
          emit $ LD r (Memory frameBase fp)
          return r

    program_prelude = do emit $ LD  gp (Memory 0 ac)
                         emit $ LDA fp (Memory 0 gp)
                         emit $ ST  ac (Memory 0 ac)
                         call_void_function "main" 0
                         emit $ HALT

                         comment "-> the input routine"
                         declare_function (Function Void "input" [] (AnyPosition $ CompoundStatement [] []))
                         r <- claimRegister
                         emit $ IN r
                         return_register r
                         freeRegister r
                         comment "<- the input routine"

                         comment "-> the output routine"
                         declare_function (Function Void "output" [AnyPosition $ Variable Int "value"] (AnyPosition $ CompoundStatement [] []))
                         r <- claimRegister
                         emit $ LD  r (Memory (-3) fp)
                         emit $ OUT r
                         freeRegister r
                         return_void
                         comment "<- the output routine"

    program_finale  = return () :: Compiler ()

    allocateStack words = do off <- currentStackPointer
                             incrementStackPointer words
                             return $ Stack off

    positionSymbol pos (FuncSymbol _ f) = FuncSymbol pos f
    positionSymbol pos (VarSymbol  _ v) = VarSymbol pos v

    declare_function :: Function -> Compiler ()
    declare_function f = do addr <- currentAddress
                            pos <- return $ Text addr
                            sym <- return $ positionSymbol pos $ symbolizeFunc f
                            declareSymbol (AnyPosition sym)

    allocate_stack_variable :: Variable -> Compiler ()
    allocate_stack_variable v@(Variable t _) = do pos <- allocateStack (sizeOf t)
                                                  sym <- return $ positionSymbol pos $ symbolizeVar v
                                                  declareSymbol (AnyPosition sym)

    allocate_locals locals = sequence_ $ map allocate_stack_variable locals
    allocate_params params = sequence_ $ map allocate_stack_variable params

    withFunction :: Function -> [Positioned Variable] -> Compiler () -> Compiler ()
    withFunction f params x = do comment ("-> function " ++ (nameOf f))
                                 declare_function f
                                 withBlock $ do allocate_params (map deposition params)
                                                x
                                 comment ("<- function " ++ (nameOf f))
