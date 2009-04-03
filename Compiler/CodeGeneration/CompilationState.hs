
module Compiler.CodeGeneration.CompilationState where
    import Control.Monad.State
    import Data.Set as Set
    import Data.Ix

    import Compiler.CodeGeneration.InstructionSet
    import Compiler.CodeGeneration.SymbolResolution
    import Compiler.CodeGeneration.LabelResolution
    import Compiler.CodeGeneration.LabelTable


    import Compiler.SymbolTable



    data CompilationBlock = CompilationBlock {
          symbolTable  :: SymbolTable,
          labelTable   :: LabelTable,
          instructions :: [Instruction],
          stackPointer :: Int
        }

    data CompilationState = CompilationState {
          usedRegisters :: Set Int,
          instCounter  :: Int,          -- ^Current program address
          blocks       :: [CompilationBlock]
        }

    newBlock = do top <- topBlock
                  return $ top { instructions = [] }
               
    initBlock = CompilationBlock {
                  symbolTable = initSymbolTable,
                  labelTable = initLabelTable,
                  instructions = [],
                  stackPointer = -3
              }


    type Compiler a = State CompilationState a


    -- |Compiles something within a compilation environment,
    -- |returning the resulting instructions
    withCompiler x = instructions $ head $ blocks $ execState x init
        where init = CompilationState {
                       usedRegisters = empty,
                       instCounter = 0,
                       blocks = [initBlock]
                     }

    


    emit i = do b <- topBlock
                i' <- return $ (instructions b) ++ [i]
                replaceTopBlock $ b { instructions = i' }

                unless (isComment i) incrementAddress


    -- |Return the address at which the next instruction will be
    -- |placed
    currentAddress = do st <- get :: Compiler CompilationState
                        return $ instCounter st

    incrementAddress = do st <- get
                          put $ st { instCounter = (instCounter st) + 1 }

    currentStackPointer = do b <- topBlock
                             return $ stackPointer b

    incrementStackPointer i = do b <- topBlock
                                 b' <- return $ b { stackPointer = (stackPointer b) - i }
                                 replaceTopBlock b'

    currentSymbolTable = do b <- topBlock
                            return $ symbolTable b
  

    availableRegisters st = toList $ all `difference` used
        where all = fromList $ range (0,8)
              reserved = fromList [pc, gp, fp, ac]
              used = usedRegisters st                            

    -- |This is the poor man's register allocator. Think of it like a
    -- |penny tray at a convenience store: "need a register? take a
    -- |register. have a register? leave a register."  What if there
    -- |are no free registers, you ask? Tough shit, it isn't smart
    -- |enough to spill to memory.
    claimRegister = do st <- get :: Compiler CompilationState
                       case availableRegisters st of
                         []     -> error "ow, my brain! there's no free registers!"
                         (x:xs) -> do put $ st { usedRegisters = insert x (usedRegisters st) }
                                      return x

    freeRegister r = do st <- get :: Compiler CompilationState
                        put $ st { usedRegisters = delete r (usedRegisters st) }

    declareSymbol s = declareSymbols [s]
    declareSymbols syms = do b <- topBlock
                             table' <- return $ insertSymbols (symbolTable b) syms
                             replaceTopBlock $ b { symbolTable = table' }



    -- |Pushes a new compilation block to the stack. All labels within
    -- |a block are isolated from their surrounding environment
    pushBlock = do st <- get :: Compiler CompilationState
                   b <- newBlock
                   blocks' <- return $ b:(blocks st)
                   put $ st { blocks = blocks' }
                   return ()

    popBlock = do st <- get :: Compiler CompilationState
                  (x:xs) <- return $ blocks st
                  put $ st { blocks = xs }
                  return x

    topBlock = do st <- get :: Compiler CompilationState
                  return $ head $ blocks st

    appendInstructions i = do top <- topBlock
                              top' <- return $ top { instructions = (instructions top) ++ i }
                              
                              replaceTopBlock top'

    defineLabel name address = do b <- topBlock
                                  replaceTopBlock $ b { labelTable = (insertLabel (labelTable b) name address) }

    replaceTopBlock x = do st <- get
                           xs <- return $ tail $ blocks st
                           put $ st { blocks = (x:xs) }

    finalizeBlock b = do i' <- return $ resolveSymbols (symbolTable b) (instructions b)
                         i'' <- return $ resolveLabels  i' (labelTable b)
                         appendInstructions i''
                        
    withBlock x = do pushBlock
                     x
                     b <- popBlock
                     finalizeBlock b

    withBlockRet x = do pushBlock
                        ret <- x
                        b <- popBlock
                        finalizeBlock b
                        return ret

    comment x  = emit $ COMMENT x
    label name = do a <- currentAddress
                    comment ("label " ++ name ++ " defined at " ++ show a)
                    defineLabel name a
