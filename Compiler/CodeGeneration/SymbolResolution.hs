
module Compiler.CodeGeneration.SymbolResolution(resolveSymbols) where
    import Debug.Trace

    import Compiler.Syntax
    import Compiler.SymbolTable
    import Compiler.CodeGeneration.InstructionSet

    resolveSymbols symbols instrs = map (resolveSymbol symbols) instrs

    resolveSymbol symbols (LD  r (SymbolRef addr name)) = (LD  r (symbolAddress symbols name addr))
    resolveSymbol symbols (LDA r (SymbolRef addr name)) = (LDA r (symbolAddress symbols name addr))
    resolveSymbol symbols (ST  r (SymbolRef addr name)) = (ST  r (symbolAddress symbols name addr))
    resolveSymbol _ i = i



    symbolAddress symbols name add | symbolDefined name symbols = address
                                   | otherwise = error ("symbol " ++ show name ++ " is undefined")
        where address = case loc of
                          Unknown   -> error ("symbol " ++ show name ++ " has not been placed anywhere!")
                          Stack off -> Memory off fp
                          Data  off -> Memory off gp
                          Text  off -> Memory (off - add - 1) pc

              loc = case symbolLookup name symbols of
                      Positioned _ (FuncSymbol a _) -> a
                      Positioned _ (VarSymbol  a _) -> a
                      AnyPosition  (FuncSymbol a _) -> a
                      AnyPosition  (VarSymbol  a _) -> a

              