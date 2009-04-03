{-# OPTIONS -fglasgow-exts #-}

module Compiler.Semantics where
    import Text.ParserCombinators.Parsec
    import Data.Either

    import Compiler.Syntax
    import Compiler.CompileError
    import Compiler.SymbolTable
    import Compiler.TypeChecking.Nameable
    import Compiler.TypeChecking.Typeable

    dupeToError (old, new) = DupeError old new

    programTypeErrors p@(Program syms) =
        dupeErrors ++ (concat $ map (funcTypeErrors table) funcs)
        where dupeErrors = map dupeToError (findDuplicates syms)
              table = toSymbolTable syms `mergeSymbolTables` initSymbolTable
              funcs = globalFuncs p

    globalFuncs :: Program -> [Positioned Function]
    globalFuncs (Program syms) = map symbolToFunc $ filter isFunction syms
        where isFunction (Positioned _ (FuncSymbol _ _)) = True
              isFunction _ = False

              symbolToFunc (Positioned pos (FuncSymbol _ f)) = Positioned pos f

    funcTypeErrors globals (Positioned _
                            (Function _ _ args
                                      body@(Positioned _
                                            (CompoundStatement locals _)))) =
                          dupeErrors ++ stmtTypeErrors table body

        where syms = argSyms ++ map symbolizePVar locals
              dupeErrors = map dupeToError (findDuplicates syms)
              
              table = toSymbolTable argSyms `mergeSymbolTables` globals
              argSyms = map symbolizePVar args


    collectSyms (table, dupes) sym | symbolDefined name table = 
                          let dupes' = dupes ++ [(oldsym, sym)]
                          in (table, dupes')

                                   | otherwise = (insertSymbol table sym, dupes)
                  where name = nameOf sym
                        oldsym = symbolLookup name table


    findDuplicates :: [Positioned Symbol] -> [(Positioned Symbol,
                                               Positioned Symbol)]
    findDuplicates syms = dupes
        where (_, dupes) = foldl collectSyms (initSymbolTable, []) syms

    makeTable globalTable localSyms =
        toSymbolTable localSyms `mergeSymbolTables` globalTable

             

    stmtTypeErrors globals (Positioned pos (ExpressionStatement e))  =
        case typeOf pos globals e of
          Left _ -> []
          Right error -> [error]

    stmtTypeErrors symbols (Positioned _ (CompoundStatement locals stmts)) =
        foldl (++) [] $ map (stmtTypeErrors localTable) stmts

        where localSyms = map symbolizePVar locals
              localTable = makeTable symbols localSyms

    stmtTypeErrors globals (Positioned pos (SelectionStatement e
                                                               thenClause
                                                               elseClause))
        | isRight t = case t of
                        Right e -> [e]
        | t /= Left Int =
            let msg = "the condition of a selection statement must " ++
                      "evaluate to an integer. This one evaluates to " ++
                      show t
            in [TypeError pos msg]
        | otherwise = stmtTypeErrors globals thenClause ++
                      stmtTypeErrors globals elseClause
        where t = typeOf pos globals e
              isRight (Right _) = True
              isRight _ = False

    -- |Return statements can't make type errors
    stmtTypeErrors globals (Positioned pos ReturnStatement) = []

    stmtTypeErrors globals other = []