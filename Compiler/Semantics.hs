{-# OPTIONS -fglasgow-exts #-}

module Compiler.Semantics where
    import Data.Map (Map, union, empty, insert, lookup)
    import Text.ParserCombinators.Parsec
    import Compiler.Syntax

    programTypeErrors :: Program -> [CompileError]
    programTypeErrors p@(Program syms) = concat $ map (funcTypeErrors table) funcs
        where table = toSymbolTable syms
              funcs = globalFuncs p

    globalFuncs :: Program -> [Positioned Function]
    globalFuncs (Program syms) = map symbolToFunc $ filter isFunction syms
        where isFunction (Positioned _ (FuncSymbol _)) = True
              isFunction _ = False

              symbolToFunc (Positioned pos (FuncSymbol f)) = Positioned pos f

    toSymbolTable :: [Positioned Symbol] -> Map String (Positioned Symbol)
    toSymbolTable p = foldl insertSymbol empty p
        where insertSymbol table s = insert (nameOf s) s table
    
    symbolize (Positioned pos arg) = Positioned pos (VarSymbol arg)

    funcTypeErrors globals (Positioned _ (Function _ _ args block)) = stmtTypeErrors table block
        where table = toSymbolTable argSyms `union` globals
              argSyms = map symbolize args
             

    stmtTypeErrors globals (Positioned pos (ExpressionStatement e)) =
        case typeOf pos globals e of
          Left Int -> []
          Left _ -> [TypeError pos "expressions must result in integers"]
          Right error -> [error]


    stmtTypeErrors globals (Positioned _ (CompoundStatement locals stmts)) =
        foldl (++) [] $ map (stmtTypeErrors localTable) stmts

        where localSyms = map symbolize locals
              localTable = toSymbolTable localSyms `union` globals

    stmtTypeErrors globals (Positioned pos (SelectionStatement e thenClause elseClause))
        | typeOf pos globals e /= Left Int = [TypeError pos "the condition of a selection statement must evaluate to an integer"]
        | otherwise = stmtTypeErrors globals thenClause ++
                      stmtTypeErrors globals elseClause

    -- |Return statements can't make type errors
    stmtTypeErrors globals (Positioned pos ReturnStatement) = []

    stmtTypeErrors globals other = []