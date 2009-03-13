{-# OPTIONS -fglasgow-exts #-}

module Compiler.Semantics where
    import Data.Map (Map, union, empty, insert, member)
    import qualified Data.Map as M
    import Text.ParserCombinators.Parsec
    import Compiler.Syntax

    dupeToError (old, new) = DupeError old new

    programTypeErrors p@(Program syms) = dupeErrors ++ (concat $ map (funcTypeErrors table) funcs)
        where dupeErrors = map dupeToError (findDuplicates syms)
              table = toSymbolTable syms
              funcs = globalFuncs p

    globalFuncs :: Program -> [Positioned Function]
    globalFuncs (Program syms) = map symbolToFunc $ filter isFunction syms
        where isFunction (Positioned _ (FuncSymbol _)) = True
              isFunction _ = False

              symbolToFunc (Positioned pos (FuncSymbol f)) = Positioned pos f

    globalTable (Program p) = toSymbolTable p
    -- localTable program function = 

    functionSymbols :: Function -> [Positioned Symbol]
    functionSymbols (Function _ _ args (Positioned _ (CompoundStatement locals _))) =
        map symbolize (args ++ locals)

    toSymbolTable :: [Positioned Symbol] -> Map String (Positioned Symbol)
    toSymbolTable syms = foldl insertSymbol empty syms
        where insertSymbol table s = insert (nameOf s) s table
    
    symbolize (Positioned pos arg) = Positioned pos (VarSymbol arg)

    funcTypeErrors globals (Positioned _ (Function _ _ args body@(Positioned _ (CompoundStatement locals _)))) = dupeErrors ++ stmtTypeErrors table body
        where dupeErrors = map dupeToError (findDuplicates (argSyms ++ map symbolize locals))
              table = toSymbolTable argSyms `union` globals
              argSyms = map symbolize args


    findDuplicates :: [Positioned Symbol] -> [(Positioned Symbol, Positioned Symbol)]
    findDuplicates syms = dupes
        where collectSyms (table, dupes) sym | member name table = (table, dupes ++ [(oldsym, sym)])
                                             | otherwise = (M.insert name sym table, dupes)
                  where name = nameOf sym
                        Just oldsym = M.lookup name table

              (_, dupes) = foldl collectSyms (M.empty, []) syms

    makeTable globalTable localSyms = toSymbolTable localSyms `union` globalTable

             

    stmtTypeErrors globals (Positioned pos (ExpressionStatement e))  =
        case typeOf pos globals e of
          Left _ -> []
          Right error -> [error]

    stmtTypeErrors symbols (Positioned _ (CompoundStatement locals stmts)) =
        foldl (++) [] $ map (stmtTypeErrors localTable) stmts

        where localSyms = map symbolize locals
              localTable = makeTable symbols localSyms

    stmtTypeErrors globals (Positioned pos (SelectionStatement e thenClause elseClause))
        | typeOf pos globals e /= Left Int = [TypeError pos "the condition of a selection statement must evaluate to an integer"]
        | otherwise = stmtTypeErrors globals thenClause ++
                      stmtTypeErrors globals elseClause

    -- |Return statements can't make type errors
    stmtTypeErrors globals (Positioned pos ReturnStatement) = []

    stmtTypeErrors globals other = []