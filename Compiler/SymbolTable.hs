
module Compiler.SymbolTable where
    import Data.Maybe
    import qualified Data.Map as M

    import Compiler.Syntax
    import Compiler.TypeChecking.Nameable

    import Compiler.Positioned
    import Compiler.CompileError

    import Text.ParserCombinators.Parsec 
    import Text.ParserCombinators.Parsec.Pos


    import Debug.Trace

    type SymbolTable = M.Map String (Positioned Symbol)

    initSymbolTable = toSymbolTable [Positioned (initialPos "prelude") $ FuncSymbol Unknown $ Function Int "input" [] (Positioned (initialPos "prelude") $ CompoundStatement [] []),
                                     Positioned (initialPos "prelude") $ FuncSymbol Unknown $ Function Void "output" [Positioned (initialPos "prelude") $ Variable Int "value"] (AnyPosition $ CompoundStatement [] [])]

    allFunctions table = M.elems $ M.filter isFunc table
        where isFunc (FuncSymbol _ _) = True
              isFunc _ = False
              
    insertSymbol  table s = M.insert (nameOf s) s table
    insertSymbols table syms = foldl insertSymbol table syms

    toSymbolTable :: [Positioned Symbol] -> SymbolTable
    toSymbolTable syms = insertSymbols M.empty syms


    mergeSymbolTables a b = M.union a b
    
    symbolizePVar (Positioned pos arg) = Positioned pos (symbolizeVar arg)
    symbolizePVar (AnyPosition arg)    = AnyPosition    (symbolizeVar arg)

    symbolizePFunc (Positioned pos arg) = Positioned pos (symbolizeFunc arg)
    symbolizePFunc (AnyPosition arg)    = AnyPosition    (symbolizeFunc arg)

    symbolizeVar  v = (VarSymbol  Unknown v)
    symbolizeFunc f = (FuncSymbol Unknown f)

    globalTable (Program p) = toSymbolTable p
    -- localTable program function = 

    functionSymbols :: Function -> [Positioned Symbol]
    functionSymbols (Function _ _ args
                     (Positioned _ (CompoundStatement locals _))) =
        map symbolizePVar (args ++ locals)
             

    symbolUndefined name table = isNothing $ M.lookup name table
    symbolDefined   name table = isJust    $ M.lookup name table
    symbolLookup    name table = fromJust  $ M.lookup name table

    symbolIsFunction name table | symbolUndefined name table = trace ("symbolIsFunction: " ++ show name ++ " is not defined")  False
                                | otherwise =
                            case symbolLookup name table of
                              Positioned _ (FuncSymbol _ _) -> True
                              AnyPosition (FuncSymbol _ _) -> True
                              other -> trace ("symbolIsFunction: " ++ show name ++ " is not a function, it's " ++ show other) False


    symbolIsVariable :: String -> SymbolTable -> Bool
    symbolIsVariable name table | symbolUndefined name table = False
                                | otherwise =
                            case fromJust $ M.lookup name table of
                              Positioned _ (VarSymbol _ _) -> True
                              _ -> False

    symbolIsArray :: String -> SymbolTable -> Bool
    symbolIsArray name table | symbolUndefined name table = False
                             | otherwise =
                            case symbolLookup name table of
                              Positioned _ (VarSymbol _ (Variable (Array _ _) _ )) -> True
                              _ -> False


    symbolValidateArray pos name symbols | symbolUndefined name symbols = Just [TypeError pos (show name ++ " is not defined")]
                                         | symbolIsArray name symbols = Nothing -- We're good
                                         | otherwise = Just [TypeError pos (show name ++ " is not an array")]

    symbolValidateVar pos name symbols | symbolUndefined name symbols = Just [TypeError pos (show name ++ " is not defined")]
                                       | symbolIsVariable name symbols = Nothing -- We're good
                                       | otherwise = Just [TypeError pos (show name ++ " is not a variable")]

    symbolValidateFunc pos name symbols | symbolUndefined name symbols  = Just [TypeError pos ("undefined function " ++ show name)]
                                        | symbolIsFunction name symbols = Nothing
                                        | otherwise = Just [TypeError pos ("attempt to call non-function" ++ show name)]
       

    functionReturnsVoid name symbols | symbolUndefined name symbols        = error (name ++ " is not defined")
                                     | not (symbolIsFunction name symbols) = error (name ++ " is not a function")
                                     | otherwise =
                                         case symbolLookup name symbols of
                                           Positioned _ (FuncSymbol _ (Function t _ _ _)) -> t == Void
                                           AnyPosition (FuncSymbol _ (Function t _ _ _)) -> t == Void