module Main where
    import System(getArgs)
    import System.Console.GetOpt
    import Data.Map hiding (map)
    import Monad

    import Compiler.Parser(program)
    import Compiler.Semantics
    import Compiler.Syntax
    import Text.ParserCombinators.Parsec(parseFromFile)

    import IPPrint
            
    -- |Command line flags
    data Flag = ShowAST    -- ^The syntax tree should be printed
              | ShowTables -- ^The symbol tables (global and for each block) should be printed
              | Check      -- ^The code should only be checked for correctness

    header = "Usage: cm {-c|-s|-a} file"

    options = [Option ['a'] ["show-syntax"] (NoArg ShowAST)
                      "show parsed abstract syntax tree",

               Option ['s'] ["show-symbol-tables"] (NoArg ShowTables)
                      "show symbol tables",

               Option ['c'] ["check"] (NoArg Check)
                      "check for errors only"]

    printTable :: Map String (Positioned Symbol) -> IO ()
    printTable table = sequence_$map (putStrLn . showSymbol) (assocs table)
        where showSymbol (name, Positioned pos (VarSymbol (Variable t _))) =
                  "  " ++ show name ++ " is " ++ showType t ++ " defined at " ++ show pos
              showSymbol (name, Positioned pos (FuncSymbol (Function t _ _ _))) =
                  "  " ++ show name ++ " is a function returning " ++ showType t ++ " defined at " ++ show pos

              showType (Pointer Int) = "an unbounded integer array"
              showType (Array Int n) = "an integer array of size " ++ show n
              showType Int           = "an integer"
              showType Void          = "void"
                       
    printFunctionTables :: Map String (Positioned Symbol) -> Positioned Function -> IO ()
    printFunctionTables globals (Positioned pos f@(Function t name args body)) =
        do putStrLn ("\nIn function " ++ name ++ " defined at " ++ show pos ++ ":")
           printTable (makeTable globals (functionSymbols f))

    execParser flag path = 
        do result <- parseFromFile program path
           case result of
             Left err   -> print err -- syntax error
             Right ast  -> do typeErrors <- return $ programTypeErrors ast
                              case typeErrors of
                                []     -> handleSyntaxTree flag ast
                                errors -> sequence_ $ map (putStrLn . show) errors


    handleSyntaxTree Check      ast = putStr "No errors\n"
    handleSyntaxTree ShowAST    ast = pprint ast
    handleSyntaxTree ShowTables ast = do putStrLn "Global scope:"
                                         printTable globals
                                         sequence_ $ map (printFunctionTables globals) (globalFuncs ast)
        where Program syms = ast
              globals = toSymbolTable syms

    main = do args <- getArgs
              case getOpt RequireOrder options args of
                ([flag],  [path],  [])   -> execParser flag path
                (_,       _,       msgs) -> error $ concat msgs ++ usageInfo header options
