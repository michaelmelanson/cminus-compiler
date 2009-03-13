module Main where
    import System(getArgs)
    import System.Console.GetOpt

    import Compiler.Parser(program)
    import Compiler.Semantics(programTypeErrors)
    import Compiler.Syntax(Program)
    import Text.ParserCombinators.Parsec(parseFromFile)

    import IPPrint
            
    -- |Command line flags
    data Flag = Show  -- ^The syntax tree should be printed
              | Check -- ^The code should only be checked for correctness

    header = "Usage: cm {-c|-a} file"

    options = [Option ['a'] ["show-syntax"] (NoArg Show)
                      "show parsed abstract syntax tree",

               Option ['c'] ["check"] (NoArg Check)
                      "check for errors only"]

    showSyntaxTreeIf True prog  = pprint prog
    showSyntaxTreeIf False _ = putStr "No errors\n"

    handleTypeErrors :: [String] -> Bool -> Program -> IO ()
    handleTypeErrors [] showSyntax prog = showSyntaxTreeIf showSyntax prog
    handleTypeErrors errors _ _ = mapM putStrLn errors >> return ()

    execParser path showSyntax = 
        do result <- parseFromFile program path
           case result of
             Left err -> print err -- syntax error
             Right a  -> 
                 do typeErrors <- return $ map show $ programTypeErrors a
                    handleTypeErrors typeErrors showSyntax a
                                                
    main = do args <- getArgs
              case getOpt RequireOrder options args of
                ([Show],  [path], [])    -> execParser path True 
                ([Check], [path], [])    -> execParser path False
                (_,       _,       msgs) -> error $ concat msgs ++ usageInfo header options
