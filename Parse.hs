module Main where
    import System(getArgs)
    import System.Console.GetOpt

    import Compiler.Parser
    import Text.ParserCombinators.Parsec

    import IPPrint
            
    data Flag = Show | Check

    header = "Usage: cm {-c|-a} file"

    options = [Option ['a'] ["show-syntax"] (NoArg Show)
                      "show parsed abstract syntax tree",

               Option ['c'] ["check"] (NoArg Check)
                      "check for errors only"]

    execParser path show = do result <- parseFromFile program path
                              case result of
                                Left err -> print err
                                Right a  -> if show then
                                                pprint a
                                            else
                                                putStr "No errors\n"

    main = do args <- getArgs
              case getOpt RequireOrder options args of
                ([Show],  [path], [])    -> execParser path True
                ([Check], [path], [])    -> execParser path False
                (_,       _,       msgs) -> error $ concat msgs ++ usageInfo header options
