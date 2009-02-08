module Main where
    import System(getArgs)
    import Monad

    import Compiler.Parser
    import Text.ParserCombinators.Parsec

    import IPPrint

    main = do path <- liftM head getArgs
              result <- parseFromFile program path
              case result of
                Left err -> print err
                Right a  -> pprint a

              