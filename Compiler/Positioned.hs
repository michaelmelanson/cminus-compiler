
module Compiler.Positioned where
    import Text.ParserCombinators.Parsec

    import Compiler.Syntax


    returnWithPosition x = do pos <- getPosition
                              return $ Positioned pos x

    deposition (Positioned _ x) = x
