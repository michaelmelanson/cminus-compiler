
module Compiler.CompileError where
    import Text.ParserCombinators.Parsec

    import Compiler.Syntax
    import Compiler.TypeChecking.Nameable

    data CompileError = TypeError SourcePos String
                      | DupeError (Positioned Symbol) (Positioned Symbol)
                      | MultipleErrors [CompileError]
                        deriving (Eq)

    instance Show CompileError where
        show (TypeError pos message) = "Type error at " ++ show pos ++ ": " ++ message
        show (DupeError (Positioned oldPos old)
                        (Positioned newPos new)) =
                 "Duplicate symbol: " ++
                 "\n\t" ++ show (nameOf old) ++ " defined at " ++ (show oldPos) ++
                 "\n\t" ++ show (nameOf new) ++ " defined at " ++ (show newPos)

        show (MultipleErrors errors) = concat $ map show errors