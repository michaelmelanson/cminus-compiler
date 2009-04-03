
module Compiler.TypeChecking.Typeable where
    import Data.Maybe

    import Text.ParserCombinators.Parsec 
    
    import Compiler.Syntax
    import Compiler.Positioned
    import Compiler.CompileError
    import Compiler.SymbolTable

    class Typeable a where
        typeOf :: SourcePos -> SymbolTable -> a
               -> Either Type CompileError

        
    instance Typeable Symbol where
        typeOf _ _ (FuncSymbol _ (Function t _ _ _)) = Left t
        typeOf _ _ (VarSymbol  _ (Variable t _))     = Left t

    instance Typeable a => Typeable (Positioned a) where
        typeOf _ table (Positioned pos x) = typeOf pos table x

    instance Typeable Value where
        -- |A literal number is always an integer
        typeOf _ _ (IntValue _) = Left Int

        -- |A variable reference is the same type as its variable
        typeOf pos symbols (VariableRef name)
            | symbolUndefined name symbols = Right (TypeError pos ("undefined variable " ++ show name))
            | otherwise                    = typeOf pos symbols (symbolLookup name symbols)


        -- |An array reference is the same type as an element of that array
        typeOf pos symbols (ArrayRef name indexExpr)
            -- First check if either the array or the subscript has an error
            | isRight typeOfArray          = typeOfArray
            | isRight indexType            = indexType

            -- Next check for local errors
            | symbolUndefined name symbols     = Right (TypeError pos ("undefined variable" ++ show name))
            | not (symbolIsArray name symbols) = Right (TypeError pos ("variable " ++ show name ++ " is not an array"))
            | indexType /= Left Int            = Right (TypeError pos "array subscript must be an integer")
            | outOfBounds indexExpr            = Right (TypeError pos "array subscript out of range")

            -- I guess it's good
            | otherwise                    = Left varType
            where indexType   = typeOf pos symbols indexExpr
                  var         = symbolLookup name symbols
                  typeOfArray = typeOf pos symbols var

                  bounded = case typeOfArray of
                              Left (Array t n) -> True
                              Left _ -> False

                  (varType, arrayLen) = case typeOfArray of
                                          Left (Pointer t) -> (t, undefined)
                                          Left (Array t n) -> (t, n)                                      

                  outOfBounds (ValueExpr (IntValue n)) | bounded && n >= arrayLen = True
                                                       | n < 0         = True
                                                       | otherwise     = False
                  outOfBounds _ = False

                  isRight (Right _) = True
                  isRight _ = False


        -- |A function call is the same type as the return type of that function
        typeOf pos symbols (FunctionCall name _)
            | symbolUndefined name symbols =
                Right (TypeError pos ("undefined function " ++ show name))
            | otherwise = typeOf pos symbols (symbolLookup name symbols)


    instance Typeable Expression where
        -- |The type of a value expression is the type of its value
        typeOf pos symbols (ValueExpr v) = 
            case validateValue pos symbols v of
              Just errors -> Right (MultipleErrors errors)
              Nothing -> typeOf pos symbols v

        -- |An arithmetic expression is the same type as its subexpressions
        typeOf pos symbols (ArithmeticExpr _ lhs rhs) =
            let lefttype  = typeOf pos symbols lhs
                righttype = typeOf pos symbols rhs
            in case (lefttype, righttype) of
                 (Left t1, Left t2) -> coerce symbols pos t1 t2
                 (Left _,  Right e) -> Right e
                 (Right e, _)       -> Right e


        typeOf pos symbols (AssignmentExpr lhs rhs) =
            let lefttype  = typeOf pos symbols lhs
                righttype = typeOf pos symbols rhs
            in case (lefttype, righttype) of
                 (Left t1, Left t2) -> coerce symbols pos t1 t2
                 (Left _,  Right e) -> Right e
                 (Right e, _)       -> Right e


    validateArgs table (Positioned pos (FuncSymbol _ (Function _ _ args _))) params = 
        let pairs = zip args params

            validateArg (Positioned _ (Variable t _), param) =
                case typeOf pos table param of
                  Right error -> Just [error]
                  Left paramType -> case coerce table pos t paramType of
                                      Left _ -> Nothing
                                      Right error -> Just [error]

        in listToMaybe $ mapMaybe validateArg pairs


    coerce _ pos t1 t2 = case t1 == t2 of
                       True -> Left t1
                       False -> Right (TypeError pos ("mismatched types:"
                                                      ++ "\n\tleft hand side is of type " ++ show t1
                                                      ++ "\n\tright hand side is of type " ++ show t2))

    validateValue _   _       (IntValue _) = Nothing

    validateValue pos symbols (VariableRef name) = symbolValidateVar pos name symbols
    validateValue pos symbols (ArrayRef name sub) = symbolValidateArray pos name symbols
    validateValue pos symbols (FunctionCall name args) =
        case symbolValidateFunc pos name symbols of
          Just x  -> Just x   -- Error
          Nothing ->
              case validateArgs symbols sym args of
                Just errors -> Just [TypeError pos ("argument mismatch: " ++ (concat $ map show errors))]
                Nothing     -> Nothing
            where sym = symbolLookup name symbols