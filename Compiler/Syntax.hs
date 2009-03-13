
-- |The data structures used to represent the abstract syntax tree of a C-Minus file
module Compiler.Syntax where
    import qualified Data.Map as M
    import Data.Maybe
    import Text.ParserCombinators.Parsec

    import Debug.Trace


    data CompileError = TypeError SourcePos String
                      | DupeError (Positioned Symbol) (Positioned Symbol)
                        deriving (Eq)

    instance Show CompileError where
        show (TypeError pos message) = "Type error at " ++ show pos ++ ": " ++ message
        show (DupeError (Positioned oldPos old)
                        (Positioned newPos new)) =
                 "Duplicate symbol: " ++
                 "\n\t" ++ show (nameOf old) ++ " defined at " ++ (show oldPos) ++
                 "\n\t" ++ show (nameOf new) ++ " defined at " ++ (show newPos)

    data Type = Int
              | Array Type Integer
              | Pointer Type
              | Void
                deriving (Show, Eq)

    data Value = IntValue Integer                 -- ^A literal integer value
               | VariableRef String               -- ^A reference to an integer variable
               | ArrayRef String Expression       -- ^A reference to an index of an array
               | FunctionCall String [Expression] -- ^A function call
                 deriving (Show, Eq)

    -- |A C-Minus operator. Each of these should be fairly self-explanatory
    data Operator = Add
                  | Subtract
                  | Multiply
                  | Divide
                  | LessOrEqual
                  | Less
                  | GreaterOrEqual
                  | Greater
                  | Equal
                  | NotEqual
                    deriving (Show, Eq)

    data Program = Program [Positioned Symbol]
                   deriving (Show)

    data Expression =
            -- |Assignment of an expression to either a VariableRef or
            --  an ArrayRef (the other constructors for Value are
            --  never generated).
              AssignmentExpr Value Expression

            -- |An arithmetic expression
            | ArithmeticExpr Operator Expression Expression

            -- |A simple expression that evaluates to a value
            | ValueExpr Value
              deriving (Show, Eq)


    data Statement =
                   -- |A statement that's also an expression (probably
                   --  an assignment, though not necessarily)
                     ExpressionStatement Expression

                   -- |A code block. These define their own scope, so
                   --  they can have local variable declarations, as
                   --  well as a list of statements.
                   | CompoundStatement [Positioned Variable] [Positioned Statement]

                   -- |An if statement, with a then-clause and an
                   --  optional else clause. The else clause, if
                   --  non-existent, is represented by a NullStatement.
                   | SelectionStatement Expression
                                        (Positioned Statement)
                                        (Positioned Statement)

                   -- |A while statement, with a condition and a statement.
                   | IterationStatement Expression
                                        (Positioned Statement)

                   -- |A return statement with no return value.
                   | ReturnStatement 

                   -- |A return statement with a return value.
                   | ValueReturnStatement Expression

                   -- |An empty statement. These are only generated
                   --  when an IterationStatement lacks an else-clause
                   | NullStatement -- A nothing statement (e.g. missing "else")
                     deriving (Show, Eq)

    data Variable   = Variable Type String
                      deriving (Show, Eq)

    data Function = Function Type String [Positioned Variable] (Positioned Statement) -- ^A function declaration
                    deriving (Show, Eq)

    data Symbol = VarSymbol Variable -- ^A global variable
                | FuncSymbol Function -- ^A global function
                  deriving (Show, Eq)

    data Positioned a = Positioned SourcePos a 
                      | AnyPosition a

    instance Show a => Show (Positioned a) where
        show (Positioned _ a) = show a

    instance Eq a => Eq (Positioned a) where
        (Positioned _ x) == (Positioned _ y) = x == y
        (AnyPosition  x) == (Positioned _ y) = x == y
        (Positioned _ x) == (AnyPosition y)  = x == y
        (AnyPosition  x) == (AnyPosition y)  = x == y

    returnWithPosition x = do pos <- getPosition
                              return $ Positioned pos x

    deposition (Positioned _ x) = x

    class Nameable a where
        nameOf :: a -> String

    class Typeable a where
        typeOf :: SourcePos -> M.Map String (Positioned Symbol) -> a -> Either Type CompileError


    instance Nameable a => Nameable (Positioned a) where
        nameOf (Positioned _ x) = nameOf x

    instance Nameable Variable where
        nameOf (Variable _ name) = name

    instance Nameable Function where
        nameOf (Function _ name _ _) = name

    instance Nameable Symbol where
        nameOf (VarSymbol  v) = nameOf v
        nameOf (FuncSymbol f) = nameOf f
        
    instance Typeable Symbol where
        typeOf _ _ (FuncSymbol (Function t _ _ _)) = Left t
        typeOf _ _ (VarSymbol  (Variable t _))     = Left t

    instance Typeable a => Typeable (Positioned a) where
        typeOf _ table (Positioned pos x) = typeOf pos table x

    instance Typeable Value where
        -- |A literal number is always an integer
        typeOf _ _ (IntValue _) = Left Int

        -- |A variable reference is the same type as its variable
        typeOf pos symbols (VariableRef name)
            | M.notMember name symbols = Right (TypeError pos ("undefined variable " ++ show name))
            | otherwise                = typeOf pos symbols (fromJust $ M.lookup name symbols)


        -- |An array reference is the same type as an element of that array
        typeOf pos symbols (ArrayRef name indexExpr)
            -- First check if either the array or the subscript has an error
            | isRight typeOfArray      = typeOfArray
            | isRight indexType        = indexType

            -- Next check for local errors
            | M.notMember name symbols = Right (TypeError pos ("undefined variable" ++ show name))
            | indexType /= Left Int    = Right (TypeError pos "array subscript must be an integer")
            | outOfBounds indexExpr    = Right (TypeError pos "array subscript out of range")

            -- I guess it's good
            | otherwise                = Left varType
            where indexType   = typeOf pos symbols indexExpr
                  var         = fromJust $ M.lookup name symbols
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
            | M.notMember name symbols = Right (TypeError pos ("undefined function " ++ show name))
            | otherwise = typeOf pos symbols (fromJust $ M.lookup name symbols)

    validateArgs table (Positioned pos (FuncSymbol (Function _ _ args _))) params = 
        let pairs = zip args params

            validateArg (Positioned _ (Variable t _), param) =
                case typeOf pos table param of
                  Right error -> Just [error]
                  Left paramType -> case coerce table pos t paramType of
                                      Left _ -> Nothing
                                      Right error -> Just [error]

        in listToMaybe $ mapMaybe validateArg pairs

    isFunction :: String -> M.Map String (Positioned Symbol) -> Bool
    isFunction name table = 
        case M.lookup name table of
          Just (Positioned _ (FuncSymbol _)) -> True
          _ -> False

    isVariable name table = 
        case M.lookup name table of
          Just (Positioned _ (VarSymbol _)) -> True
          _ -> False


    coerce _ pos t1 t2 = case t1 == t2 of
                       True -> Left t1
                       False -> Right (TypeError pos ("mismatched types:"
                                                      ++ "\n\tleft hand side is of type " ++ show t1
                                                      ++ "\n\tright hand side is of type " ++ show t2))

    validateValue _   _       (IntValue _) = Nothing

    validateValue pos symbols (VariableRef name) | M.notMember name symbols = Just [TypeError pos (show name ++ " is not defined")]
                                                 | not (isVariable name symbols) = Just [TypeError pos (show name ++ " is not a variable")]
                                                 | otherwise = Nothing

    validateValue pos symbols (ArrayRef name sub) | M.notMember name symbols = Just [TypeError pos (show name ++ " is not defined")]
                                                  | not (isVariable name symbols) = Just [TypeError pos (show name ++ " is not an array")]
                                                  | otherwise = Nothing

    validateValue pos symbols (FunctionCall name args) | M.notMember name symbols      = Just [TypeError pos ("undefined function "          ++ show name)]
                                                       | not (isFunction name symbols) = Just [TypeError pos ("attempt to call non-function" ++ show name)]
                                                       | otherwise =
                       case validateArgs symbols sym args of
                         Just errors -> Just [TypeError pos ("argument mismatch: " ++ (concat $ map show errors))]
                         Nothing     -> Nothing
        where Just sym@(Positioned _ (FuncSymbol (Function returnType _ _ _))) = M.lookup name symbols
              

    instance Typeable Expression where
        -- |The type of a value expression is the type of its value
        typeOf pos symbols (ValueExpr v) = 
            case validateValue pos symbols v of
              Just errors -> Right (TypeError pos ("invalid value: " ++ (concat$ map show errors)))
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
