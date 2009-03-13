
-- |The data structures used to represent the abstract syntax tree of a C-Minus file
module Compiler.Syntax where
    import qualified Data.Map as M
    import Data.Maybe
    import Text.ParserCombinators.Parsec


    data CompileError = TypeError SourcePos String
                        deriving (Eq)

    instance Show CompileError where
        show (TypeError pos message) = "Type error at " ++ show pos ++ ": " ++ message


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

            -- Must be good
            | otherwise                = Left varType
            where indexType                  = typeOf pos symbols indexExpr
                  var                        = fromJust $ M.lookup name symbols
                  typeOfArray = typeOf pos symbols var
                  Left (Array varType arrayLen) = typeOfArray

                  outOfBounds (ValueExpr (IntValue n)) | n >= arrayLen = True
                                                       | n < 0         = True
                                                       | otherwise     = False
                  outOfBounds _ = False

                  isRight (Right _) = True
                  isRight _ = False


        -- |A function call is the same type as the return type of that function
        typeOf pos symbols (FunctionCall name _)
            | M.notMember name symbols = Right (TypeError pos ("undefined function " ++ show name))
            | otherwise = typeOf pos symbols (fromJust $ M.lookup name symbols)


    instance Typeable Expression where
        -- |The type of a value expression is the type of its value
        typeOf pos symbols (ValueExpr v) = typeOf pos symbols v

        -- |An arithmetic expression is the same type as its subexpressions
        typeOf pos symbols (ArithmeticExpr _ lhs rhs) =
            let lefttype  = typeOf pos symbols lhs
                righttype = typeOf pos symbols rhs
            in case (lefttype, righttype) of
                 (Left t1, Left t2) | t1 == t2 -> Left t1
                                    | otherwise -> Right (TypeError pos ("mismatched types in arithmetic expression:"
                                                                         ++ "\n\tleft hand side " ++ show lhs ++ " is of type " ++ show t1
                                                                         ++ "\n\tright hand side " ++ show rhs  ++ "is of type " ++ show t2))
                 (Left _,  Right e) -> Right e
                 (Right e, _)       -> Right e


        typeOf pos symbols (AssignmentExpr lhs rhs) =
            let lefttype  = typeOf pos symbols lhs
                righttype = typeOf pos symbols rhs
            in case (lefttype, righttype) of
                 (Left t1, Left t2) | t1 == t2 -> Left t1
                                    | otherwise -> Right (TypeError pos ("mismatched types in assignment:"
                                                                         ++ "\n\tleft hand side " ++ show lhs ++ " is of type " ++ show t1
                                                                         ++ "\n\tright hand side " ++ show rhs  ++ "is of type " ++ show t2))
                 (Left _,  Right e) -> Right e
                 (Right e, _)       -> Right e
