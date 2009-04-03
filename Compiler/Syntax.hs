
-- |The data structures used to represent the abstract syntax tree of a C-Minus file
module Compiler.Syntax where
    import Data.Maybe
    import Text.ParserCombinators.Parsec

    -- |Where is a symbol? All these are absolute addresses
    data Location = Unknown       -- ^We don't know yet
                  | Stack   Int   -- ^It's on the stack (absolute position)
                  | Data    Int   -- ^It's in the data region (i.e, a global variable)
                  | Text    Int   -- ^It's in the text region (i.e, a function)
                    deriving (Show, Eq)

    -- |Source position information
    data Positioned a = Positioned SourcePos a 
                      | AnyPosition a

    deposition (Positioned _ x) = x
    deposition (AnyPosition x) = x

    instance Show a => Show (Positioned a) where
        show (Positioned _ a) = show a
        show (AnyPosition a)  = show a

    instance Eq a => Eq (Positioned a) where
        (Positioned _ x) == (Positioned _ y) = x == y
        (AnyPosition  x) == (Positioned _ y) = x == y
        (Positioned _ x) == (AnyPosition y)  = x == y
        (AnyPosition  x) == (AnyPosition y)  = x == y


    data Type = Int
              | Array Type Int
              | Pointer Type
              | Void
                deriving (Show, Eq)

    data Value = IntValue     Int                  -- ^A literal integer value
               | VariableRef  String               -- ^A reference to an integer variable
               | ArrayRef     String Expression    -- ^A reference to an index of an array
               | FunctionCall String [Expression]  -- ^A function call
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

    data Symbol = VarSymbol  Location Variable -- ^A global variable
                | FuncSymbol Location Function -- ^A global function
                  deriving (Show, Eq)

    sizeOf :: Type -> Int
    sizeOf Int = 1
    sizeOf (Array t n) = (sizeOf t) * n
    sizeOf (Pointer _) = 1
    sizeOf Void = error "why do you need the size of void?"