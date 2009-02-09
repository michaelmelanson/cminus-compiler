
-- |The data structures used to represent the abstract syntax tree of a C-Minus file
module Compiler.Syntax where

    data Type = Int
              | Array Type Integer
              | Pointer Type
              | Void
                deriving (Show, Eq)

    data Variable   = Variable Type String
                      deriving (Show, Eq)

    data Value      = IntValue Integer    -- ^A literal integer value
                    | VariableRef String  -- ^A reference to an integer variable
                    | ArrayRef String Expression -- ^A reference to an index of an array
                    | FunctionCall String [Expression] -- ^A function call
                      deriving (Show, Eq)

    -- |A C-Minus operator. Each of these should be fairly self-explanatory
    data Operator   = Add
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
                   | CompoundStatement [Variable] [Statement]

                   -- |An if statement, with a then-clause and an
                   --  optional else clause. The else clause, if
                   --  non-existent, is represented by a NullStatement.
                   | SelectionStatement Expression Statement Statement

                   -- |A while statement, with a condition and a statement.
                   | IterationStatement Expression Statement

                   -- |A return statement with no return value.
                   | ReturnStatement

                   -- |A return statement with a return value.
                   | ValueReturnStatement Expression

                   -- |An empty statement. These are only generated
                   --  when an IterationStatement lacks an else-clause
                   | NullStatement -- A nothing statement (e.g. missing "else")
                     deriving (Show, Eq)

    data Toplevel = GlobalVariable Variable -- ^A global variable
                  | Function Type String [Variable] Statement -- ^A function declaration
                    deriving (Show, Eq)
