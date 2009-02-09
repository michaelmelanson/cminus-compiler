-- |The parser for the C-Minus compiler. This converts the token
--  stream produced by Compiler.Scanner into an abstract syntax tree.

module Compiler.Parser where
    import Compiler.Scanner

    import Text.ParserCombinators.Parsec 
    import Text.ParserCombinators.Parsec.Expr hiding (Operator)

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

    -- |The top parser. This parses an entire C-Minus source file
    program = do { whiteSpace
                 ; p <- many1 toplevel_decl
                 ; return p
                 }

    -- |Parser for one top-level declaration (either a global variable or a function)
    toplevel_decl = do { t <- typeSpec
                       ; id <- identifier
                       ; x <- toplevelTail t id
                       ; return x
                       }
                   <?> "a function declaration or global variable"
        where toplevelTail t id = do { args <- parens params <?> "function arguments"
                                     ; body <- braces compound_stmt <?> "function body"
                                     ; return $ Function t id args body
                                     }  
                              <|> do { size <- squares (integer <?> "array size") <?> "array bounds"
                                     ; semi
                                     ; return $ GlobalVariable$Variable (Array t size) id
                                     }
                              <|> (semi >> (return $ GlobalVariable $ Variable t id))

    var_declaration = do { p <- param
                         ; semi
                         ; return p
                         }
           

    param = do { t <- typeSpec
               ; id <- identifier
               ; x <- maybeArrayLen t id
               ; return x
               }
        where maybeArrayLen t id = do { squares whiteSpace
                                      ; return $ Variable (Pointer t) id
                                      }
                               <|> do { size <- squares integer
                                      ; return $ Variable (Array t size) id
                                      }
                               <|> (return $ Variable t id)

    params = (reserved "void" >> return []) <|> commaSep1 param

    statement = (var_declaration >> unexpected "variable declaration (variables must be declared before any statements)")
            <|> return_stmt
            <|> braces compound_stmt
            <|> selection_stmt
            <|> iteration_stmt
            <|> expression_stmt
            <?> "statement"

    expression_stmt = do { expr <- expression
                         ; semi
                         ; return $ ExpressionStatement expr
                         }

    compound_stmt = do { vars <- many var_declaration
                       ; stmts <- many statement
                       ; return $ CompoundStatement vars stmts
                       }

    selection_stmt = do { reserved "if"
                        ; condExpr <- parens expression
                        ; thenClause <- statement
                        ; elseClause <- else_clause
                        ; return $ SelectionStatement condExpr
                                                      thenClause
                                                      elseClause
                        }
        where else_clause = do { reserved "else"
                               ; stmt <- statement
                               ; return stmt
                               }
                        <|> return NullStatement

    iteration_stmt = do { reserved "while"
                        ; condExpr <- parens expression
                        ; body <- statement
                        ; return $ IterationStatement condExpr body
                        }

    return_stmt = do { reserved "return"
                     ; x <- return_value
                     ; return x
                     }
        where return_value = (semi >> return ReturnStatement)
                         <|> do { expr <- simple_expression
                                ; semi
                                ; return $ ValueReturnStatement expr
                                } 
              

    expression = try (do { ident <- lvalue
                         ; reservedOp "="
                         ; value <- simple_expression
                         ; return $ AssignmentExpr ident value                   
                         })
             <|> simple_expression

        where lvalue = do { ident <- identifier
                          ; x <- maybeindex ident
                          ; return x
                          }
              
              maybeindex ident = do { index <- squares expression
                                    ; return $ ArrayRef ident index
                                    } <|> do { return $ VariableRef ident }
                             


    simple_expression = buildExpressionParser table factor
        where table = [[op ">=" (ArithmeticExpr GreaterOrEqual) AssocLeft,
                        op ">"  (ArithmeticExpr Greater)        AssocLeft,
                        op "<=" (ArithmeticExpr LessOrEqual)    AssocLeft,
                        op "<"  (ArithmeticExpr Less)           AssocLeft,
                        op "==" (ArithmeticExpr Equal)          AssocLeft,
                        op "!=" (ArithmeticExpr NotEqual)       AssocLeft],

                       [op "*"  (ArithmeticExpr Multiply)       AssocLeft,
                        op "/"  (ArithmeticExpr Divide)         AssocLeft],

                       [op "+"  (ArithmeticExpr Add)            AssocLeft,
                        op "-"  (ArithmeticExpr Subtract)       AssocLeft]]
              op s f assoc = Infix (do{ reservedOp s; return f}) assoc

    factor  = (parens simple_expression)
              <|> value_expression

    value_expression = do { val <- value
                          ; return $ ValueExpr val
                          }

    value = do { num <- integer; return $ IntValue num }
        <|> do { ident <- identifier
               ; x <- variableOrCall ident
               ; return x
               }
        where variableOrCall ident = do { index <- squares simple_expression
                                        ; return $ ArrayRef ident index
                                        }
                                 <|> do { args <- parens (commaSep simple_expression)
                                        ; return $ FunctionCall ident args
                                        }
                                 <|> do { return $ VariableRef ident }
