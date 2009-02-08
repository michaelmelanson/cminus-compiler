module Compiler.Parser where
    import Compiler.Scanner

    import Text.ParserCombinators.Parsec 
    import Text.ParserCombinators.Parsec.Expr hiding (Operator)

    data Variable   = Variable Type String
                      deriving (Show, Eq)

    data Function   = Function Type String [Variable] Statement
                      deriving (Show, Eq)

    data Value      = IntValue Integer
                    | VariableRef String
                    | ArrayRef String Expression
                    | FunctionCall String [Expression]
                      deriving (Show, Eq)

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

    data Expression = AssignmentExpr Expression Expression
                    | ArithmeticExpr Operator Expression Expression
                    | ValueExpr Value
                      deriving (Show, Eq)


    data Statement = ExpressionStatement Expression
                   | CompoundStatement [Variable] [Statement]
                   | SelectionStatement Expression Statement Statement
                   | IterationStatement Expression Statement
                   | ReturnStatement
                   | ValueReturnStatement Expression
                   | NullStatement -- A nothing statement (e.g. missing "else")
                     deriving (Show, Eq)

    data Toplevel = ToplevelVar  Variable
                  | ToplevelFunc Function
                    deriving (Show, Eq)

    program = do { whiteSpace
                 ; p <- many1 toplevel_decl
                 ; return p
                 }

    toplevel_decl = do { t <- typeSpec
                       ; id <- identifier
                       ; x <- toplevelTail t id
                       ; return x
                       }
                   <?> "a function declaration or global variable"
        where toplevelTail t id = do { args <- parens params <?> "function arguments"
                                     ; body <- braces compound_stmt <?> "function body"
                                     ; return $ ToplevelFunc (Function t id args body)
                                     }  
                              <|> do { size <- squares (integer <?> "array size") <?> "array bounds"
                                     ; semi
                                     ; return $ ToplevelVar$Variable (Array t size) id
                                     }
                              <|> (semi >> (return $ ToplevelVar $ Variable t id))

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
                         <|> do { expr <- expression
                                ; semi
                                ; return $ ValueReturnStatement expr
                                } 
              

    expression = buildExpressionParser table factor
        where table = [[op ">=" (ArithmeticExpr GreaterOrEqual) AssocLeft,
                        op ">"  (ArithmeticExpr Greater)        AssocLeft,
                        op "<=" (ArithmeticExpr LessOrEqual)    AssocLeft,
                        op "<"  (ArithmeticExpr Less)           AssocLeft,
                        op "==" (ArithmeticExpr Equal)          AssocLeft,
                        op "!=" (ArithmeticExpr NotEqual)       AssocLeft],

                       [op "*"  (ArithmeticExpr Multiply)       AssocLeft,
                        op "/"  (ArithmeticExpr Divide)         AssocLeft],

                       [op "+"  (ArithmeticExpr Add)            AssocLeft,
                        op "-"  (ArithmeticExpr Subtract)       AssocLeft],

                       [op "="  AssignmentExpr AssocLeft]]
              op s f assoc = Infix (do{ reservedOp s; return f}) assoc

    factor  = (parens expression)
              <|> value_expression

    value_expression = do { val <- value
                          ; return $ ValueExpr val
                          }

    value = do { num <- integer; return $ IntValue num }
        <|> do { ident <- identifier
               ; x <- variableOrCall ident
               ; return x
               }
        where variableOrCall ident = do { index <- squares expression
                                        ; return $ ArrayRef ident index
                                        }
                                 <|> do { args <- parens (commaSep expression)
                                        ; return $ FunctionCall ident args
                                        }
                                 <|> do { return $ VariableRef ident }
