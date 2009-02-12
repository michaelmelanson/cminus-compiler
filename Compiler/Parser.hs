-- |The parser for the C-Minus compiler. This converts the token
--  stream produced by Compiler.Scanner into an abstract syntax tree.

module Compiler.Parser where
    import Compiler.Syntax
    import Compiler.Scanner

    import Text.ParserCombinators.Parsec 
    import Text.ParserCombinators.Parsec.Expr hiding (Operator)


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
        where maybeArrayLen t id = do { squares whiteSpace <?> "array bounds"
                                      ; return $ Variable (Pointer t) id
                                      }
                               <|> do { size <- squares integer <?> "array bounds"
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
                                    }
                             <|> do { return $ VariableRef ident }
                             


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
