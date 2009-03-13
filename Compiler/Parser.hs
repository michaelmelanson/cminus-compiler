-- |The parser for the C-Minus compiler. This converts the token
--  stream produced by Compiler.Scanner into an abstract syntax tree.

module Compiler.Parser where
    import Compiler.Syntax
    import Compiler.Scanner

    import Text.ParserCombinators.Parsec 
    import Text.ParserCombinators.Parsec.Prim
    import Text.ParserCombinators.Parsec.Expr hiding (Operator)

    import Monad

    -- |The top parser. This parses an entire C-Minus source file
    program = do { whiteSpace
                 ; syms <- many1 toplevel_decl
                 ; return $ Program syms
                 }

    -- |Parser for one top-level declaration (either a global variable or a function)
    toplevel_decl = function_declaration <|> global_variable

    function_declaration = try (do t <- typeSpec
                                   id <- identifier
                                   args <- parens params
                                           <?> "function arguments"
                                            
                                   body <- braces compound_stmt
                                           <?> "function body"
                                            
                                   returnWithPosition $ FuncSymbol $ Function t id args body)
                           <?> "function declaration"

    global_variable = try (do t <- typeSpec
                              id <- identifier
                              size <- squares (integer <?> "array size")
                                      <?> "array bounds"
                              semi
                              returnWithPosition $ VarSymbol $ Variable (Array t size) id)
                      <|>
                      try (do t <- typeSpec
                              id <- identifier
                              semi
                              returnWithPosition $ VarSymbol $ Variable t id)
                         
                      <?> "global variable"

    var_declaration = do { p <- param
                         ; semi
                         ; return p
                         }
           

    param = try (do t <- typeSpec
                    id <- identifier
                    size <- squares (integer <?> "array size")
                            <?> "array bounds"
                    returnWithPosition $ Variable (Array t size) id)
            <|>
            try (do t <- typeSpec
                    id <- identifier
                    squares whiteSpace
                    returnWithPosition $ Variable (Pointer t) id)
            <|>
            try (do t <- typeSpec
                    id <- identifier
                    returnWithPosition $ Variable t id)


    params = (reserved "void" >> return []) <|> commaSep1 param

    statement = do var_declaration
                   unexpected "variable declaration (variables must be declared before any statements)"

                <|> return_stmt
                <|> braces compound_stmt
                <|> selection_stmt
                <|> iteration_stmt
                <|> expression_stmt
                <?> "statement"

    expression_stmt = do expr <- expression
                         semi
                         returnWithPosition $ ExpressionStatement expr

    compound_stmt = do vars  <- many (var_declaration <?> "local variable")
                       stmts <- many statement
                       returnWithPosition $ CompoundStatement vars stmts


    selection_stmt = do reserved "if"
                        condExpr   <- parens expression
                        thenClause <- statement
                        elseClause <- else_clause
                        returnWithPosition $ SelectionStatement condExpr
                                                                thenClause
                                                                elseClause

        where else_clause = do reserved "else"
                               stmt <- statement
                               return stmt

                        <|> returnWithPosition NullStatement

    iteration_stmt = do reserved "while"
                        condExpr <- parens expression
                        body <- statement
                        returnWithPosition $ IterationStatement condExpr body

    return_stmt = do reserved "return"
                     x <- return_value
                     return x

        where return_value = (semi >> returnWithPosition ReturnStatement)
                         <|> do expr <- simple_expression
                                semi
                                returnWithPosition $ ValueReturnStatement expr

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
        where table = [[op ">=" (ArithmeticExpr GreaterOrEqual) AssocNone,
                        op ">"  (ArithmeticExpr Greater)        AssocNone,
                        op "<=" (ArithmeticExpr LessOrEqual)    AssocNone,
                        op "<"  (ArithmeticExpr Less)           AssocNone,
                        op "==" (ArithmeticExpr Equal)          AssocNone,
                        op "!=" (ArithmeticExpr NotEqual)       AssocNone],

                       [op "*"  (ArithmeticExpr Multiply)       AssocLeft,
                        op "/"  (ArithmeticExpr Divide)         AssocLeft],

                       [op "+"  (ArithmeticExpr Add)            AssocLeft,
                        op "-"  (ArithmeticExpr Subtract)       AssocLeft]]
              op s f assoc = Infix (do{ reservedOp s; return f}) assoc

    factor  = (parens simple_expression)
              <|> value_expression

    value_expression = do val <- value
                          return $ ValueExpr val

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
