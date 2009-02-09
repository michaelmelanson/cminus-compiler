-- Scanning rules for the C- language

module Compiler.Scanner where
    import Compiler.Syntax

    import Text.ParserCombinators.Parsec
    import Text.ParserCombinators.Parsec.Language
    import qualified Text.ParserCombinators.Parsec.Token as P

    cMinusStyle = emptyDef {
                         commentStart   = "/*"
                       , commentEnd     = "*/"
                       , commentLine    = "//"
                       , nestedComments = True
                       , identStart     = letter
                       , identLetter    = alphaNum <|> oneOf "_'"
                       , reservedNames  = []
                       , reservedOpNames= []	
                       , caseSensitive  = False	   
                       }

    scanner = P.makeTokenParser cMinusStyle

    whiteSpace  = P.whiteSpace scanner
    integer     = P.integer scanner
    parens      = P.parens scanner
    braces      = P.braces scanner
    squares     = P.squares scanner
    semi        = P.semi scanner <?> "semi-colon"
    identifier  = P.identifier scanner
    reserved    = P.reserved scanner
    reservedOp  = P.reservedOp scanner
    commaSep    = P.commaSep scanner
    commaSep1   = P.commaSep1 scanner
    typeSpec    = (reserved "int" >> return Int)
              <|> (reserved "void" >> return Void)
