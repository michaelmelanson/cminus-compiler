module IPPrint(pshow, pprint) where

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Text.Read

pshow :: Show a => a -> String
pshow v =
    case parseModule ("value = "++s) of
      ParseOk         m -> tidy $ prettyPrint m
      ParseFailed _ _   -> s
    where
      s = show v
      tidy x =
          case readPrec_to_S skipBoring 0 x of
            [((), tail)] -> "   " ++ tail
            _            -> s


pprint :: Show a => a -> IO ()
pprint = putStrLn . pshow

skipBoring :: ReadPrec ()
skipBoring =
    do { Ident "value" <- lexP; Punc  "=" <- lexP; return () } <++
    do { lexP; skipBoring }
