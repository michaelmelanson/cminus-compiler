module Compiler.CodeGeneration.LabelTable where
    import qualified Data.Map as M
    import Data.Maybe

    type LabelTable = M.Map String Int

    initLabelTable = M.empty :: LabelTable
    

    insertLabel table name a = M.insert name a table
    
    labelDefined table name = isJust $ M.lookup name table
    lookupLabel table name = fromJust $ M.lookup name table