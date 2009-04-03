
module Compiler.CodeGeneration.LabelResolution(resolveLabels) where
    import Compiler.CodeGeneration.InstructionSet
    import Compiler.CodeGeneration.LabelTable

    resolveLabels instrs labels = map (resolveLabel labels) instrs

    resolveLabel labels (JLT r (LabelRef a name)) = (JLT r (labelOffset labels name a))
    resolveLabel labels (JLE r (LabelRef a name)) = (JLE r (labelOffset labels name a))
    resolveLabel labels (JGE r (LabelRef a name)) = (JGE r (labelOffset labels name a))
    resolveLabel labels (JGT r (LabelRef a name)) = (JGT r (labelOffset labels name a))
    resolveLabel labels (JEQ r (LabelRef a name)) = (JEQ r (labelOffset labels name a))
    resolveLabel labels (JNE r (LabelRef a name)) = (JNE r (labelOffset labels name a))
    resolveLabel labels (LD  7 (LabelRef a name)) = (LD  7 (labelOffset labels name a))
    resolveLabel _ i = i
    
    labelOffset :: LabelTable -> String -> Int -> Address
    labelOffset labels name instAddress | labelDefined labels name  = Memory (labelAddress - instAddress - 1) pc
                                        | otherwise = error ("undefined label " ++ name)
        where labelAddress = lookupLabel labels name