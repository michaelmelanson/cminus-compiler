
module Compiler.CodeGeneration.InstructionSet where

    type Register = Int

    data Address  = Memory Int Register  -- ^A memory reference

                  -- |An unresolved label reference. These are
                  -- |generated during compilation, but soon resolved
                  -- |to memory references.
                  | LabelRef  Int String 

                  -- |An unresolved symbol reference.
                  | SymbolRef Int String
                    deriving (Show, Eq)

    type Block = [Instruction]

    data Instruction = LABEL String -- ^A pseudo-instruction for marking positions
                                    --  in instruction lists

                     | SYMBOL String -- ^A pseudo-instruction that
                                     -- defines a symbol's address

                     -- ^Well, what do you think it is? :)
                     | COMMENT String

                     -- The RO instructions
                     | HALT
                     | IN   Register
                     | OUT  Register
                     | ADD  Register Register Register
                     | SUB  Register Register Register
                     | MUL  Register Register Register
                     | DIV  Register Register Register

                     -- The RM instructions
                     | LD   Register Address
                     | LDA  Register Address
                     | LDC  Register Int
                     | ST   Register Address
                     | JLT  Register Address
                     | JLE  Register Address
                     | JGE  Register Address
                     | JGT  Register Address
                     | JEQ  Register Address
                     | JNE  Register Address
                       deriving (Eq)
                       
    instance Show Instruction where
        show HALT           = showRO "HALT" 0 0 0
        show (IN  r)        = showRO "IN"   r 0 0
        show (OUT r)        = showRO "OUT"  r 0 0

        show (ADD x y z)    = showRO "ADD"  x y z 
        show (SUB x y z)    = showRO "SUB"  x y z 
        show (MUL x y z)    = showRO "MUL"  x y z 
        show (DIV x y z)    = showRO "DIV"  x y z 

        show (LD  r a)      = showRM "LD"   r a
        show (LDA r a)      = showRM "LDA"  r a
        show (LDC r a)      = showRC "LDC"  r a
        show (ST  r a)      = showRM "ST"   r a
        show (JLT r a)      = showRM "JLT"  r a
        show (JLE r a)      = showRM "JLE"  r a
        show (JGE r a)      = showRM "JGE"  r a
        show (JGT r a)      = showRM "JGT"  r a
        show (JEQ r a)      = showRM "JEQ"  r a
        show (JNE r a)      = showRM "JNE"  r a

        show (LABEL   name) = "* label: "  ++ show name
        show (SYMBOL  name) = "* symbol: " ++ show name
        show (COMMENT c)    = "* " ++ c

    showRO op x y z = op     ++ " " ++ 
                      show x ++ "," ++
                      show y ++ "," ++
                      show z

    showRC op r c = op     ++ " " ++
                    show r ++ "," ++
                    show c ++ "(0)"

    showRM op r m = op     ++ " " ++
                    show r ++ "," ++
                    showM m
        where showM (Memory o r) = show o ++ "(" ++ show r ++ ")"

              showM (LabelRef  _ name) = "**LABEL "  ++ show name ++ "**"
              showM (SymbolRef _ name) = "**SYMBOL " ++ show name ++ "**"


    pc  = 7 :: Register    -- |Program counter
    gp  = 6 :: Register    -- |Global pointer (points to high end of memory)
    fp  = 5 :: Register    -- |Frame pointer (points to current frame pointer)
    ac  = 0 :: Register    -- |Accumulation register 0
    ac1 = 1 :: Register    -- |Accumulation register 1



    isComment (COMMENT _) = True

-- Remove these two:
    isComment (LABEL _) = True
    isComment (SYMBOL _) = True

    isComment _           = False