
module Compiler.TypeChecking.Nameable where
    import Compiler.Syntax

    class Nameable a where
        nameOf :: a -> String


    instance Nameable a => Nameable (Positioned a) where
        nameOf (Positioned _ x) = nameOf x
        nameOf (AnyPosition x) = nameOf x

    instance Nameable Variable where
        nameOf (Variable _ name) = name

    instance Nameable Function where
        nameOf (Function _ name _ _) = name

    instance Nameable Symbol where
        nameOf (VarSymbol _ v) = nameOf v
        nameOf (FuncSymbol _ f) = nameOf f
