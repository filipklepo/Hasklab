module Tree where

data SymTree = Var   String
             | Const Double
             | Fun   String  SymTree
             | Add   SymTree SymTree
             | Sub   SymTree SymTree
             | Mul   SymTree SymTree
             | Div   SymTree SymTree
             | Pow   SymTree Double
             deriving Eq

instance Show SymTree where
    show (Var   c)     = c
    show (Const n)     = show n
    show (Fun   n  a ) = n ++ " ( " ++ show a ++ " )"
    show (Add   l r)   = show l ++ " + "  ++ show r
    show (Sub   l r)   = show l ++ " - "  ++ show r
    show (Mul   l r)   = show l ++ " * "  ++ show r
    show (Div   l r)   = show l ++ " \\ " ++ show r
    show (Pow   l e )  = show l ++ " ^ "  ++ show e

-- Extracts all occurrences of variables from given symbolic tree.
variables :: SymTree -> [String]
variables (Const _)   = []
variables (Var v)     = [v]
variables (Fun n rt)  = variables rt
variables (Add lt rt) = variables lt ++ variables rt
variables (Sub lt rt) = variables lt ++ variables rt
variables (Mul lt rt) = variables lt ++ variables rt
variables (Div lt rt) = variables lt ++ variables rt
variables (Pow lt n)  = variables lt
