module Transform (deriveBy, substitute, evaluate, optimise) where

import Data.Char
import Data.List
import Tree

functionDerivative :: SymTree -> String -> SymTree
functionDerivative f@(Fun n a) d = case n of
  "sin" -> Mul (deriveBy a d) (Fun "cos" a)
  "cos" -> Mul (Mul (Const (-1)) (deriveBy a d)) (Fun "sin" a)
  "exp" -> Mul (deriveBy a d) f
  "log" -> Div (Const 1) (Mul (deriveBy a d) (Fun "ln" (Const 10)))

deriveBy :: SymTree -> String -> SymTree
deriveBy (Const _)   _ = Const 0
deriveBy (Var   x)  v
  | x == v    = Const 1
  | otherwise = Const 0
deriveBy f@(Fun _ _) v = optimise $ functionDerivative f v
deriveBy (Add lt rt) v = optimise $ Add (deriveBy lt v) (deriveBy rt v)
deriveBy (Sub lt rt) v = optimise $ Sub (deriveBy lt v) (deriveBy rt v)
deriveBy (Mul lt rt) v =
  optimise $ Add (Mul lt (deriveBy rt v)) (Mul (deriveBy lt v) rt)
deriveBy (Div lt rt) v =
  optimise $ Div (Sub (Mul ltdv rt) (Mul lt rtdv)) (Pow rt 2)
  where ltdv = deriveBy lt v
        rtdv = deriveBy rt v
deriveBy (Pow lt n) v =
    optimise $ Mul (Mul (Const n) (deriveBy lt v)) (Pow lt $ n - 1)

substitute :: String -> SymTree -> Double -> SymTree
substitute v c@(Const _) _  = c
substitute v tv@(Var  x) vv
  | x == v    = Const vv
  | otherwise = tv
substitute v (Fun fn rt) vv =
  optimise $ Fun fn $ substitute v rt vv
substitute v (Add lt rt) vv =
  optimise $ Add (substitute v lt vv) (substitute v rt vv)
substitute v (Sub lt rt) vv =
  optimise $ Sub (substitute v lt vv) (substitute v rt vv)
substitute v (Mul lt rt) vv =
  optimise $ Mul (substitute v lt vv) (substitute v rt vv)
substitute v (Div lt rt) vv =
  optimise $ Div (substitute v lt vv) (substitute v rt vv)
substitute v (Pow lt n)  vv =
  optimise $ Pow (substitute v lt vv) n

e :: Double
e = 2.7182818

evaluate :: SymTree -> Double
evaluate (Const c)   = c
evaluate (Var _)     =
  error "Tree still contains variables, substitute them before evaluation"
evaluate (Fun fn rt) = case fn of
  "sin" -> sin rtval
  "cos" -> cos rtval
  "exp" -> exp rtval
  "log" -> log rtval
  "ln"  -> logBase e rtval
  where rtval = evaluate rt
evaluate (Add lt rt) = evaluate lt + evaluate rt
evaluate (Sub lt rt) = evaluate lt - evaluate rt
evaluate (Mul lt rt) = evaluate lt * evaluate rt
evaluate (Div lt rt) = evaluate lt / evaluate rt
evaluate (Pow lt n)  = (evaluate lt) ** n

-- Optimises SymTree by removing unnecessary nodes.
optimise :: SymTree -> SymTree
optimise (Add (Const 0) rt) = optimise rt
optimise (Add lt (Const 0)) = optimise lt
optimise (Add lt rt) = Add (optimise lt) (optimise rt)
optimise (Sub lt (Const 0)) = optimise lt
optimise (Sub lt rt) = Sub (optimise lt) (optimise rt)
optimise (Mul (Const 0) rt) = Const 0
optimise (Mul lt (Const 0)) = Const 0
optimise (Mul (Const 1) rt) = optimise rt
optimise (Mul lt (Const 1)) = optimise lt
optimise (Mul lt rt) = Mul (optimise lt) (optimise rt)
optimise (Div (Const 0) rt) = Const 0
optimise (Div lt (Const 0)) = Const $ 1 / 0
optimise (Div lt (Const 1)) = optimise lt
optimise (Div lt rt) = Div (optimise lt) (optimise rt)
optimise (Pow _ 0) = Const 1
optimise (Pow (Const 0) _) = Const 0
optimise (Pow lt 1) = optimise lt
optimise a = a
