module Parse (createExpression) where

import Data.Char
import Data.List
import Transform
import Tree

-- Creates symbolic tree representation of given expression.
createExpression :: String -> SymTree
createExpression = optimise . parse . infixToPostfix . words

-- Parses a given list of strings, representing an expression in postfix
-- notation, into a symbolic tree.
parse :: [String] -> SymTree
parse = flip parser []

parser :: [String] -> [SymTree] -> SymTree
parser [] [r]    = r
parser (x:xs) oq
    | isVariable x = parser xs $ append oq (Var x)
    | isConstant x = parser xs $ append oq (Const (read x))
    | isFunction x = parser xs $ append ioq (Fun x l)
    | x == "*"     = parser xs $ append iioq (Mul sl l)
    | x == "/"     = parser xs $ append iioq (Div sl l)
    | x == "+"     = parser xs $ append iioq (Add sl l)
    | x == "-"     = parser xs $ append iioq (Sub sl l)
    | x == "^"     = parser xs $ append iioq (Pow sl $ treeConstVal l)
    where append xs x  = xs ++ [x]
          (l, ioq)      = (last oq, init oq)
          (sl, iioq) = (last $ init oq, init $ init oq)
          treeConstVal (Const n) = n

-- Takes expression in infix notation and transforms it into postfix notation.
infixToPostfix :: [String] -> [String]
infixToPostfix xs = infixToPostfixr xs [] []

-- Tranformator from infix to postix notation. Its arguments are list of input
-- tokens, operators stack and output queue.
infixToPostfixr :: [String] -> [String] -> [String] -> [String]
infixToPostfixr []     [] oq = oq
infixToPostfixr []     os oq = oq ++ os
infixToPostfixr (x:xs) os oq
  | isConstant x || isVariable x =
    infixToPostfixr xs os (oq ++ [x])
  | isFunction x || isLeftParenthese x = infixToPostfixr xs (x:os) oq
  | isOperator x = let (r, ros) = splitOsStack (poppable x) in
      infixToPostfixr xs (x:ros) (oq ++ r)
  | isRightParenthese x = let (r, ros) = tailSnd $ splitOsStack (/= "(") in
      if not $ null ros then infixToPostfixr xs ros (oq ++ r)
      else error $ "Invalid expression: Right parenthese does not have "
                  ++ "a matching left parenthese. " ++ (show oq)
  | otherwise = error $ "Unknown input: " ++ x
  where splitOsStack f = span f os

tailSnd :: (a, [b]) -> (a, [b])
tailSnd (a, []) = (a, [])
tailSnd (a, ys) = (a, tail ys)

-- Determines, for given current operator token and stack top
-- (which can be operator or function), whether the stack top must be
-- popped or not.
poppable :: String -> String -> Bool
poppable _  "(" = False
poppable ct st
  | isFunction st = True
  | leftAssoc  ct = precedence ct <= precedence st
  | otherwise     = precedence ct < precedence st
  where leftAssoc "^" = False
        leftAssoc _   = True

-- Determines the precedence value for given operator.
precedence :: String -> Int
precedence o
  | not . elem o
    $ concatMap fst precArr = error $ "Unknown operator " ++ o
  | otherwise = head . map snd $ filter (elem o . fst) precArr
  where precArr = [(["^"], 4),(["*", "/"], 3),(["+", "-"], 2)]

isConstant :: String -> Bool
isConstant = all isDigit

isOperator :: String -> Bool
isOperator = flip elem ["+","-","*","/","^"]

isFunction :: String -> Bool
isFunction = flip elem ["sin", "cos", "exp", "log", "ln"]

isLeftParenthese :: String -> Bool
isLeftParenthese = (== "(")

isRightParenthese :: String -> Bool
isRightParenthese = (== ")")

isVariable :: String -> Bool
isVariable x =
  (not . or $ map ($ x) [isConstant, isOperator, isFunction,
   isLeftParenthese, isRightParenthese]) && length x == 1
   && (not . isDigit $ head x)
