module Main where

import Lib
import Parsing
import Data.Char

main :: IO ()
main = putStrLn "use ghci"

-- This is an example of parsers that parse math expr and evals their result.
-- Here's the data types we are implicitly using.
-- In other words, this is the way we model math exprs.

-- expr := expr addop term | term
-- expr := term mulop factor | factor
-- factor := digit | expr
-- digit := 0 | 1 | ... | 9
-- addop := + | -
-- mulop := * | /

-----------------------------------------------------------------------------
-- 1. This code takes in an input and evaluates while parsing.
-- " 1 + 2 * 3 - 4 " -> [interpretExpr] -> [(-1, "")]
-----------------------------------------------------------------------------
interpretExpr :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)

-- expr and term are two high level parsers.
-- recall an expr is a bunch of terms addop-ed...
interpretExpr = interpretTerm `chainl1` addop 
-- ...and a term is a bunch of factors mulop-ed
interpretTerm = interpretFactor `chainl1` mulop 

-- these parsers are the 'base cases' which do the gritty work.
-- a factor is a digit or (expression)
interpretFactor = interpretDigit +++ do {symb "("; n <- interpretExpr; symb ")"; return n}
-- a digit is a number
interpretDigit = do {x <- token (sat isDigit); return (ord x - ord '0')}

-- these are just hunting for the operators.
addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}
mulop = do {symb "*"; return (*)} +++ do {symb "/"; return (div)}

-----------------------------------------------------------------------------
-- 2. This code takes in an AST and pretty-prints it (aka convert to String).
-- AST -> [printExpr] -> "1 (2 ( 3 4 5) 4)"
-----------------------------------------------------------------------------
-- our tree can be binary since we are only using binary ops

data AST a = Nil | Node a (AST a) (AST a) deriving Show

-- https://stackoverflow.com/questions/12556469/nicely-printing-showing-a-binary-tree-in-haskell

printExpr tree = unlines (printExprHelper tree)

-- printExprHelper :: 
printExprHelper (Node root left right) 
    = (show root) : (printSubtree left right)
        where 
            printSubtree left right =
            	((pad "+- " "|  ") (printExprHelper right))
            	    ++ ((pad "`- " "   ") (printExprHelper left))

            pad first rest = zipWith (++) (first : repeat rest)
printExprHelper Nil = [] 


printExpr2 Nil = "Nil"
printExpr2 (Node root left right) = 
	 show root ++ " (" ++ printExpr2 left ++ "), (" 
	 ++ printExpr2 right ++ ")" 

-----------------------------------------------------------------------------
-- 3. This code takes in pretty-print tree (str) and makes AST 
-- "1 (2 ( 3 4 5) 4)" -> [printExpr] -> AST
-----------------------------------------------------------------------------

-- parser for newlines
newline = char 'a'























