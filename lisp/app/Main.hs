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
-- 2. This code takes in an AST and prints it (aka convert to String).
-- AST -> [printExpr] -> "1 (2 ( 3 4 5) 4)"
-----------------------------------------------------------------------------
-- our tree can be binary since we are only using binary ops

data AST a = Nil | Node a (AST a) (AST a) deriving Show

printExpr :: Show a => AST a -> String
printExpr Nil = "Nil"
printExpr (Node root left right) = 
	 show root ++ " (" ++ printExpr left ++ "), (" 
	 ++ printExpr right ++ ")" 
-- (), [] for left, right 

-- human-readable printing for debugging
-- https://stackoverflow.com/questions/12556469/nicely-printing-showing-a-binary-tree-in-haskell

debugPrint tree = unlines (debugPrintHelper tree)

debugPrintHelper (Node root left right) 
    = (show root) : (printSubtree left right)
        where 
            printSubtree left right =
            	((pad "+- " "|  ") (debugPrintHelper right))
            	    ++ ((pad "`- " "   ") (debugPrintHelper left))

            pad first rest = zipWith (++) (first : repeat rest)
debugPrintHelper Nil = [] 


-----------------------------------------------------------------------------
-- 3. This code takes in printed tree (String) and makes AST 
-- "1 (2 ( 3 4 5) 4)" -> [printExpr] -> AST
----------------------------------------------------------------------------

-- @TODO
-- convert this to parser type so we can apply many
-- also put into AST lmoa

-- parseExpr :: String -> [(String, String)]
-- doesn't do paren! only nabs root, need to delete th
parseExpr string = return $ apply (item `sepby` ((char '(' ) +++ (char ')')) ) string --"1 ( 2 ( 3 4)"

-- f string = do {a <- parseExpr; b <- space; c <- parseExpr; return c}

-- f :: [(String, String)] -> AST String 
-- f [("Nil", _)] = Nil
-- f [(root, rest)] = (Node root, rest, rest)




















