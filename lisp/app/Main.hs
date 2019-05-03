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

--				   root    left    right   parent
data AST a = Nil | Node a (Node a) (Node a) deriving Show
data Node a = a Node a -- have some notion of parent?

printExpr :: Show a => AST a -> String
printExpr Nil = ""
printExpr (Node root left right) = 
	 show root ++ " ( " ++ printExpr left ++ " ) " ++ " ( " 
	 ++ printExpr right ++ " ) " 
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


-- @TODO convert this into a monad!
-- proof of concept at the moment
-- @ TODO get rid of parens!!!!!!
parseExpr :: String -> [(String, String)]
parseExpr input = apply (item `sepby` ((symb "(" ) +++ (symb ")"))) input --"1 ( 2 ( 3 4)"

firstLast::[a]->[a]
firstLast [] = []
firstLast [x] = []
firstLast xs = tail (init xs)


-- @TODO
-- 1. Get rid of the parenthesis! (done)
-- 2. Figure out left and right 

-- guard expr on root () and []
-- handle parens!

-- parseTree :: String -> AST String -> AST String
-- parseTree input (root left right parent) =
-- 	| fst result == "(" = parseTree (tail input) left
-- 	| fst result `elem` ["+", "-", "/", "*"] =  
	-- where result = (parseExpr input)!!0


-- ignore this
comb :: String -> AST -> AST String
comb "" _ = Nil
comb input (root left right (proot pleft pright))
	| fst result == "(" = comb (tail input) -- get rid of paren, handle left case
	| fst result == ")" = comb (tail input) -- ...
	| fst result == " " = comb (tail input) side -- get rid of space
	| fst result `elem` ["+", "-", "/", "*"] = (Node (fst result) Nil (comb (snd result) root)
    | isDigit (fst result) = (Node (fst result) )

	where result = (parseExpr input)!!0


-- old version of comb that cannot handle traversing up the tree
-- comb :: String -> AST String
-- comb "" = Nil
-- comb input
-- 	| fst result == "(" = comb (tail input) -- get rid of paren, handle left case
-- 	| fst result == ")" = comb (tail input) -- am i dumb
-- 	| fst result == "[" = comb (tail input) -- lmoa
-- 	| fst result == "]" = comb (tail input) -- lmoa
-- 	| fst result == " " = comb (tail input) -- get rid of space
-- 	| otherwise = (Node (fst result) (comb (snd result)) (Nil)) 
-- 	-- | check for right child, do (Node fs)
-- 	where result = (parseExpr input)!!0



















