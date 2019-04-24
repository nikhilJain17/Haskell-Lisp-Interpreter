module Main where

import Lib
import Parsing
import Data.Char

main :: IO ()
main = putStrLn "use ghci"
-- main = putStrLn (fst ( apply expr " 1 - 2 * 3 + 4 " ) !! 1)
-- main = putStrLn ( convert (isSpace '\n') )

-- This is an example of parsers that parse math expr and evals their result.
-- Here's the data types we are implicitly using.
-- In other words, this is the way we model math exprs.

-- expr := expr addop term | term
-- expr := term mulop factor | factor
-- factor := digit | expr
-- digit := 0 | 1 | ... | 9
-- addop := + | -
-- mulop := * | /

expr :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)

-- expr and term are two high level parsers.
-- recall an expr is a bunch of terms addop-ed...
expr = term `chainl1` addop 
-- ...and a term is a bunch of factors mulop-ed
term = factor `chainl1` mulop 

-- these parsers are the 'base cases' which do the gritty work.
-- a factor is a digit or (expression)
factor = digit +++ do {symb "("; n <- expr; symb ")"; return n}
-- a digit is a number
digit = do {x <- token (sat isDigit); return (ord x - ord '0')}

-- these are just hunting for the operators.
addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}
mulop = do {symb "*"; return (*)} +++ do {symb "/"; return (div)}
















