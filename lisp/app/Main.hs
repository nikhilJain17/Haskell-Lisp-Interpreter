module Main where

import Lib
import Parsing
import Data.Char
import Data.List

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
-- AST -> [printExpr] -> "1 (2 (3 (4) (5)) 4)"
-----------------------------------------------------------------------------
-- our tree can be binary since we are only using binary ops

--                 root    left    right   
data AST a = Nil | Node a (AST a) (AST a) deriving Show
-- data Node a = a Node a -- have some notion of parent?

-- printExpr :: Show a => AST a -> String
printExpr :: AST String -> String
printExpr (Node root _ Nil) = root
printExpr (Node root Nil _) = root
printExpr (Node root left right) = 
     "(" ++ printExpr left ++ " " ++ root 
     ++ " " ++ printExpr right ++ ")" 

-- human-readable printing for debugging
-- https://stackoverflow.com/questions/12556469/nicely-printing-showing-a-binary-tree-in-haskell
-- to use: putStrLn (debugPrint tree)
debugPrint :: AST String -> String
debugPrint tree = unlines (debugPrintHelper tree)

-- debugPrintHelper :: AST String -> String
debugPrintHelper (Node root left right) 
    = root : (printSubtree left right)
        where 
            printSubtree left right =
                ((pad "+- " "|  ") (debugPrintHelper right))
                    ++ ((pad "`- " "   ") (debugPrintHelper left))

            pad first rest = zipWith (++) (first : repeat rest)
debugPrintHelper Nil = [] 


------------------------------------------------------------------------------
-- 3. Parse input expression to AST
-- i.e. String -> AST String
-------------------------------------------------------------------------------
parseExpr :: String -> AST String
parseExpr input
    -- order of ops: look for parenthesis
    | scanParen input = Node parenRoot (parseExpr parenLeftNode) (parseExpr parenRightNode)
    -- next, look for addops
    | scanAddop input = Node addopRoot (parseExpr addopLeftNode) (parseExpr addopRightNode) 
    -- then, look for mulops
    | scanMulop input = Node mulopRoot (parseExpr mulopLeftNode) (parseExpr mulopRightNode)
    -- finally, check if they are numbers???   
    | otherwise = Node input Nil Nil
        where 
            addopRoot = (splitAddop input)!!0    
            parenRoot = (splitParen input)!!0    
            mulopRoot = (splitMulop input )!!0   
            addopLeftNode = (splitAddop input)!!1    
            parenLeftNode = (splitParen input)!!1    
            mulopLeftNode = (splitMulop input )!!1
            addopRightNode = (splitAddop input)!!2    
            parenRightNode = (splitParen input)!!2    
            mulopRightNode = (splitMulop input )!!2   





-- given an input expression, return if it has parenthesis
scanParen :: String -> Bool
scanParen s = isInfixOf "(" s 

-- given a VALID parenthesized input expression, split at outermost op
-- i.e. "(3 * 4) + (2 - 4)" => ["+", "(3 * 4)", "(2 - 4)"]

-- @TODO HANDLE EDGE CASE
-- ((3 + 4) - 9)
splitParen :: String -> [] String
splitParen s = splitParenHelper s "" 0

splitParenHelper :: String -> String -> Int -> [] String
splitParenHelper (c:cs) leftStr parenCount 
    | parenCount == 0 && c `elem` ['+', '-', '*', '/'] = [[c], leftStr, cs] -- outer paren, found op, done 
    | c == '(' = splitParenHelper cs (leftStr ++ [c]) (parenCount + 1) -- left paren
    | c == ')' = splitParenHelper cs (leftStr ++ [c]) (parenCount - 1) -- right paren
    -- (expr) case, i.e. full expression enclosed in parens
    | null (c:cs) = splitParen (removeParen leftStr)
    | otherwise = splitParenHelper cs (leftStr ++ [c]) parenCount -- no paren or outer op

-- remove first and last paren from string
removeParen :: String -> String
removeParen str = tail (init str)

-- given an input expression, return if it has mulops
scanMulop :: String -> Bool
scanMulop s = (isInfixOf "*" s) || ((isInfixOf "/" s))

-- given a non-parenthesized input expr, split at multiplication ops
-- i.e. "3 * 4 + 2" => ["*", "3", "4 + 2"]
splitMulop :: String -> [] String
splitMulop s = splitMulopHelper s ""

-- leftStr is accumulation of LHS of op
splitMulopHelper :: String -> String -> [] String
splitMulopHelper (c:cs) leftStr
    | (c == '*') || (c == '/') = [[c], leftStr, cs]
    | otherwise = splitMulopHelper cs (leftStr ++ [c])

-- given an input expression, return if it has addops
scanAddop :: String -> Bool
scanAddop s = (isInfixOf "+" s) || ((isInfixOf "-" s))

-- split by addop, no precedence rules needed
splitAddop :: String -> [] String
splitAddop s = splitAddopHelper s ""

splitAddopHelper :: String -> String -> [] String
splitAddopHelper (c:cs) leftStr
    | (c == '+') || (c == '-') = [[c], leftStr, cs]
    | otherwise = splitAddopHelper cs (leftStr ++ [c])



------------------------------------------------------------------------------
-- 4. Evaluate an AST of expressions
-- i.e. AST String -> Int
-------------------------------------------------------------------------------

-- expr := expr addop term | term
-- expr := term mulop factor | factor
-- factor := digit | expr
-- digit := 0 | 1 | ... | 9
-- addop := + | -
-- mulop := * | /

-- highest level function that handles logic for is this a term or expression
-- i.e. handles order of operations 


-- @TODO let it work when you have spaces!!!
-- i.e. + (2 4) errors since the root node is Node "+ " with an extra space
-- @TODO handle divide by zero

evalTree :: AST String -> Int
evalTree (Node n Nil Nil) = read n
evalTree (Node n left right)
    -- | left == Nil && right == Nil = read n -- tree is just a number at root
    | n `elem` ["+", "-"] = evalExpr (Node n left right)
    -- | (isInfixOf "+" n) || (isInfixOf "-" n)
    | n `elem` ["*", "/"] = evalTerm (Node n left right)
    | otherwise = 0 -- invalid op

-- lower level function that handles +/- expressions 
evalExpr :: AST String -> Int
evalExpr (Node op left right)
    | op == "+" = (evalTree left) + (evalTree right)
    | op == "-" = (evalTree left) - (evalTree right)
    | otherwise = 0 -- invalid op

-- lower level function that handles * / terms 
evalTerm :: AST String -> Int
evalTerm (Node op left right)
    | op == "*" = (evalTree left) * (evalTree right)
    | op == "/" = (evalTree left) `div` (evalTree right) -- handle divide by zero
    | otherwise = 0 -- invalid op


-----------------------------------------------------------------------------
-- ***[DEPRECATED]***
-- Note: this code goes from String version of AST to AST
-- Does not go from human-readable input to AST!
-- This was the old parseExpr.
-- ****************** 
-- This code takes in printed tree (String) and makes AST 
-- "1 (2 ( 3 4 5) 4)" -> [printExpr] -> AST
----------------------------------------------------------------------------

-- function that nabs the root from a subtree
root :: String -> String
root "" = ""
root input = 
    if first == "(" 
        then ""
        else first Prelude.++ (root (tail input))
    where first = take 1 input


-- function that nabs first subtree i.e. the stuff between the parens
-- e.g. "4(1(2)(3))(3()())" --> "1(2)(3)""
subtree :: String -> String
subtree "()" = ""
subtree "" = " "
subtree (s:cs) = -- hunt for the first left paren
    if s == '(' then subtreeHelper cs 1 else subtree cs


-- get left subtree
leftSubtree :: String -> String
leftSubtree s = init $ subtree s

-- get right subtree
rightSubtree :: String -> String
rightSubtree s = -- chop off root and left subtree and eat the rest
    init $ subtree $ take (length s) $ drop (length extra) s
    where extra = (root s) ++ (leftSubtree s)

subtreeHelper :: String -> Int -> String
subtreeHelper input numLeft = -- figure out how to combine recursive case
    if s == ")" then 
        if (numLeft - 1) == 0 then ")"
        else s Prelude.++ (subtreeHelper (tail input) (numLeft-1))
    else if s == "(" 
        then s Prelude.++ (subtreeHelper (tail input) (numLeft+1)) 
        else s Prelude.++ (subtreeHelper (tail input) numLeft) 
    where s = take 1 input 


-- main func from String -> AST
parseExpr' :: String -> AST String
parseExpr' "" = Nil
parseExpr' s = 
    (Node (root s) (parseExpr' (leftSubtree s)) (parseExpr' (rightSubtree s)))












