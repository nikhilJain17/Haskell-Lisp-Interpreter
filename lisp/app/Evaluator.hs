module Evaluator (
eval
)

where

import LispParsing

-- data LispVal = Atom String 
--     | List [LispVal]
--     | DottedList [LispVal] LispVal
--     | Number Integer
--     | String String 
--     | Bool Bool


-- This module is the evaluator.
-- i.e. evaluator :: Code -> Data
-- Lisp is nice because Code and Data are both the same data type.
-- evaluator :: LispVal -> LispVal

eval :: LispVal -> LispVal
-------------------------
-- PRIMITIVES
-- These first few pattern matches are the most straightforward cases.
-- The expressions here are just basic data, so we evaluate them as such.
eval val@(String _) = val -- bind val to ANY lispval that is a string (i.e. atom, string)
eval val@(Number _) = val -- same but for number
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val -- match for quoted expr (i.e. straight data)
--------------------------
-- FUNCTION APPLICATION
eval (List (Atom func: args)) = applyFunc func (map eval args) -- [func, arg1, arg2,...]



-- apply function to arguments
-- function is an operator stored in dictionary called primitives
applyFunc :: String -> [LispVal] -> LispVal
applyFunc func args = maybe (Bool False) ($ args) $ lookup func primitives


-- dictionary of primitive operations
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

-- map an operator to code
-- (map unpackNum args) goes from [LispVal] to [Integer] (convert args to nums)
-- foldl op (map unpackNum args) goes from [Integer] to Integer (eval nums)
-- Number $ ... puts result into a LispVal
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op args = Number $ foldl1 op $ map unpackNum args

-- unpackNum converts numeric LispVal to Integer
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String s) = let parsed = reads s in
                        if null parsed 
                            then 0
                        else fst $ parsed !! 0

unpackNum (List [n]) = unpackNum n -- one element list only
unpackNum _ = 0














