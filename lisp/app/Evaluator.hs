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

