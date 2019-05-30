module Evaluator (
showVal
)

where

import LispParsing
-- import Parsing
-- data LispVal = Atom String 
--     | List [LispVal]
--     | DottedList [LispVal] LispVal
--     | Number Integer
--     | String String 
--     | Bool Bool

-- let there be a human-readable version of lispvals
instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String s) = "\"" Prelude.++ s Prelude.++ "\""
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List l) = "(" Prelude.++ (unwordsList l) Prelude.++ ")"
showVal (DottedList head tail) = "(" Prelude.++ (unwordsList head) 
    Prelude.++ " . " Prelude.++ (showVal tail) Prelude.++ ")"

-- convert a list of lispVals to a human-readable string
unwordsList :: [LispVal] -> String 
unwordsList = unwords . map showVal