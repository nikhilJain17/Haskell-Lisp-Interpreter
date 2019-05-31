module Evaluator (
eval,
throwError,
-- Parser,
ParseError(..),
ThrowsError,
LispError(..)
)

where

import Parsing
import LispParsing
import Control.Monad.Error

-- This module is the evaluator.
-- i.e. evaluator :: Code -> Data
-- Lisp is nice because Code and Data are both the same data type.
-- evaluator :: LispVal -> LispVal

-- data LispVal = Atom String 
--     | List [LispVal]
--     | DottedList [LispVal] LispVal
--     | Number Integer
--     | String String 
--     | Bool Bool


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

-------------------------------------------------------------------
-- Error Handling

type SourceName = String
type Line       = Int
type Column     = Int

-- represends source (filename), line, and column of things
data SourcePos  = SourcePos SourceName !Line !Column
    deriving ( Eq, Ord)

data ParseError = ParseError !SourcePos [String]


data LispError = NumArgs Integer [LispVal]
                | TypeMismatch String LispVal
                | Parser ParseError
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | Default String

instance Show SourcePos where
  show (SourcePos name line column)
    | null name = showLineColumn
    | otherwise = "\"" ++ name ++ "\" " ++ showLineColumn
    where
      showLineColumn    = "(line " ++ show line ++
                          ", column " ++ show column ++
                          ")"

instance Show LispError where show = showError

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected: " ++ show expected
                                ++ " args, found: " ++ show found                
showError (TypeMismatch expected found) = "Type mismatch, expected: "
                                ++ expected ++ ", found: " ++ show found         
showError (Parser (ParseError sourcepos _)) = "Parse error at" ++ show sourcepos

instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default


-- type for funcs that may throw a LispError or may return a value
-- this is a CURRIED DATA CONSTRUCTOR!
-- i.e. if f :: Int->Int, then ThrowsError Integer => Either LispError Integer
type ThrowsError = Either LispError

-- all our errors turn into strings and get returned
-- catchError :: Either a b -> (error -> Either) -> 
trapError action = catchError action (return . show)

-- extract data from either monad
-- not to be used with Left, since that's an error
extractValue :: ThrowsError a -> a
extractValue (Right val) = val










