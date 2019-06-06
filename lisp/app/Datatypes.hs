module Datatypes where

import Control.Monad
import Control.Monad.Error
import Data.Either
import Control.Monad.Trans.Error hiding (catchError)
import Control.Monad.IO.Class
import Data.IORef -- stateful thread thing for envs, can only be used in IO monad

data ParseError = ParseError !SourcePos [String]

data LispError = NumArgs Integer [LispVal]
                | TypeMismatch String LispVal
                | Parse ParseError
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | Default String
                
instance Show LispError where show = showError
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected: " ++ show expected
                                ++ " args, found: " ++ show found                
showError (TypeMismatch expected found) = "Type mismatch, expected: "
                                ++ expected ++ ", found: " ++ show found         
showError (Parse (ParseError sourcepos _)) = "Parse error at" ++ show sourcepos


type SourceName = String
type Line       = Int
type Column     = Int

-- represends source (filename), line, and column of things
data SourcePos  = SourcePos SourceName !Line !Column
    deriving (Eq, Ord)

instance Show SourcePos where
  show (SourcePos name line column)
    | null name = showLineColumn
    | otherwise = "\"" ++ name ++ "\" " ++ showLineColumn
    where
      showLineColumn    = "(line " ++ show line ++
                          ", column " ++ show column ++
                          ")"

type Env = IORef [(String, IORef LispVal)] -- the type for states
type IOThrowsError = ErrorT LispError IO -- takes one more arg, which is return type of function


data LispVal = Atom String 
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String 
    | Bool Bool
    | PrimitiveFunc ([LispVal] -> IOThrowsError LispVal) -- throwserror?
    -- names of parameters, variable num of args, function body, func's env of creation 
    -- stored as record type
    | Func {params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env}

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
showVal (PrimitiveFunc _) = "<primitive func>"
-- just show header of function, aBsTrAcTiOn
showVal (Func {params=args, vararg=varargs, body=body, closure=env}) =
    "(lambda (" ++ unwords (map show args) ++ 
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ... )"

-- convert a list of lispVals to a human-readable string
unwordsList :: [LispVal] -> String 
unwordsList = unwords . map showVal











