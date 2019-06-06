module LispParsing (
parseString,
parseAtom,
parseNumber,
parseLispExpr,
parseList,
parseDottedList,
parseQuoted,
showVal,
LispVal(..)
)

where

import Parsing
import Control.Monad
import Control.Monad.Error
import Data.Either
import Control.Monad.Trans.Error hiding (catchError)
import Control.Monad.IO.Class
import Data.IORef -- stateful thread thing for envs, can only be used in IO monad
import Datatypes

-- data LispVal = Atom String 
--     | List [LispVal]
--     | DottedList [LispVal] LispVal
--     | Number Integer
--     | String String 
--     | Bool Bool
--     | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
--     -- names of parameters, variable num of args, function body, func's env of creation 
--     -- stored as record type
--     | Func {params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env}

------------------------------------------------------------------------------
-- Lisp-specific parsing functions
-- Atom String := stores string naming the atom
-- List [LispVal] := stores a list of LispVals (proper list)
-- DottedList [LispVal] LispVal := (a b . c), an improper list
-- Number Integer := a number
-- String String := a string
-- Bool Bool := a boolean
------------------------------------------------------------------------------

-- to parse Lisp strings
parseString :: Parser LispVal
parseString = do 
                char '"'               -- find opening quote
                x <- many (noneOf "\"") -- find rest of chars
                char '"'                -- closing quote
                return (String x)

-- to parse Lisp atoms
parseAtom :: Parser LispVal
parseAtom = do  first <- letter +++ symbol   -- first char in lispval can be letter or symbol
                rest <- many (letter +++ digit +++ symbol) -- other chars can also be num
                let atom = [first] Prelude.++ rest -- assemble the atom
                return $ case atom of           -- is it a true/false?  
                    "#t" -> Bool True           -- otherwise just return the val
                    "#f" -> Bool False          
                    otherwise -> Atom atom


-- to parse Lisp numbers
-- read this function right to left
-- many1 digit reads a string of digits
-- (Number . read) converts the string of digits into a number, then a LispVal
-- liftM lifts this func (Number . read) into a parser monad context
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit


-- to parse Lisp exprs
-- note that (+++) is the choice operator defined in Parsing.hs
parseLispExpr :: Parser LispVal -- @TODO should do spaces first...
parseLispExpr = 
        do  
            -- a <- spaces
            parseString     -- self explanatory 
            +++ parseAtom 
            +++ parseNumber
            +++ parseQuoted
            +++ do              -- list: try both options of proper and improper list
                char '('        -- 'try' lets us backtrack (doesn't consume input)
                x <- (try parseList) +++ parseDottedList
                char ')' -- note that try doesn't actually work!!!
                return x



-- to parse lisp lists
-- (sepby parseExpr spaces) is a parser that ignores spaces and parses exprs
-- liftM List (sepby parseExpr spaces) lifts List data constructor into a parser
-- combinators are actually amazing look how easy this is
parseList :: Parser LispVal
parseList = liftM List (sepby parseLispExpr spaces)


-- to parse improper lists (a b .c) i.e. [a | -> [b | c]]
parseDottedList :: Parser LispVal
parseDottedList = do
                    head <- endBy parseLispExpr spaces -- get proper list
                    tail <- char '.' >> spaces >> parseLispExpr -- throw out extra stuff, get tail
                    return (DottedList head tail)

-- to parse single quote in lisp
-- i.e. `(expt 2 3) 
-- note that quote expressions are evaluated as just data, not code
parseQuoted :: Parser LispVal
parseQuoted = do
                char '\'' -- get quote
                a <- parseLispExpr
                return (List [Atom "quote", a])

-- -- let there be a human-readable version of lispvals
-- instance Show LispVal where show = showVal

-- showVal :: LispVal -> String
-- showVal (String s) = "\"" Prelude.++ s Prelude.++ "\""
-- showVal (Atom name) = name
-- showVal (Number num) = show num
-- showVal (Bool True) = "#t"
-- showVal (Bool False) = "#f"
-- showVal (List l) = "(" Prelude.++ (unwordsList l) Prelude.++ ")"
-- showVal (DottedList head tail) = "(" Prelude.++ (unwordsList head) 
--     Prelude.++ " . " Prelude.++ (showVal tail) Prelude.++ ")"
-- showVal (PrimitiveFunc _) = "<primitive func>"
-- -- just show header of function, aBsTrAcTiOn
-- showVal (Func {params=args, vararg=varargs, body=body, closure=env}) =
--     "(lambda (" ++ unwords (map show args) ++ 
--         (case varargs of
--             Nothing -> ""
--             Just arg -> " . " ++ arg) ++ ") ... )"

-- -- convert a list of lispVals to a human-readable string
-- unwordsList :: [LispVal] -> String 
-- unwordsList = unwords . map showVal


















