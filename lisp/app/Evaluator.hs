{-# LANGUAGE ExistentialQuantification #-} -- we are using existentials to squash together our unpacker funcs into a single type
module Evaluator (
evalLispExpr,
--throwError,
extractValue,
trapError,
ParseError(..),
ThrowsError,
LispError(..),
SourcePos(..),
Env,
nullEnv,
runIOThrows,
liftThrows
)

where

import Parsing
import LispParsing
import Control.Monad.Error
import Data.Either
import Control.Monad.Trans.Error hiding (catchError)
import Control.Monad.IO.Class
import Data.IORef -- stateful thread thing for envs, can only be used in IO monad

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

-----------------------------
-- Variables and assignments: 
-- IORef is a state monad (box of mutable state) that gets passed around and mutated by IO actions
-- another option is State monad, but we are using IO anyways so let's use IORef
type Env = IORef [(String, IORef LispVal)] -- the type for states
type IOThrowsError = ErrorT LispError IO -- takes one more arg, which is return type of function



evalLispExpr :: Env -> LispVal -> IOThrowsError LispVal
-------------------------
-- PRIMITIVE DATA
-- These first few pattern matches are the most straightforward cases.
-- The expressions here are just basic data, so we evaluate them as such.
evalLispExpr env val@(String _) = return val -- bind val to ANY lispval that is a string (i.e. atom, string)
evalLispExpr env val@(Number _) = return val -- same but for number
evalLispExpr env val@(Bool _) = return val
evalLispExpr env (Atom id) = getVar env id -- look up variable
evalLispExpr env (List [Atom "quote", val]) = return val -- match for quoted expr (i.e. straight data)
--------------------------
-- CONDITIONALS 
evalLispExpr env (List [Atom "if", pred, conseq, alt]) =
    do  
        result <- evalLispExpr env pred
        case result of -- (note that any value other than #f is True)
            Bool False -> evalLispExpr env alt
            otherwise -> evalLispExpr env conseq
--------------------------
-- VARIABLE ASSIGNMENT
evalLispExpr env (List [Atom "set!", Atom var, form]) =
    evalLispExpr env form >>= setVar env var -- (evalLisExpr env form) = val, pipe into setVar

evalLispExpr env (List [Atom "define", Atom var, form]) =
    evalLispExpr env form >>= defineVar env var

--------------------------
-- FUNCTION APPLICATION
-- note that we bind because we lift our values into ThrowsError monad
evalLispExpr env (List (Atom func: args)) = mapM (evalLispExpr env) args >>= liftThrows . applyFunc func  -- [func, arg1, arg2,...]
evalLispExpr env badForm = Control.Monad.Error.throwError $ BadSpecialForm "Unrecognized special form" badForm


-- result is EITHER function applied to arguments, or LispError
-- i.e. result is an Either value
-- function is an operator stored in dictionary called primitives
applyFunc :: String -> [LispVal] -> ThrowsError LispVal
applyFunc func args = maybe (Control.Monad.Error.throwError $ NotFunction "Unrecognized primitive function args" func) 
                        ($ args) 
                        (lookup func primitives)


-- dictionary of primitive operations
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)), -- numeric ops
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)), -- comparison ops for num
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)), -- comparison ops for bools
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)), -- comparison for str
              ("string?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car), -- LISt Processing functions
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)] 


-- generic boolBinop function that is PARAMETERIZED by unpacker func
-- i.e. the unpacker converts LispVals into native Haskell types
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                                then Control.Monad.Error.throwError $ NumArgs 2 args
                            else do -- note that either arg may throw TypeMismatch
                                    left <- unpacker $ args !! 0
                                    right <- unpacker $ args !! 1
                                    return $ Bool $ left `op` right
-- amazing
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

-- convert string-type lispval to haskell string
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = Control.Monad.Error.throwError $ TypeMismatch "string" notString 

-- convert boolean lispval to haskell bool
unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = Control.Monad.Error.throwError $ TypeMismatch "boolean" notBool

-- map an operator to code
-- (map unpackNum args) goes from [LispVal] to [Integer] (convert args to nums)
-- foldl op (map unpackNum args) goes from [Integer] to Integer (evalLispExpr nums)
-- Number $ ... puts result into a LispVal
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
-- error case when there is only 1 arg
numericBinop op singleVal@[_] = Control.Monad.Error.throwError (NumArgs 2 singleVal)
numericBinop op args = (mapM unpackNum args) >>= (return . Number . foldl1 op)


-- unpackNum converts numeric LispVal to Integer
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String s) = let parsed = reads s in
                        if null parsed 
                            then Control.Monad.Error.throwError $ TypeMismatch "number" $ String s
                        else return $ fst $ parsed !! 0

unpackNum (List [n]) = unpackNum n -- one element list only
unpackNum notNum = Control.Monad.Error.throwError $ TypeMismatch "number" notNum

-- list functions

-- the reason these funcs input [LispVal] is to handle NumArgs exception
-- i.e. they SHOULD take in 1 input (a List) but if they take in more,
-- then the lisp code is wrong
-- i.e. if we inputted LispVal, then it would never throw a NumArgs exception
-- because we would just take in the first argument and ignore the extra (wrong) ones

-- car list returns list!!0
-- (car (a b c)) = a
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = Control.Monad.Error.throwError $ TypeMismatch "pear" badArg
car badArgList = Control.Monad.Error.throwError $ NumArgs 1 badArgList

-- cdr returns tail list
-- (cdr (a b c)) = (b c)
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs -- note that [x] is pattern matched as x : [] so (cdr a) = Nil
cdr [DottedList (_:xs) tail] = return $ DottedList xs tail 
cdr [DottedList [xs] x] = return x -- (cdr (a . b)) = b
cdr [badArg] = Control.Monad.Error.throwError $ TypeMismatch "pear" badArg
cdr badArgList = Control.Monad.Error.throwError $ NumArgs 1 badArgList

-- cons combines lists
-- @TODO (cons 2 '(3 . 4))????
cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List $ [x] -- (cons x Nil) = (x) i.e. (x . Nil)
cons [x, List xs] = return $ List $ [x] ++ xs -- (cons a (b c)) = (a b c)
cons [x, DottedList xs tail] = return $ DottedList ([x] ++ xs) tail -- (cons a (b . c)) = (a b . c)
cons [x, y] = return $ DottedList [x] y -- (cons a b) = (a . b), cons isn't nil terminated by default
cons badArgList = Control.Monad.Error.throwError $ NumArgs 2 badArgList

-- equiv recognizes things as equal if the values are equivalent

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool (arg1 == arg2)
eqv [(Number arg1), (Number arg2)] = return $ Bool (arg1 == arg2)
eqv [(String arg1), (String arg2)] = return $ Bool (arg1 == arg2)
eqv [(Atom arg1), (Atom arg2)] = return $ Bool (arg1 == arg2)
eqv [(DottedList x tailX), (DottedList y tailY)] = eqv [List (x ++ [tailX]), List (y ++ [tailY]) ]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && 
    (and $ map eqvPair $ zip arg1 arg2) -- and = foldr1 (&&)
        where 
            eqvPair (x1, x2) = case eqv [x1, x2] of
                                    Left err -> False
                                    Right (Bool val) -> val
eqv [_, _] = return $ Bool False -- type mismatch...
eqv badArgList = Control.Monad.Error.throwError $ NumArgs 2 badArgList

-------------------------------------------------------------------
-- Error Handling

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

-- make LispError an instance of Prelude's Error
-- can use built in error handling funcs
instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

-- type for functions that may throw a LispError or may return a value
-- this is a CURRIED TYPE CONSTRUCTOR!
-- i.e. if f :: Int->Int, then ThrowsError Integer => Either LispError Integer
type ThrowsError = Either LispError

-- all our errors turn into strings and get returned
-- catchError takes an Either and a func
    -- if the Either has an error, then it applies the func to the either
    -- in this case, we turn our errors into strings and put that string into an either
-- trapError :: ThrowsError a -> Either
trapError action = catchError action (return . show)

-- extract data from either monad
-- not to be used with Left, since that's a (programmer) error
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
------------------------------------------------------------------
-- Weak Typing
-- we want to compare data of different types
-- i.e. "2" and 2

-- algebraic data type for a generic unpacker
-- we can't store values of different types in list
-- but we can use existential types 
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

-- takes an Unpacker, determines if two LispVals are equal when it unpacks them
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do
        unpacked1 <- unpacker arg1 -- convert LispVals to Haskell values
        unpacked2 <- unpacker arg2
        return (unpacked1 == unpacked2) `catchError` (const $ return False)

-- (equal? "2" 2) = #t
-- objects are equal? if they print the same

-- @TODO why doesn't this work lol
-- the unpackers are throwing TypeErrors...
-- idk, moving on and coming back to this

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = 
    do  -- make hetero list of funcs, then mapM unpack them to get vals, then OR them to fold the 2 elem list 
        primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
            [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
        eqvEquals <- eqv [arg1, arg2]
        return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)

equal badArgList = Control.Monad.Error.throwError $ NumArgs 2 badArgList

----------------------------------------------------------------------------
-- Variables and Assignment
-- 2 ways to change environment:
-- (set! var value) updates variable
-- (define var value) creates new variable

-- empty environment, i.e. no variables assigned
nullEnv :: IO Env
nullEnv = newIORef []

-- we need to use 2 monads simultaneously! Error and Env 
-- its ok, monad transformers are here to help
-- they are like "super monads" and combine monads together

-- equivalent of "lift" for monad transformers
-- i.e. lift first-level monad into second-level monad
-- actually wild
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = Control.Monad.Trans.Error.throwError err
liftThrows (Right val) = return val

-- helper func to run the action in the supermonad IOThrowsError 
-- (trapError action) takes potential errors to string reprs
-- runErrorT (trapError action) does the computation
-- return . extractValue extracts value and puts it in IO monad
runIOThrows :: IOThrowsError String -> IO String 
runIOThrows action = runErrorT (trapError action) >>= return . extractValue


-- check if variable is in scope already
isBound :: Env -> String -> IO Bool
-- (readIORef envRef) gets the IO env value
-- then pass this value to lookup to get what we want
-- "maybe False (const True)" converts the Maybe from lookup to a Bool
-- then return to lift into IO monad, even though we just want a true/false value
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

-- get value of bound variable
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = 
    do 
        env <- liftIO $ readIORef envRef -- get env from IORef and lift into IOThrowsError supermonad
        maybe (Control.Monad.Trans.Error.throwError $ UnboundVar "Unbound variable" var)
            (liftIO . readIORef) -- if Just val, then snipe val and put it in monad 
            (lookup var env) -- lookup in dict, returns Maybe 

-- set value of bound variable
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val =
    do
        env <- liftIO $ readIORef envRef -- get env from IORef
        maybe (Control.Monad.Trans.Error.throwError $ UnboundVar "Unbound variable" var)
            (liftIO . (flip writeIORef val)) -- if val exists, then write new value 
            (lookup var env) -- lookup in dict, returns Maybe 
        return val

-- sets value if defined, or creates it if not
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value =
    do
        alreadyDefined <- liftIO $ isBound envRef var
        if alreadyDefined
            then setVar envRef var value >> return value
            else liftIO $ do
                valueRef <- newIORef value
                env <- readIORef envRef 
                writeIORef envRef ((var, valueRef) : env)
                return value

-- bind lots of vars at the same time (i.e. in function calls)
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = 
    -- get env,           bind new vars,    put it in new IORef box
    readIORef envRef >>= extendEnv bindings >>= newIORef
    where -- extendEnv maps addBinding to list of desired bindings, and then appends env to end of list
        extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do -- takes in (name, val), puts it in new IORef box, returns (var, box)  
                                    ref <- newIORef value
                                    return (var, ref)















