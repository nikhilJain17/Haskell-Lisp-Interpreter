{-# LANGUAGE TypeFamilies #-}
module LispValF where
-- putting the F in F-algebra
import Control.Monad.Error
import Datatypes
import Data.Functor.Foldable
import Data.Functor.Foldable
import Data.List
import Parsing
import Data.Char

-- recursive lispval data type
data LispValBasic = AtomBasic String
	| ListBasic [LispValBasic]
	| DottedListBasic [LispValBasic] LispValBasic
	| NumberBasic Integer
	| StringBasic String
	| BoolBasic Bool
	--		      op, i.e. "+"   argtypes   returntypes
	| PrimitiveFuncBasic {name :: String, argtypes :: [LispType], returntype :: LispType} -- don't store an actual haskell func, just store description of it
	| ErrBasic String -- hmm...

-- parameterized lispval data type
data LispValF f = AtomF String 
	| ListF [f]
	| DottedListF [f] f
	| NumberF Integer
	| StringF String
	| BoolF Bool
	| ErrF String -- this is if a func throws an error, to keep things easy (i.e. error is a lispvalf)
	| PrimitiveFuncF ([f] -> LispValF f)
	-- | Func {params :: [String], vararg :: (Maybe String), body :: [f], closure :: Env}

-- @todo type family stuff...
type instance Base LispValBasic = LispValF

-- functors work on polymorphic types
-- i.e. f :: * -> *
instance Functor LispValF where
	fmap f (ListF l) = ListF (fmap f l)
	fmap f (DottedListF head tail) = DottedListF (fmap f head) (f tail)
	-- for these 3, we have a rigid type (i.e. n :: Integer)
	-- but f :: * -> *, not Integer -> * and so on
	-- so we don't call f on value
	fmap f (NumberF n) = NumberF n
	fmap f (StringF s) = StringF s
	fmap f (BoolF b) = BoolF b
	-- fmap f (PrimitiveFuncF func) = PrimitiveFuncF (f . func) 

-- project maps the recursive data type to the parameterized data type
instance Recursive LispValBasic where
	project (ListBasic l) = ListF l
	project (DottedListBasic head tail) = DottedListF head tail
	project (NumberBasic n) = NumberF n
	project (StringBasic s) = StringF s
	project (BoolBasic b) = BoolF b
	-- project (PrimitiveFuncBasic func) = PrimitiveFuncF func 


-- embed maps the parameterized data type to the recursive data type
instance Corecursive LispValBasic where
	embed (ListF l) = ListBasic l
	embed (DottedListF head tail) = DottedListBasic head tail
	embed (NumberF n) = NumberBasic n
	embed (StringF s) = StringBasic s
	embed (BoolF b) = BoolBasic b
	-- embed (PrimitiveFuncF func) = PrimitiveFuncBasic func

instance Show (LispValF f) where show = showF

showF :: LispValF f -> String
showF (AtomF str) = str
showF (NumberF i) = show i
showF (StringF str) = str
showF (BoolF bool) = show bool
-- showF (PrimitiveFuncF _) = "<primitive func>"
showF (ListF l) = "<list> length: " ++ show (length l) -- unwordsListF l
showF (DottedListF head tail) = "<dotted list>"--unwordsListF head ++ unwordsListF [tail]

instance Show (LispValBasic) where show = showBasic

showBasic :: LispValBasic -> String
showBasic (NumberBasic n) = show n
showBasic (StringBasic s) = s
showBasic (BoolBasic b) = show b
showBasic (ErrBasic e) = e
showBasic (AtomBasic a) = a
showBasic (PrimitiveFuncBasic {name=name, argtypes=argtypes, returntype=returntype}) = name
------------------------------------------------
-- Type Checking
------------------------------------------------

data LispType = NumType | StrType | BoolType | FnType [LispType] LispType

instance Show (LispType) where show = showType
showType :: LispType -> String
showType (NumType) = "Num"
showType (StrType) = "Str"
showType (BoolType) = "Bool"
showType (FnType args result) = "<Func: (" ++ unwordsType args ++ ") -> " ++ showType result ++ ">"

unwordsType :: [LispType] -> String 
unwordsType = (intercalate ", ") . (map showType)



-- @TODO 
-- first pass is to try to manually do the recursion
-- then the move is to abstract away recursion with recursion schemes
getType :: LispValBasic -> LispType
-- base cases are straightforward
getType (NumberBasic n) = NumType
getType (StringBasic s) = StrType
getType (BoolBasic b) = BoolType
getType (PrimitiveFuncBasic name argtypes returntypes) = FnType argtypes returntypes


-- primitive funcs
primitiveFuncs = 
			[(PrimitiveFuncBasic "add" [NumType, NumType] NumType),
			(PrimitiveFuncBasic "sub" [NumType, NumType] NumType),
			(PrimitiveFuncBasic "concat" [StrType, StrType] StrType),
			(PrimitiveFuncBasic "equal" [NumType, NumType] BoolType)]

--------------------------------------------------------------------------
-- Some parsers to take the user's input and turn it into a LispValBasic

parseStr :: Parser LispValBasic
parseStr = do
				char '"'
				str <- many (noneOf "\"")
				char '"'
				return (StringBasic str)

parseNum :: Parser LispValBasic
parseNum = do
				num <- many (sat isDigit)
				return (NumberBasic (read num))

-- hmm... atoms...
parseAtomB :: Parser LispValBasic
parseAtomB = do
				first <- letter +++ symbol   -- first char in lispval can be letter or symbol
				rest <- many (letter +++ digit +++ symbol) -- other chars can also be num
				let atom = [first] Prelude.++ rest -- assemble the atom
				return $ case atom of           -- is it a true/false?  
					"#t" -> BoolBasic True           -- otherwise just return the val
					"#f" -> BoolBasic False          
					otherwise -> AtomBasic atom


parsePrimitiveFunc :: Parser LispValBasic
parsePrimitiveFunc = do
                        char '('
                        -- get function name
                        firstFnName <- letter
                        restFnName <- many (letter +++ digit)
                        char ')'
                        let fnName = [firstFnName] Prelude.++ restFnName
                        let match = [x | x <- primitiveFuncs, (name x) == fnName]
                        return $ case match of 
                        	[] -> ErrBasic "not primitive"
                        	otherwise -> match!!0




-- @todo something for functions
-- (name ("fn-signature" (arg-type arg-type...) (return-type)) (result-type) (body))
parseFunc :: Parser LispValBasic
parseFunc = do
				char '('
				-- get function name
				firstFnName <- letter
				restFnName <- many (letter +++ digit)
				-- get fn signature
				char '('
				-- @todo fill in rest here
				-- end of fn 
				char ')'
				let fnName = [firstFnName] Prelude.++ restFnName
				return (AtomBasic fnName)

parseLispBasicExpr :: Parser LispValBasic
parseLispBasicExpr = do
						parseStr
						+++ parseNum
						+++ parseAtomB
				















