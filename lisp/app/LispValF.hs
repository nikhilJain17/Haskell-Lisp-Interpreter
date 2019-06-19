{-# LANGUAGE TypeFamilies #-}
module LispValF where

import Control.Monad.Error
import Datatypes
import Data.Functor.Foldable
import Data.Functor.Foldable
-- import recursion-schemes-5.1.3.src.Data.Functor.Base

-- let d = ListF[ NumberF 4 ]
-- d :: LispValF (LispValF Integer)

-- let e = In (NumberF 4)
-- e :: Fix LispValF

-- let f = In (ListF [In (NumberF 4)])
-- f :: 

-- https://blog.sumtypeofway.com/recursion-schemes-part-41-2-better-living-through-base-functors/

-- recursive lispval data type
data LispValBasic = AtomBasic String
	| ListBasic [LispValBasic]
	| DottedListBasic [LispValBasic] LispValBasic
	| NumberBasic Integer
	| StringBasic String
	| BoolBasic Bool
	--		      op, i.e. "+"   argtypes   returntypes
	| PrimitiveFuncBasic String [LispType] LispType -- don't store an actual haskell func, just store description of it

-- parameterized lispval data type
data LispValF f = AtomF String 
	| ListF [f]
	| DottedListF [f] f
	| NumberF Integer
	| StringF String
	| BoolF Bool
	| Error String -- this is if a func throws an error, to keep things easy (i.e. error is a lispvalf)
	| PrimitiveFuncF ([f] -> LispValF f)
	-- | Func {params :: [String], vararg :: (Maybe String), body :: [f], closure :: Env}

-- @todo type family stuff...
type instance Base LispValBasic = LispValF


-- @TODO fix this stuff later for functions probably

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
------------------------------------------------
-- Type Checking
------------------------------------------------

data LispType = NumType | StrType | BoolType | FnType [LispType] LispType

instance Show (LispType) where show = showType
showType :: LispType -> String
showType (NumType) = "Number"
showType (StrType) = "String"
showType (BoolType) = "Boolean"
showType (FnType args result) = "Function: <args " ++ unwordsType args ++ "> <return " ++ showType result

unwordsType :: [LispType] -> String 
unwordsType = unwords . map showType


-- @TODO 
-- first pass is to try to manually do the recursion
-- then the move is to abstract away recursion with recursion schemes
getType :: LispValBasic -> LispType
-- base cases are straightforward
getType (NumberBasic n) = NumType
getType (StringBasic s) = StrType
getType (BoolBasic b) = BoolType
getType (PrimitiveFuncBasic name argtypes returntypes) = FnType argtypes returntypes



















