module LispValF where

import Control.Monad.Error
import Datatypes
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
	| PrimitiveFuncBasic ([LispValBasic] -> ThrowsError LispValBasic)

-- recursion-schemes stuff

-- parameterized lispval data type
data LispValF f = AtomF String 
	| ListF [f]
	| DottedListF [f] f
	| NumberF Integer
	| StringF String
	| BoolF Bool
	| PrimitiveFuncF ([f] -> ThrowsError f)
	-- | Func {params :: [String], vararg :: (Maybe String), body :: [f], closure :: Env}

-- @todo type family stuff...
type instance Base LispValBasic = LispValF

-- project maps the recursive data type to the parameterized data type
instance Recursive LispValBasic where
	project (ListBasic l) = ListF l
	project (DottedListBasic head tail) = DottedListF head tail
	project (NumberBasic n) = NumberF n
	project (StringBasic s) = StringF s
	project (BoolBasic b) = BoolF b
	project (PrimitiveFuncBasic func) = PrimitiveFuncF func 


-- embed maps the parameterized data type to the recursive data type
instance Corecursive LispValBasic where
	embed ListF l = (ListBasic l)
	embed DottedListF head tail = (DottedListBasic head tail)
	-- here
	embed (NumberBasic n) = NumberF n
	embed (StringBasic s) = StringF s
	embed (BoolBasic b) = BoolF b
	embed (PrimitiveFuncBasic func) = PrimitiveFuncF func

instance Show (LispValF f) where show = showF

showF :: LispValF f -> String
showF (AtomF str) = str
showF (NumberF i) = show i
showF (StringF str) = str
showF (BoolF bool) = show bool
showF (PrimitiveFuncF _) = "<primitive func>"
showF (ListF l) = "<list> length: " ++ show (length l) -- unwordsListF l
showF (DottedListF head tail) = "<dotted list>"--unwordsListF head ++ unwordsListF [tail]

-- unwordsListF = unwords . map showF 

newtype Fix f = In (f (Fix f))

type LispValgebra = Fix LispValF

