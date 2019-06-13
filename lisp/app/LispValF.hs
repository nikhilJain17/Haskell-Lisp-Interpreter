module LispValF where

import Control.Monad.Error
import Datatypes


-- let d = ListF[ NumberF 4 ]
-- d :: LispValF (LispValF Integer)

-- let e = In (NumberF 4)
-- e :: Fix LispValF

-- let f = In (ListF [In (NumberF 4)])
-- f :: 

-- https://blog.sumtypeofway.com/recursion-schemes-part-41-2-better-living-through-base-functors/

-- f algebra stuff
data LispValF f = AtomF String 
	| ListF [f]
	| DottedListF [f] f
	| NumberF Integer
	| StringF String
	| BoolF Bool
	| PrimitiveFuncF ([f] -> ThrowsError f)
	-- | Func {params :: [String], vararg :: (Maybe String), body :: [f], closure :: Env}

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

