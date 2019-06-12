module LispValF where

import Control.Monad.Error
import Datatypes


-- f algebra stuff
data LispValF f = Atom String 
	| List [f]
	| DottedList [f] f
	| Number Integer
	| String String
	| Bool Bool
	| PrimitiveFunc ([f] -> ThrowsError f)
	-- | Func {params :: [String], vararg :: (Maybe String), body :: [f], closure :: Env}

newtype Fix f = Fx (f (Fix f))

type LispValgebra = Fix LispValF

