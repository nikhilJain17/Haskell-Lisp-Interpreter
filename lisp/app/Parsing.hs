module Parsing (

) where

-- define a parameterized parser type
-- it's a func that takes a str and returns list of results
-- nonempty string indicates success
newtype Parse a = Parser (String -> [(a, String)])

-- simple parser that parses each character
item :: Parser Char
item = Parser (\cs -> case cs of 
						"" -> []
						(c:cs) -> [(c, cs)])


-- "anti monad" which drops a parser from context to just function
parse :: Parser p -> p 
parse (Parser p) = p

instance Monad Parser where
	-- lift into parser context
	return a = Parser (\cs -> [(a, cs)])
	-- take string, apply parser p to input string
	-- look at list of results, apply parser to each result string
	-- concat list of lists
	p >>= f = Parser (\cs -> concat [parse (f a) cs' 
		| (a, cs') <- parse p cs])

-- monads can have more advanced properties too!
-- note that MonadPlus subset MonadZero subset Monad

-- "zero" or null parser... why though?
instance MonadZero Parser where
	zero = Parser (\cs -> [])

-- can combine parsers! apply both parsers p and q
instance MonadPlus Parser where
	p ++ q = Parser (\cs -> parse p cs ++ parse q cs)

-- define deterministic ++, where we just take first result
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p ++ q) cs of 
						[] -> []
						(x:xs -> [x]))


-- parser that conditionally consumes characters if they satisfy a predicate
sat :: (Char -> Bool) -> Parser Char
sat predicate = do {c <- item; if predicate c then return c else zero}

















