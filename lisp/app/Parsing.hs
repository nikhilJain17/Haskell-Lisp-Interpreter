module Parsing (
Parser,
item,
sat,
char,
string,
many,
sepby,
chainl,
chainl1,
space,
symb,
apply,
(+++),
token,
isSpace,
--isDigit
newline_search
) where
-- http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf

-- define a parameterized parser type
-- it's a func that takes a str and returns list of results
-- nonempty string indicates success
newtype Parser a = Parser (String -> [(a, String)])

-- simple parser that parses each character
item :: Parser Char
item = Parser (\cs -> case cs of 
                        "" -> []
                        (c:cs) -> [(c, cs)])


-- "anti monad" which drops a parser from context to just function
parse :: Parser a -> (String -> [(a, String)])
parse (Parser p) = p

instance Functor Parser where
    -- apply func to output of parser
    fmap func parser = Parser (\cs -> case parse parser cs of
                                [] -> []
                                [(a, cs')] -> [(func a, cs')]) 

instance Applicative Parser where
    -- pure lifts something into Parser context
    pure a = Parser (\cs -> [(a, cs)])
    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = Parser (\cs -> case parse pg cs of
                        [] -> []
                        [(a, cs')] -> parse (fmap a px) cs') 

instance Monad Parser where
    -- lift into parser context
    return a = Parser (\cs -> [(a, cs)])
    -- take string cs, apply parser p to input string
    -- look at list of results, apply parser to each result string
    -- concat list of lists
    p >>= f = Parser (\cs -> concat [parse (f a) cs' 
        | (a, cs') <- parse p cs])

-- monads can have more advanced properties too!
-- note that MonadPlus subset MonadZero subset Monad
class Monad m => MonadZero m where
  zero :: m a

class MonadZero m => MonadPlus m where
    (++) :: m a -> m a -> m a     

-- "zero" or null parser... why though?
instance MonadZero Parser where
    zero = Parser (\cs -> [])

-- -- can combine parsers! apply both parsers p and q
instance MonadPlus Parser where
    p ++ q = Parser (\cs -> (parse p cs) Prelude.++ (parse q cs))

-- define deterministic ++, where we just take first result
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p Parsing.++ q) cs of 
                        [] -> []
                        (x:xs) -> [x])

-- parser that conditionally consumes characters if they satisfy a predicate
sat :: (Char -> Bool) -> Parser Char
sat predicate = do {c <- item; if predicate c then return c else zero}

-- parser for character equality
char :: Char -> Parser Char
char c = sat (c ==)

-- Recursive parser combinators

-- 1. parse for equality to a specific string
string :: String -> Parser String
string "" = return "" -- remember that return lifts stuff into parser context
-- check first char matches, then recursively apply string to rest of str 
string (c:cs) = do {char c; string cs; return (c:cs)}

-- 2. parse repeated applications of parser p
-- many can do 0 or more applications of p
many :: Parser a -> Parser [a] 
many p = many1 p +++ return []

-- many1 does 1 or more applications of p
many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

-- 3. parse repeated applications of parser p, throw away results of sep parser
sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do a <- p
                    as <- many (do {sep; p})
                    return (a:as)

-- 4. parse repeated applications of parser p, 
-- separated by applications of parser op
-- whose result value is an OPERATOR that is assoc to left
-- and is used to combine results from the p parsers?
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
                where 
                    rest a = (do f <- op
                                 b <- p
                                 rest (f a b))
                             +++ return a


-- no need to have lexical phase (string -> seq of tokens)
-- we can just use our parsers!

-- 1. parse string of spaces tabs newlines (check if stuff equals whitespace)
space :: Parser String
space = many (sat isSpace)

isSpace :: Char -> Bool
isSpace s
    | s == ' ' = True
    | s == '\t' = True
    | s == '\n' = True
    | otherwise = False

isDigit :: Char -> Bool
isDigit d = elem d ['0','1','2','3','4','5','6','7','8','9']

-- 2. parse token using parser p, throw away trailing space
token :: Parser a -> Parser a
token p = do {a <- p; space; return a}

-- 3. parse symbolic token (string checks for equality to str)
symb :: String -> Parser String
symb cs = token (string cs)

-- 4. apply parser p, throw away leading space
apply :: Parser a -> String -> [(a, String)]
apply p = parse (do {space; p})

-- 5. consume chars until first occurence of char
-- char_search :: char -> Parser b -> Parser [a]
newline_search = do {a <- item; result <- char '\n'; return result}



