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
symbol,
oneOf,
noneOf,
space',
spaces,
parse,
try,
endBy,
skip,
skipMany,
plusplus,
chainr
--isDigit
-- newline_search
) 
where

import Data.Char
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

-- can combine parsers! apply both parsers p and q
instance MonadPlus Parser where
    p ++ q = Parser (\cs -> (parse p cs) Prelude.++ (parse q cs))

-- define deterministic ++, where we just take first success
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
                                 rest (f a b)) +++ return a


-- no need to have lexical phase (string -> seq of tokens)
-- we can just use our parsers!

-- 1. parse string of spaces tabs newlines (check if stuff equals whitespace)
space :: Parser String
space = many (sat isSpace)

-- isSpace :: Char -> Bool
-- isSpace s
--     | s == ' ' = True
--     | s == '\t' = True
--     | s == '\n' = True
--     | otherwise = False

isDigit :: Char -> Bool
isDigit d = elem d ['0','1','2','3','4','5','6','7','8','9']

-- 2. parse token using parser p, throw away trailing space
token :: Parser a -> Parser a
token p = do {a <- p; space; return a}

-- 3. parse symbolic token (string checks for equality to str)
symb :: String -> Parser String
symb cs = token (string cs)

-- could probably delete these
token2 p = do {item; a <- p; return a}
symb2 cs = token2 (string cs)

-- 4. apply parser p, throw away leading space
apply :: Parser a -> String -> [(a, String)]
apply p = parse (do {space; p})

-- 5. consume chars until first occurence of char
-- char_search :: char -> Parser b -> Parser [a]
newline_search = do {a <- item; result <- symb "\n"; return result}

-----------------------------------------------------------
-- part 2 of parser

type SourceName = String

-- Parse repeated applications of parser p, 
-- separated by applications of parser op
-- whose result value is an OPERATOR that is assoc to right
-- and is used to combine results from the p parsers
-- 'a' (the value not the type var) is used to return a generic failure
chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op a = (chainr1 p op) +++ return a

-- Def: Right chaining of 1 or more terms.
-- Notes:   1 - (2 + (4 - 2))
--          here, left operands are allowed to be integers
--          right operands are either expressions or integers 
-- Works:   Get left operand a, get op, recurse on right subexpr, combine results 
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = do {a <- p; rest a} -- get left integer, then parse right expr
            where 
                rest a = do f <- op -- get op
                            b <- chainr1 p op -- recurse to get right operand
                            return (f a b) -- eval the expr and keep going
                            +++ return a  -- base case: no more terms left, we are done

-- chainr1 p op = do {a <- p; rest a}
  -- where rest x = do f <- op
  --                   y <- chainr1 p op
  --                   return (f x y) 
  --                   +++ return x


-- chainr1 p op = do {a <- p; rest a}
--       where rest a = do f <- op
--                  b <- chainr1 p op
--                  return (f a b) +++ return a 



------------------------------------------------------------
-- Testing for chainl, chainr
-- > apply (chainl (string "a") (plusplus) ("default")) 
-- "a~a~a~a~ab"[("a a a a a","b")]

-- plusplus is an op parser which parses the '~' operator
plusplus :: Parser (String -> String -> String)
plusplus = Parser (\(c:cs) -> if c == '~' then [((+-+), cs)]
                                else [])

-- (+-+) is an op that swap/appends two strings with a space betw. them
infixr 4 +-+
(+-+) :: String -> String -> String
"" +-+ s = s
s +-+ "" = s
s +-+ t | last s == ' ' = t Prelude.++ s
        | head t == ' ' = t Prelude.++ s
s +-+ t = t Prelude.++ " " Prelude.++ s
------------------------------------------------------------

-- Def:     Parse out any single symbol character (defined in Data.Char) 
-- Works:   Just use sat with the appropriate predicate
-- Ex:      apply symbol "=3" = [('=', "3")]
symbol :: Parser Char
symbol = sat Data.Char.isSymbol

-- Def:     Succeeds if current char is in input string
-- Works:   Use char parsers for all chars in string in parallel, and pick result with +++.
-- Ex:      apply (oneOf "aeiou") "if" = [('i',"f")]
--          apply (many (oneOf "aeiou")) "ouch" = [("ou","ch")]
oneOf :: String -> Parser Char
oneOf str = foldr (+++) zero [char c | c <- str]
-- note that the 'zero' element is defined in MonadZero
-- oneOf uses the "or" operator for lists-return any success since there will only be 1

noneOf :: String -> Parser Char
noneOf str = Parser (\(c:cs) -> case (c `elem` str) of
                           True -> []
                           False -> [(c, cs)])
-- Note: A possible implementation of noneOf using similar ideas as oneOf is possible.
-- noneOf str = foldr (&&&) item [sat (c /=) | c <- str]
-- Here, noneOf uses the "and" operator for lists-return any failure, since there will only be 1
-- see defn of "&&&" below
-- we only want p &&& q to return a result if both p and q return results
(&&&) :: Parser a -> Parser a -> Parser a
--             Unfortunately, this bool condition is broken
p &&& q = Parser (\cs -> if (length (parse p cs) /= 0 ||
                             length (parse q cs) /= 0) 
                        then []
                        else parse (p +++ q) cs)


-- Def: apply parser p, ignore result 
skip :: Parser a -> Parser ()
skip p = do {p; return ()}

-- Def: apply parser p zero or more times, ignore result
skipMany :: Parser a -> Parser ()
skipMany p = (skipMany1 p) +++ return ()

-- Def: apply parser p one or more times, ignore result
skipMany1 :: Parser a -> Parser ()
skipMany1 p = do {result <- (skip p); (skipMany p)}

-- Def: Parse a single whitespace character 
-- Note: space is already defined!!!
space' :: Parser Char
space' = sat isSpace

-- Def: Parse zero or more whitespace characters
spaces :: Parser String
spaces = many (sat isSpace)

-- Def: Parse input, but if there's an error, don't consume any characters
-- @TODO what's the result value for the error case???
-- bottom = bottom
try :: Parser a -> Parser a
try p = Parser (\cs -> if length (parse p cs) == 0 --error!
                            then [(undefined, cs)] -- what to return...
                       else parse p cs
                )

        --     emptyParser = Parser (\cs -> [(cs)])

-- endBy p sep parses zero or more occurrences of p, separated and ended by sep. Returns a list of values returned by p.
endBy :: Parser a -> Parser b -> Parser [a]
endBy p sep = many (do {a <- p; sep; return a})

-- parse' p filePath input runs a parser p over Identity without user state
-- note there is already a func parse
parse' :: Parser a -> SourceName -> String -> Either () a
parse' p _ s = undefined 























