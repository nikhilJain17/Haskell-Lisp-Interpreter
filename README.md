# Haskell-Lisp-Interpreter
This project is a Lisp interpreter written in Haskell, based on [this book](https://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf). It includes a general-purpose monadic parsing library based on [this paper](http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf) which implements a subset of the Parsec library. The Lisp interpreter is built on top of these parsing functions.

## Parsing Library
### Parser Combinators
The parsing library in `Parsing.hs` features a number of simple and useful combinators that are used to make more powerful parsers. For example:

`sat :: (Char -> Bool) -> Parser Char` consumes characters if they satisfy a predicate. 

We define `char :: Char -> Parser Char` as `char c = sat (c ==)` as a parser to match against a specific letter. 

We use `char` to define `string :: String -> Parser String` as `string (c:cs) = do {char c; string cs; return (c:cs)}` to recursively match against a specific string.  

### Monadic Parsing
The parsing library also takes advantage of monads to chain together parsers easily. For example:

We define a utility function `many :: Parser a -> Parser [a]` that parses repeated applications of a parser `p`. `many` relies on a helper function `many1 :: Parser a -> Parser [a]` which is defined as `many1 p = do {a <- p; as <- manyp; return (a:as)}`. `many` is the main way we chain together parsers.

There are many examples of this in `Parsing.hs`, namely `sepby`, `chainr` and `chainl`.

## Lisp Features
This interpreter supports many features of the Lisp language.
### Number functions
![alt text](https://raw.githubusercontent.com/nikhilJain17/Haskell-Lisp-Interpreter/master/lisp/img/primitive_num.png "")

![alt text](https://raw.githubusercontent.com/nikhilJain17/Haskell-Lisp-Interpreter/master/lisp/img/primitive_num2.png "")

### String functions
![alt text](https://raw.githubusercontent.com/nikhilJain17/Haskell-Lisp-Interpreter/master/lisp/img/primitive_str.png "")

### List primitives
![alt text](https://raw.githubusercontent.com/nikhilJain17/Haskell-Lisp-Interpreter/master/lisp/img/primitive_list.png "")

### Boolean functions
![alt text](https://raw.githubusercontent.com/nikhilJain17/Haskell-Lisp-Interpreter/master/lisp/img/conditional.png "")

### Quote notation
![alt text](https://raw.githubusercontent.com/nikhilJain17/Haskell-Lisp-Interpreter/master/lisp/img/quote.png "")

### Error checking
![alt text](https://raw.githubusercontent.com/nikhilJain17/Haskell-Lisp-Interpreter/master/lisp/img/error.png "")

### Defining variables
![alt text](https://raw.githubusercontent.com/nikhilJain17/Haskell-Lisp-Interpreter/master/lisp/img/variables.png "")

### Defining functions
![alt text](https://raw.githubusercontent.com/nikhilJain17/Haskell-Lisp-Interpreter/master/lisp/img/functions.png "")
