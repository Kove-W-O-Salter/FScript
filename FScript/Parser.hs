
--
-- @File          : FScript/Parser.hs
-- @Module        : FScript.Parser
-- @Project       : FScript
-- @Description   : Parser of syntax
-- @Maintainer    : Kove W. Ochre-Salter
-- @Copyright     : (c) Kove W. Ochre-Salter, 2018
-- @Creation Date : 18'th June 2018
--

module FScript.Parser where

--
-- @Reason        : To parse the syntax and generate an expression
--                  tree we must first have access to the expression
--                  type, Expr.
-- @Creation Date : 18'th June 2018.
--

import FScript.Expr

--
-- @Reason        : To parse synatx we need a parser
--                  and Parsec is my favourite.
-- @Creation Date : 18'th June 2018.
--

import Text.Parsec

--
-- @Reason        : The Identity Monad is just for abstracting of ParsecT.
-- @Creation Date : 18'th June 2018.
--

import Control.Monad.Identity

--
-- @Decription    : Abstracting over Parsec's default Parsing
--                  type since it's not easy to read.
-- @Creation Date : 18'th June 2018.
--

type Parser a = ParsecT String () Identity a

--
-- @Description   : Parse an expression.
-- @Creation Date : 18'th June 2018.
--
fScript        :: String -> Either ParseError Expr
fScript         = parse expression "FScript"

--
-- @Description   : Parse an expression.
-- @Creation Date : 18'th June 2018.
--
expression :: Parser Expr
expression  = (try constant) <|> (try application) <|> (try declaration) <|> parenthesis

--
-- @Description   : Match a constant.
-- @Creation Date : 18'th June 2018.
--

constant :: Parser Expr
constant  = do i <- integer
               return $ Con i

--
-- @Description   : Match an integer value.
-- @Creation Date : 18'th June 2018.
--

integer :: Parser Int
integer  = do is <- digits
--              notFollowedBy letter
              return $ read is

--
-- @Description   : Match one or more digits.
-- @Creation Date : 18'th June 2018.
--

digits :: Parser String
digits  = many1 digit

--
-- @Description   : Match an application.
-- @Creation Date : 18'th June 2018.
--

application :: Parser Expr
application  = (try idApplication) <|> opApplication

--
-- @Description   : Match the application of an identifier
--                  to zero or more arguments.
-- @Creation Date : 18'th June 2018.
--

idApplication :: Parser Expr
idApplication  = do i <- identifier
                    many space
                    as <- arguments
--                    notFollowedBy (letter <|> digit)
                    return $ Cal i as

--
-- @Description   : Match an identifier.
-- @Creation Date : 18'th June 2018.
--

identifier :: Parser String
identifier  = do l <- letter
                 dl <- many (letter <|> digit)
                 return $ l : dl

--
-- @Description   : Match zero or more expressions separated by semicolons.
-- @Creation Date : 18'th June 2018.
--

arguments :: Parser [Expr]
arguments  = do as <- sepBy expression (string "`")
--                notFollowedBy (letter <|> digit)
                return as

--
-- @Description   : Match the application of an operator to
--                  one or more arguments.
-- @Creation Date : 18'th June 2018.
--

opApplication :: Parser Expr
opApplication  = do char '['
                    o <- operator
                    char ']'
                    spaces
                    as <- arguments
--                    notFollowedBy (letter <|> digit)
                    return $ App o as

--
-- @Description   : Match an operator.
-- @Creation Date : 18'th June 2018.
--

operator :: Parser Oper
operator  = do o <- oneOf "+-*/=<>!"
               return $ readOp o

--
-- @Description   : Read an operator.
-- @Creation Date : 18'th June 2018.
--

readOp     :: Char -> Oper
readOp '+'  = Add
readOp '-'  = Sub
readOp '*'  = Mul
readOp '/'  = Div
readOp '='  = Equ
readOp '>'  = Gtr
readOp '<'  = Ltn
readOp '!'  = Not

--
-- @Description   : Match the declaration of an identifier.
-- @Creation Date : 18'th June 2018.
--

declaration :: Parser Expr
declaration  = do char '['
                  i <- identifier
                  many1 space
                  as <- sepBy identifier (char '`')
                  char ']'
                  spaces
                  string "=>"
                  spaces
                  b <- expression
                  many1 space
                  e <- expression
--                  notFollowedBy (letter <|> digit)
                  return $ Dec i as b e

--
-- @Description   : Match an expression between parenthesis.
-- @Creation Date : 18'th June 2018.
--

parenthesis :: Parser Expr
parenthesis  = between (string "(") (string ")") expression
