
--
-- @File          : FScript/Expr.hs
-- @Module        : FScript.Expr
-- @Project       : FScript
-- @Description   : Semantic representation of expressions
-- @Maintainer    : Kove W. Ochre-Salter
-- @Copyright     : (c) Kove W. Ochre-Salter, 2018
-- @Creation Date : 18'th June 2018
--

module FScript.Expr where

--
-- @Reason        : We need to use the Iden type wich
--                  is just an alias for String.
-- @Creation Date : 18'th June 2018.
--

import FScript.Stack (Iden)

--
-- @Description   : The semantic representation of expressions.
--                  An expression may be:
--                    (1), a constant integer;
--                    (2), the application of an operator;
--                    (3), the declaration of a function;
--                    (4), the application of a function;
--                    (5), conditional execution;
--                    (5), the raising of an exception.
-- @Creation Date : 18'th June 2018.
--

data Expr = Con Int
          | App Oper [Expr]
          | Dec Iden [Iden] Expr Expr
          | Cal Iden [Expr]
          | Err String
          deriving (Show,Read)

--
-- @Description   : The primitive, builtin, functions; wich,
--                  are the basis of most arithmetic expressions.
--                  An operator is one of the following:
--                    (1), Add (Addition);
--                    (2), Sub (Subtraction);
--                    (3), Mul (Multiplication);
--                    (4), Div (Division);
-- @Creation Date : 18'th June 2018.
--

data Oper = Add
          | Sub
          | Mul
          | Div
          | Equ
          | Gtr
          | Ltn
          | Not
          | Cnd
          deriving (Show,Read)
