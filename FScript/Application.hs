
--
-- @File          : FScript/Application.hs
-- @Module        : FScript.Application
-- @Project       : FScript
-- @Description   : Application of operators and identifier
-- @Maintainer    : Kove W. Ochre-Salter
-- @Copyright     : (c) Kove W. Ochre-Salter, 2018
-- @Creation Date : 18'th June 2018
--

module FScript.Application where

--
-- @Reason        : To call an identifier I must first
--                  build it.
-- @Creation Date : 18'th June 2018.
--

import FScript.Builder

--
-- @Reason        : To call an identifier I must first
--                  harvest it's Unit.
-- @Creation Date :
--

import FScript.Stack

--
-- @Reason        : When an identifier, or operator, is applied to an
--                  invalid number of arguments we must have a way of
--                  throwing an error.
-- @Creation Date : 18'th June 2018.
--

import FScript.Environ

--
-- @Reason        : To apply an identifier, or operator, to an expression
--                  we need access to the expression type; Expr.
-- @Creation Date :
--

import FScript.Expr

--
-- @Description   : Apply an operator to a list of - reduced - expressions.
-- @Creation Date : 18'th June 2018.
--

applyOper :: Oper -> [Expr] -> Environ Expr
applyOper o es = applyOper' o (map (\(Con n) -> n) es) >>= \n -> return $ Con n

--
-- @Description   : Apply an operator to a list of integers.
-- @Creation Date : 18'th June 2018.
--

applyOper' :: Oper -> [Int] -> Environ Int
applyOper' Add [x,y] = return $ x + y
applyOper' Add _     = Failure $ "Add, has been applied too an invalid number of arguments."
applyOper' Sub [x,y] = return $ x - y
applyOper' Sub _     = Failure $ "Sub, has been applied too an invalid number of arguments."
applyOper' Mul [x,y] = return $ x * y
applyOper' Mul _     = Failure $ "Mul, has been applied too an invalid number of arguments."
applyOper' Div [_,0] = Failure $ "Div, has been applied too a divsor of zero."
applyOper' Div [x,y] = return $ div x y
applyOper' Div _     = Failure $ "Div, has been applied too an invalid number of arguments."
applyOper' Equ [x,y] = return $ if x == y then 1 else 0
applyOper' Equ _     = Failure $ "Equ, has been applied too an invalid number of arguments."
applyOper' Gtr [x,y] = return $ if x >  y then 1 else 0
applyOper' Gtr _     = Failure $ "Gtr, has been applied too an invalid number of arguments."
applyOper' Ltn [x,y] = return $ if x <  y then 1 else 0
applyOper' Ltn _     = Failure $ "Ltn, has been applied too an invalid number of arguments."
applyOper' Not [x]   = return $ if x >  0 then 0 else 1
applyOper' Not _     = Failure $ "Not, has been applied too an invalid number of arguments."

--
-- @Description   : Apply an identifier to a list of - reduced - expressions.
-- @Creation Date : 18'th June 2018.
--

call               :: Stack Expr -> Iden -> [Expr] -> Environ Expr
call stack iden ps  = harvest stack iden >>= \unit -> build unit ps
