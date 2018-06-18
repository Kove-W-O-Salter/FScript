
--
-- @File          : FScript/Reduction.hs
-- @Module        : FScript.Reduction
-- @Project       : FScript
-- @Description   : Reduction of expressions to constant values
-- @Maintainer    : Kove W. Ochre-Salter
-- @Copyright     : (c) Kove W. Ochre-Salter, 2018
-- @Creation Date : 18'th June 2018
--

module FScript.Reduction where

--
-- @Reason        : To reduce an expression I need access the Expr type.
-- @Creation Date : 18'th June 2018.
--

import FScript.Expr

--
-- @Reason        : To reduce an expression I must be able to
--                  sbustitute identifiers for their values.
-- @Creation Date : 18'th June 2018.
--

import FScript.Stack

--
-- @Reason        : We must reduce code inside it's execution
--                  environment to handle any exceptions and errors.
-- @Creation Date : 18'th June 2018.
--

import FScript.Environ

--
-- @Reason        : To reduce an expression that contains an
--                  identifier I must be able to build it's body.
-- @Creation Date : 18'th June 2018.

import FScript.Builder

--
-- @Reason        : To reduce an expression I must
--                  be able to apply operorators and identifiers.
-- @Creation Date : 18'th June 2018.
--

import FScript.Application

--
-- @Description   : Reduce an expression to a constant integer value.
-- @Creation Date : 18'th June 2018.
--

reduce :: Stack Expr -> Expr -> Environ Expr
reduce stack (Con n)        = return $ Con n
reduce stack (App o es)     = unEnviron (map (reduce stack) es) >>= applyOper o
reduce stack (Dec i ps b e) = declare stack (Unit i ps b) >>= \stack' -> reduce stack' e
reduce stack (Cal i ps)     = call stack i ps >>= reduce stack
reduce stack (Err m)        = Failure m
