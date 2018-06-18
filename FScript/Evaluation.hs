
--
-- @File          : FScript/Evaluation.hs
-- @Module        : FScript.Evaluation
-- @Project       : FScript
-- @Description   : Evaluate an expression
-- @Maintainer    : Kove W. Ochre-Salter
-- @Copyright     : (c) Kove W. Ochre-Salter, 2018
-- @Creation Date : 18'th June 2018
--

module FScript.Evaluation where

--
-- @Reason        : To evaluate an expression I must have access
--                  to the expression type, Expr.
-- @Creation Date : 18'th June 2018.
--

import FScript.Expr

--
-- @Reason        : To evaluate an expression we must
--                  have acces to the stack type, Stack.
-- @Creation Date : 18'th June 2018.
--

import FScript.Stack

--
-- @Reason        : To evalute an expression whose reduction may fail
--                  I must, in turn, be prepaired to also fail.
-- @Creation Date : 18'th June 2018.
--

import FScript.Environ

--
-- @Reason        : To evaluate an expression I must first be capable
--                  of reducing it.
-- @Creation Date : 18'th June 2018.
--

import FScript.Reduction

--
-- @Description   : Evaluate an expression to an
--                  integer contained in an environment
-- @Creation Date : 18'th June 2018.
--

eval            :: Stack Expr -> Expr -> Environ Int
eval stack expr  = reduce stack expr >>= \(Con n) -> return $ n
