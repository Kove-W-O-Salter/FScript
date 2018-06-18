
--
-- @File          : FScript/Execution.hs
-- @Module        : FScript.Execution
-- @Project       : FScript
-- @Description   : Execute an expression
-- @Maintainer    : Kove W. Ochre-Salter
-- @Copyright     : (c) Kove W. Ochre-Salter, 2018
-- @Creation Date : 18'th June 2018
--

module FScript.Execution where

--
-- @Reason        : To execute an expression we
--                  must have access to the expression
--                  type, Expr.
-- @Creation Date : 18'th June 2018.
--

import FScript.Expr

--
-- @Reason        : To execute an expression, whose evaluation
--                  may fail, I must be prepaired to, my self, faile.
-- @Creation Date : 18'th June 2018.
--

import FScript.Environ

--
-- @Reason        : To execute an expression we must first evaluate it.
-- @Creation Date : 18'th June 2018.
--

import FScript.Evaluation

--
-- @Description   : Execute an expression with
--                  an - initially - empty Stack.
-- @Creation Date : 18'th June 2018.
--

exec :: Expr -> Environ Int
exec  = eval []
