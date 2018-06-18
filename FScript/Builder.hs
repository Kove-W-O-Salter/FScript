
--
-- @File          : FScript/Builder.hs
-- @Module        : FScript.Builder
-- @Project       : FScript
-- @Description   : Suite for constructing identifiers.
-- @Maintainer    : Kove W. Ochre-Salter
-- @Copyright     : (c) Kove W. Ochre-Salter, 2018
-- @Creation Date : 18'th June 2018
--

module FScript.Builder where

--
-- @Reason        : To build a identifier, whose body is of the Expr type,
--                  we need access to the Expr type.
-- @Creation Date : 18'th June 2018.
--

import FScript.Expr

--
-- @Reason        : To build a identifier we need to have access to the
--                  identifier data type (of which identifiers are members).
-- @Creation Date : 18'th June 2018.
--

import FScript.Stack

--
-- @Reason        : A identifier can be built with an
--                  invalid number of given paramaters.
-- @Creation Date : 18'th June 2018.
--

import FScript.Environ

--
-- @Description : Declare a identifiers paramaters, in it's body,
--                as the paramaters that it is called with. 
--

build              :: Unit Expr -> [Expr] -> Environ Expr
build unit gparams  | eparams' == gparams' = Success $ foldr (\(e,p) expr -> Dec e [] p expr) defin egparams
                    | otherwise            = Failure $ iden ++ ", cannot be built; since it's number of expected arguments is different to that given."
                    where (Unit iden eparams defin) = unit
                          eparams'                  = length eparams
                          gparams'                  = length gparams
                          egparams                  = zip eparams gparams
