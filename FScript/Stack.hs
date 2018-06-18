
--
-- @File          : FScript/Stack.hs
-- @Module        : FScript.Stack
-- @Project       : FScript
-- @Description   : Container for bindings.
-- @Maintainer    : Kove W. Ochre-Salter
-- @Copyright     : (c) Kove W. Ochre-Salter, 2018
-- @Creation Date : 18'th June 2018
--

module FScript.Stack where

--
-- @Reason        : Many operations on a Stack my fail; such as calling an
--                  identifier that is not bound, or, creating a binding
--                  whose identifier conflicts with that of another.
-- @Creation Date : 18'th June 2018.
--

import FScript.Environ

--
-- @Description   : An alias for the String type, whose
--                  name should be interpreted as: an
--                  abreviation for Identifier (What I will
--                  actually use it for).
-- @Creation Date : 18'th June 2018.
--

type Iden = String

--
-- @Description   : The basic unit of the stack which holds all
--                  the information needed to safely store an
--                  identifier (e.i, identifier, paramater
--                  identifiers and definition).
-- @Creation Date : 18'th June 2018.
--

data Unit a = Unit {

  identifier :: Iden,
  paramaters :: [Iden],
  definition :: a
  }
  deriving (Show,Read)

--
-- @Description   : An meaininful alias for the Stack, which
--                  is a list of Unit's.
-- @Creation Date : 19'th June 2018.
--

type Stack a = [Unit a]

--
-- @Desciption    : Check if a Unit with the identifier iden,
--                  exists on the Stack stack.
-- @Creation Date : 18'th June 2018.
--

declared            :: Stack a -> Iden -> Bool
declared stack iden  = any ((==iden) . identifier) stack

--
-- @Description   : Prepend the Unit unit to the Stack stack,
--                  if and only if no other Unit on stack shares
--                  the same identifier as unit's.
-- @Creation Date : 18'th June 2018.
--

declare            :: Stack a -> Unit a -> Environ (Stack a)
declare stack unit  | declared stack iden = Failure $ iden ++ ", cannot be bound in this scope; since it conflicts with another binding."
                    | otherwise           = Success $ unit : stack
                    where iden = identifier unit

--
-- @Description   : Remove the binding with the identifier iden,
--                  from the Stack stack, if and only if it exists.
-- @Creation Date : 18'th June 2018.
--

collect            :: Stack a -> Iden -> Environ (Stack a)
collect stack iden  | declared stack iden = Success $ filter ((/=iden) . identifier) stack
                    | otherwise           = Failure $ iden ++ ", cannot be collected; since it is not bound in this scope."

--
-- @Description   : Obtain a Unit, with the identifier iden,
--                  from the Stack stack; if and only if such a
--                  Unit exists.
-- @Creation Date : 18'th June 2018.
--

harvest            :: Stack a -> Iden -> Environ (Unit a)
harvest stack iden  | declared stack iden = Success $ head (filter ((==iden) . identifier) stack)
                    | otherwise           = Failure $ iden ++ ", cannot be harvested; since it is not bound in this scope."
