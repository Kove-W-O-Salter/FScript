
--
-- @File          : FScript/Environ.hs
-- @Module        : FScript.Environ
-- @Project       : FScript
-- @Description   : Environment of execution
-- @Maintainer    : Kove W. Ochre-Salter
-- @Copyright     : (c) Kove W. Ochre-Salter, 2018
-- @Creation Date : 18'th June 2018
--

module FScript.Environ where

--
-- @Description   : The environment of execution which may
--                  be either: failure, with an error message;
--                  or, a successful result.
-- @Note          : Is mainly used to throw exceptions.
-- @Creation Date : 18'th June 2018.
--

data Environ a = Failure String
               | Success a
               deriving (Show,Read)

--
-- @Description   : Making my environment a Functor
--                  so that I can, eventually, make it a Monad.
-- @Creation Date : 18'th June 2018.
--

instance Functor Environ where
  -- fmap :: (a -> b) -> Environ a -> Environ b
  fmap f (Failure m) = Failure m 
  fmap f (Success x) = Success $ f x

--
-- @Description   : Making my environment an Applicative
--                  so that I can, eventually, make it a Monad.
-- @Creation Date : 18'th June 2018.
--

instance Applicative Environ where
  -- pure :: a -> Environ a
  pure = Success

  -- (<*>) :: Environ (a -> b) -> Environ a -> Environ b
  (Failure m) <*> _           = Failure m
  _           <*> (Failure m) = Failure m
  (Success f) <*> (Success x) = pure $ f x

--
-- @Description   : Making my environment a Monad for
--                  ease of computation. Since at any
--                  any point in a program someone can,
--                  for example, use an undeclared identifier;
--                  in which case we must throw an error. 
-- @Creation Date : 18'th June 2018.
--

instance Monad Environ where
  -- (>>=) :: Environ a -> (a -> Environ b) -> Environ b
  (Failure m) >>= f = Failure m
  (Success x) >>= f = f x
