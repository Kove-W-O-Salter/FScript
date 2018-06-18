
--
-- @File          : FScript.hs
-- @Module        : FScript
-- @Project       : FScript
-- @Description   : FScript Interpreter
-- @Maintainer    : Kove W. Ochre-Salter
-- @Copyright     : (c) Kove W. Ochre-Salter, 2018
-- @Creation Date : 18'th June 2018
--

module FScript where

--
-- @Description   : To run an expression we need the parser
--                  and an executor.
-- @Creation Date : 18'th June 2018.
--

import FScript.Parser
import FScript.Execution

--
-- @Description   : FScript REPL.
-- @Creation Date : 18'th June 2018.
--

main :: IO ()
main  = do l <- getLine
           if l==":Load" then do
             putStrLn "Enter file to interpret: "
             f <- getLine
             s <- readFile f
             putStrLn (run s)
           else
             putStrLn (run l)
           main

--
-- @Description   : Run the source source and return it's result as a String.
-- @Creation Date : 18'th June 2018.
--
run        :: String -> String
run source  = case fScript source of
                Right e -> show (exec e)
                Left  x -> show x
