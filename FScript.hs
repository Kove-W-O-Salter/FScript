
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
main  = do putStr "Fs> "
           l <- getLine
           case l of
             ":Load"  -> prompt "Load File> " >>= readFile >>= \c -> putStrLn (run c) >> main
             ":Quit"  -> return ()
             ":Clear" -> clear 2 >> main
             _    -> putStrLn (run l) >> main

--
-- @Description   : Prompt the user for input, recursively
--                  calling itself in the case of null input.
-- @Creation Date : 18'th June 2018.
--

prompt   :: String -> IO String
prompt s  = do putStr s
               l <- getLine
               if null l then
                 prompt s
               else
                 return l

--
-- @Description   : Clear the display.
-- @Creation Date : 18'th June 2018.
--

clear   :: Int -> IO ()
clear n  = do putStr "\ESC[0;0H"
              putStr $ "\ESC[" ++ show n ++ "J"

--
-- @Description   : Run the source source and return it's result as a String.
-- @Creation Date : 18'th June 2018.
--
run        :: String -> String
run source  = case fScript source of
                Right e -> show (exec e)
                Left  x -> show x
