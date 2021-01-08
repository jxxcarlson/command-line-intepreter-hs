module Exec(exec) where 

import Data.List.Utils (replace)
 
-- DISPATCHER

prefix = "-----\n"

exec :: String -> IO ()
exec str = 
  case words str of
     [] -> putStr ""
     (cmd:args) -> 
       case cmd of
         "/help" -> putStrLn $ prefix ++ "No help file yet"
         "/echo" -> putStrLn $ prefix ++ (drop 7 str)
         _ -> putStrLn  $ prefix ++  "I don't understand\n" ++ str


