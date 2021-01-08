module Exec(exec) where 

import Data.List.Utils (replace)
 
-- DISPATCHER

exec :: String -> IO ()
exec str = 
  case words str of
     [] -> putStr ""
     (cmd:args) -> 
       case cmd of
         "/help" -> putStrLn "No help file yet"
         "/echo" -> putStrLn (drop 7 str)
         _ -> putStrLn  $ "I don't understand\n" ++ str
