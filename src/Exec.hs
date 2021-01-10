module Exec(exec) where 

import Data.List.Utils (replace)
import Info
 
-- DISPATCHER

prefix = "-----\n"

exec :: String -> IO ()
exec str = 
  case words str of
     [] -> putStr ""
     (cmd:args) -> 
       case cmd of
         "/help" -> putStrLn $ prefix ++ "No help file yet"
         "/enter" -> putStrLn $ show $ Info.get $ dropCommand "/enter" str
         "/echo" -> putStrLn $ prefix ++ dropCommand "/echo" str -- drop 7 str)
         _ -> putStrLn  $ prefix ++  "I don't understand\n" ++ str


dropCommand :: String -> String -> String 
dropCommand cmd str = drop ((length cmd)+ 2) str