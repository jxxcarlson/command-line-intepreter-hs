module Exec(exec) where
 
-- DISPATCHER

exec :: String -> IO ()
exec str = 
  case words str of
     [] -> putStrLn "??"
     (cmd:args) -> 
       case cmd of
         "help" -> putStrLn "No help file yet"
         _ -> putStrLn  $ "I don't understand '" ++ str ++ "'"
