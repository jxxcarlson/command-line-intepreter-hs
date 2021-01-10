module Exec(exec) where 

import Data.List.Utils (replace)
import Data.Maybe(catMaybes)
import Info
import qualified Data.Text.IO as TIO
 
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
         "/stat" -> stat
         "/display" -> Exec.display
         _ -> putStrLn  $ prefix ++  "I don't understand\n" ++ str


dropCommand :: String -> String -> String 
dropCommand cmd str = drop ((length cmd)+ 2) str


display :: IO ()
display = do
    text <- TIO.readFile "data.txt"
    let records = Info.getMany Info.sep text
    putStrLn $ Info.displayMany $ catMaybes records


stat :: IO ()
stat = do
    text <- TIO.readFile "data.txt"
    let records = Info.getMany Info.sep text
    putStrLn $ show $ (length records, length $ catMaybes records)

