module Exec where 

import Data.List.Utils (replace)
import Data.Maybe(catMaybes)
import Info 
import qualified Data.Text.IO as TIO
 
-- DISPATCHER

data PState = PState { count :: Int, records :: [Info] }

prefix = "-----\n"

exec :: PState -> String -> (PState, IO())
exec pState str = 
  case words str of
     [] -> carryState pState $ putStr ""
     (cmd:args) -> 
       case cmd of
         "/help" -> carryState pState $ putStrLn $ prefix ++ "No help file yet"
         "/enter" -> carryState pState $ (putStrLn $ show $ Info.get $ dropCommand "/enter" str)
         "/echo" -> carryState pState $ putStrLn $ prefix ++ dropCommand "/echo" str -- drop 7 str)
         "/stat" -> carryState pState $ stat
         "/display" -> carryState pState $ display
         _ -> carryState pState $ putStrLn  $ prefix ++  "I don't understand\n" ++ str


carryState :: PState -> IO () -> (PState, IO ())
carryState pState io = (pState, io)

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

