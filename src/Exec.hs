module Exec where 

import Data.List.Utils (replace)
import Data.Maybe(catMaybes)
import Info 
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
 
-- DISPATCHER

data PState = PState { count :: Int, records :: [Maybe Info], message :: String}

initialPState :: PState
initialPState = PState { count = 0, records = [], message = ""}

prefix = "-----\n"

exec :: PState -> String -> IO PState
exec pState str = 
  case words str of
     [] -> return pState { message = ""}
     (cmd:args) -> 
       case cmd of
         "/help" -> return pState {message =  prefix ++ "No help file yet" }
         "/enter" -> enter pState str
         "/echo" -> return pState {message = prefix ++ dropCommand "/echo" str}
         "/stat" -> stat pState
         "/display" -> display pState
         _ -> return pState {message = prefix ++  "I don't understand\n" ++ str }



enter :: PState -> String -> IO PState
enter pState str =
    let
      record = Info.get $ dropCommand "/enter" str   
    in 
      case record of
        Nothing -> return pState {message = "Error parsing record"}
        Just rec -> 
            do
                text <- TIO.readFile "data.txt"
                let text' = T.append (Info.toText rec) text
                let records = Info.getMany Info.sep text
                TIO.writeFile "data.txt" text'
                return pState {message = show rec, records = (Just rec):records}  


carryState :: PState -> IO () -> (PState, IO ())
carryState pState io = (pState, io)

dropCommand :: String -> String -> String 
dropCommand cmd str = drop ((length cmd)+ 2) str


display :: PState -> IO PState
display pState = do
    text <- TIO.readFile "data.txt"
    let records = Info.getMany Info.sep text
    return pState { records = records, message = Info.displayMany $ catMaybes records }


stat :: PState -> IO PState
stat pState = do
    text <- TIO.readFile "data.txt"
    let records = Info.getMany Info.sep text
    return pState { records = records, message = show (length records, length $ catMaybes records) }

