{-# LANGUAGE OverloadedStrings #-}

module Exec where 

import Data.List.Utils (replace)
import Data.Maybe(catMaybes)
import Info 
import qualified Data.Text.IO as TIO
import qualified Data.Text as T


 
-- DISPATCHER

data PState = PState { count :: Int
                     , records :: [Maybe Info]
                     , message :: String
                     , addressesPath :: String
                     }

initialPState :: PState
initialPState = PState { count = 0, records = [], message = "", addressesPath = "test.txt"}

prefix = "-----\n"

exec :: PState -> String -> IO PState
exec pState str = 
  case words str of
     [] -> return pState { message = ""}
     (cmd:args) -> 
       case cmd of
         "/help" -> help pState
         "/setfile" -> setFile pState args
         "/enter" -> enter pState str
         "/echo" -> return pState {message = prefix ++ dropCommand "/echo" str}
         "/stat" -> stat pState
         "/display" -> display pState
         "/mail" -> mail pState args
         _ -> return pState {message = prefix ++  "I don't understand\n" ++ str }



help :: PState -> IO PState
help pState = do
  text <- TIO.readFile  "help.txt"
  return pState { message = T.unpack text}



mail :: PState -> [String] -> IO PState
mail pState args =
    case args of 
        [] -> return pState {message = "No message file given"}
        (filePath:_) ->
            do
            message <- TIO.readFile filePath
            return pState { message = T.unpack message}

setFile :: PState -> [String] -> IO PState
setFile pState args =
    case args of 
        [] -> return pState {message = "No file name given"}
        (filePath:_) ->
            do
            return pState { message = "addresses: " ++ filePath, addressesPath = filePath}



enter :: PState -> String -> IO PState
enter pState str =
    let
      record = Info.get $ dropCommand "/enter" str   
    in 
      case record of
        Nothing -> return pState {message = "Error parsing record"}
        Just rec -> 
            do
                text <- TIO.readFile (addressesPath pState)
                let text' = T.append (Info.toText rec) text
                let records = Info.getMany Info.sep text
                TIO.writeFile (addressesPath pState) text'
                return pState {message = show rec, records = (Just rec):records}  


carryState :: PState -> IO () -> (PState, IO ())
carryState pState io = (pState, io)

dropCommand :: String -> String -> String 
dropCommand cmd str = drop ((length cmd)+ 2) str


display :: PState -> IO PState
display pState = do
    text <- TIO.readFile (addressesPath pState)
    let records = Info.getMany Info.sep text
    return pState { records = records, message = Info.displayMany $ catMaybes records }


stat :: PState -> IO PState
stat pState = do
    text <- TIO.readFile (addressesPath pState)
    let records = Info.getMany Info.sep text
    return pState { records = records, message = show (length records, length $ catMaybes records) }

