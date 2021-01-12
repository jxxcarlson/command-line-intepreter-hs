module Info(Info, displayMany, get, get', getMany, sep, toText) where  

import Parser (Parser, field, char, runParser)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (catMaybes)

data Info = Info {firstName :: String, lastName :: String, email:: String}
    deriving Show


info :: Parser Info
info = do
        fName <- Parser.field "FirstName"  <* char '\n'
        lName <- Parser.field "LastName"  <* char '\n'
        email <- Parser.field "EmailAddress" 
        return (Info fName lName email)


info' :: Parser Info
info' = do
        fName <- Parser.field "firstName"  <* char '\n'
        lName <- Parser.field "lastName"  <* char '\n'
        email <- Parser.field "email" 
        return (Info fName lName email)




displayMany :: [Info] -> String
displayMany infos = 
    displayStrings $ map display infos
 
display  :: Info -> String
display info = 
     (firstName info) ++ " " ++ (lastName info) ++ ": " ++ (email info)


toText  :: Info -> T.Text
toText info = 
     T.pack $ "FirstName: " ++ (firstName info) ++ "\n" ++ "LastName: " ++ (lastName info) ++ "\n" ++ "EmailAddress: " ++ (email info) ++ "\n----\n"


displayStrings :: [String] -> String
displayStrings items = 
    foldl (\item acc -> item ++ "\n" ++ acc) (head items) (drop 1 items)
 

get :: String -> Maybe Info
get str =  
    case Parser.runParser info str of
        (_, Left _) -> Nothing 
        (_, Right info_) -> Just info_

get' :: String -> Maybe Info
get' str = 
    case Parser.runParser info' str of
        (_, Left _) -> Nothing 
        (_, Right info_) -> Just info_

getMany :: T.Text -> T.Text -> [Maybe Info]
getMany sep text = map (\d -> get $ trimLeading $ T.unpack d) (T.splitOn sep text)

sep = T.pack "----"


trimLeading :: String -> String
trimLeading = dropWhile (\c -> c == ' ' || c == '\n')
