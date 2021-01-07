module Info(get) where  

import Parser (Parser, field, char, runParser)


data Info = Info {firstName :: String, lastName :: String, email:: String}
    deriving Show


info :: Parser Info
info = do
        fName <- Parser.field "firstName"  <* char '\n'
        lName <- Parser.field "lastName"  <* char '\n'
        email <- Parser.field "email" 
        return (Info fName lName email)


get :: String -> Maybe Info
get str = 
    case Parser.runParser info str of
        (_, Left _) -> Nothing 
        (_, Right info_) -> Just info_