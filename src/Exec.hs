{-# LANGUAGE OverloadedStrings #-}

module Exec where 

import Data.List.Utils (replace)
import Data.Maybe(catMaybes)
import Info 
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Environment

import Data.List.NonEmpty (fromList)
import Network.SendGridV3.Api
import Control.Lens ((^.))
import Network.Wreq (responseStatus, statusCode)

-- sendGridApiKey :: ApiKey
-- sendGridApiKey = ApiKey "SG..."

 
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
         "/mail" -> _mail pState args
         _ -> return pState {message = prefix ++  "I don't understand\n" ++ str }



help :: PState -> IO PState
help pState = do
  text <- TIO.readFile  "help.txt"
  return pState { message = T.unpack text}



_mail :: PState -> [String] -> IO PState
_mail pState args =
    case args of 
        [] -> return pState {message = "No message file given"}
        (filePath:_) ->
            do
            message <- TIO.readFile filePath
            status <- zendMail message
            return pState { message = T.unpack message ++ "Status: " ++ show status}


zendMail :: T.Text -> IO Int
zendMail content_ = do
    sendGridApiKey <- System.Environment.getEnv "SENDGRID_API_KEY"
    eResponse <- sendMail (ApiKey $ T.pack sendGridApiKey) ((testMail content_) { _mailSendAt = Just 1516468000 })
    return eResponse

{-


0: info > message.txt
/mail
info-exe: HttpExceptionRequest Request {
  host                 = "api.sendgrid.com"
  port                 = 443
  secure               = True
  requestHeaders       = [("Authorization","<REDACTED>"),("Content-Type","application/json"),("User-Agent","haskell wreq-0.5.2.1")]
  path                 = "/v3/mail/send"
  queryString          = ""
  method               = "POST"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 401, statusMessage = "Unauthorized"},
      responseVersion = HTTP/1.1, responseHeaders = [("Server","nginx"),("Date","Thu, 14 Jan 2021 15:19:58 GMT")
      ,("Content-Type","application/json"),("Content-Length","116")
      ,("Connection","keep-alive"),("Access-Control-Allow-Origin"
      ,"https://sendgrid.api-docs.io"),("Access-Control-Allow-Methods","POST")
      ,("Access-Control-Allow-Headers","Authorization, Content-Type, On-behalf-of, x-sg-elas-acl")
      ,("Access-Control-Max-Age","600"),("X-No-CORS-Reason","https://sendgrid.com/docs/Classroom/Basics/API/cors.html")]
      , responseBody = (), responseCookieJar = CJ {expose = []}
      , responseClose' = ResponseClose}) "{\"errors\":[{\"message\":\"The provided authorization grant is invalid, expired, or revoked\",\"field\":null,\"help\":null}]}")
-}


testMail :: T.Text -> Mail () ()
testMail content_ =
  let to = personalization $ fromList [MailAddress "jxxcarlson@gmail.com" "James Carlson"]
      from = MailAddress "jxxcarlson@gmail.com" "James Carlson"
      subject = "Test"
      content = fromList [mailContentText content_]
  in mail [to] from subject content 


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

