import           System.IO  
import           Control.Monad (unless)
import           System.IO
import           Exec



main :: IO()
main =
  do
    let pState = PState { count = 0}
    -- putStrLn "\n\nHello!\n\n"
    loop pState

loop :: PState -> IO ()
loop pState = do
  putStr (show (count pState) ++ ": info > " ) >> hFlush stdout  
  input <- innerLoop "" 
  unless (input == "/quit ") $ (snd $ exec pState input) >> loop (pState { count = (count pState) + 1})


innerLoop :: String -> IO String
innerLoop str = do
   line <- getLine
   if line == "" then return ""
   else if head line == '/' then 
       return (line ++ " " ++ str)
   else  
       innerLoop (str ++ "\n" ++ line)


read' :: IO String
read' = putStr "info > "
     >> hFlush stdout
     >> getLine



