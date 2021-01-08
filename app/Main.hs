import           System.IO  
import           Control.Monad (unless)
import           System.IO
import           Exec (exec)


main :: IO()
main =
  do
    -- putStrLn "\n\nHello!\n\n"
    loop

loop :: IO ()
loop = do
  putStr "info > " >> hFlush stdout  
  input <- innerLoop "" -- read'
  unless (input == "/quit ") $ exec input >> loop



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



