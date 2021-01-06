import           System.IO  
import           Control.Monad (unless)
import           System.IO
import           Exec (exec)


main :: IO()
main =
  do
    putStrLn "\n\nHello!\n\n"
    loop

loop :: IO ()
loop = do
  input <- read'
  unless (input == ":quit") $ exec input >> loop
  

read' :: IO String
read' = putStr "info > "
     >> hFlush stdout
     >> getLine

