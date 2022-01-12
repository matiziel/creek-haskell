import Creek
import System.IO

readCreek :: String -> Creek
readCreek = read

main = do
  putStrLn "Podaj ścieżkę do pliku:"
  line <- getLine
  contents <- readFile line
  let creek = readCreek contents
  print creek
