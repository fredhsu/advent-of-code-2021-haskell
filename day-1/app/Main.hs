module Main where

greater :: [Int] -> Int -> Int
greater (x:[]) sum = sum
greater (x:y:xs) sum = if y > x
                        then greater (y:xs) (sum + 1)
                       else
                        greater (y:xs) sum

readNumbers :: [String] -> [Int]
readNumbers l = map (read) l

window :: [Int] -> [Int]
window [] = []
window (a:[]) = []
window (a:b:[]) = []
window (a:b:c:rest) = (a + b + c) : window (b:c:rest)

main :: IO ()
main = do  
   let file = "input.txt" 
   contents <- readFile file 
   let l = lines contents
   let l' = readNumbers l
   let g = greater l' 0
   print g
   let wins = window l'
   print (greater wins 0)
   

