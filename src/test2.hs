import System.IO
-- import Data.List.Split
import Data.List

split :: String -> [String] 
split [] = [""] 
split (c:cs) 
    | c == '\n' = "" : rest 
    | otherwise = (c : head rest) : tail rest 
    where rest = split cs

main = do
    contents <- readFile "/home/cse230/Desktop/test.txt"
    let x = init $ split contents
    let y = sort [ read a::Integer | a <-x]
    let result = take 5 y
    print x
    print y
    print result
    -- let y = [ read a::Integer | a <-x]
    -- let result = take 5 ( sort y )
    -- print result
    