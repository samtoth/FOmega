module Walk where

    isValidWalk :: [Char] -> Bool
    isValidWalk xs = closed xs (0,0) 0

    closed :: [Char] -> (Int, Int) -> Int -> Bool
    closed _ _ 11 = False
    closed [] (0,0) 10 = True
    closed [] _ _ = False
    closed (c:cs) (x,y) n = case c of
        'n' -> closed cs (x, y+1) (n+1)
        'e' -> closed cs (x+1, y) (n+1)
        's' -> closed cs (x, y-1) (n+1)
        'w' -> closed cs (x-1, y) (n+1)