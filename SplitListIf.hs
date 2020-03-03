konso :: Int -> [Int] -> [Int]
konso e li = [e]++li

konsDot :: [Int] -> Int -> [Int]
konsDot li e = li++[e]

isEmpty :: [Int] -> Bool
isEmpty l = null l

isOneElmt :: [Int] -> Bool
isOneElmt l = length l == 1

isGenap :: Int -> Bool
isGenap x = (mod x 2) == 0

gtThan5 :: Int -> Bool
gtThan5 x = x>5

isPositif :: Int -> Bool
isPositif x = x>0

-- Definisi dan Spesifikasi
splitListIf :: [Int] -> (Int -> Bool) -> ([Int],[Int])



-- Realisasi
splitListIf l f = if isEmpty l then ([],[])
                  else
                   let
                    (a,b) = splitListIf (tail l) f
                   in
                    if f (head l) == True then (konso (head l) a, b)
                    else (a,konso (head l) b)
