konso :: Int -> [Int] -> [Int]
konso e li = [e]++li

konsDot :: [Int] -> Int -> [Int]
konsDot li e = li++[e]

isEmpty :: [Int] -> Bool
isEmpty l = null l

isOneElmt :: [Int] -> Bool
isOneElmt l = length l == 1

{- =========================== -}
-- Definisi dan Spesifikasi
getSmallest :: [Int] -> Int

-- Realisasi
getSmallest l = if isOneElmt l then head l
                else if head l < getSmallest(tail l) then head l
                else getSmallest(tail l)
