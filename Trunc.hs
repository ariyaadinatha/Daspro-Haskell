konso :: Int -> [Int] -> [Int]
konso e li = [e]++li

konsDot :: [Int] -> Int -> [Int]
konsDot li e = li++[e]

isEmpty :: [Int] -> Bool
isEmpty l = null l

isOneElmt :: [Int] -> Bool
isOneElmt l = length l == 1

{- ========================================================================== -}

-- trunc
{- trunc l i mengembalikan i buah elemen terdepan dari l.
Apabila i lebih besar dari jumlah elemen pada l,
maka fungsi mengirimkan l. Prekondisi: i > 0
Contoh: trunc [3,2,6,5,8] 3 = [3,2,6] -}

trunc :: [Int] -> Int -> [Int]
trunc l i
   | i==1 = konso (head l) []
   | length l < i = l
   | otherwise = konso (head l) (trunc (tail l) (i-1))

-- Aplikasi
-- trunc [1,2,3,4] 2
-- hasil [1,2]

{- ========================================================================== -}


-- trunc Alternatif 1
trunc1 :: [Int] -> Int -> [Int]
trunc1 l i = if length l<i then l
             else if i==1 then konsDot [] (head l)
             else konso (head l) (trunc1 (tail l) (i-1))
