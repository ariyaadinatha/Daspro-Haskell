import Data.List

{- insSorted x l menerima sebuah integer x dan sebuah list l yang terurut
membesar serta mengembalikan list l yang telah ditambahkan x sedemikian
sehingga elemen-elemennya tetap terurut membesar.
Contoh: insSorted 4 [2,3,5,6] = [2,3,4,5,6] -}


-- Jawaban berhasil
insSorted :: Int -> [Int] -> [Int]
insSorted x l
   | x < head l = konso x l
   | otherwise = konso (head l) (insSorted x (tail l))

-- Aplikasi
-- insSorted 3 [1,2,4,5]
-- hasil [1,2,3,4,5]


-- Jawaban berhasil, tetapi menggunakan fungsi Sort dari haskell
-- insSorted Alternatif 1
insSorted1 :: Int -> [Int] -> [Int]
insSorted1 x l = sort ([x] ++ l)




konso :: Int -> [Int] -> [Int]
konso e li = [e]++li

konsDot :: [Int] -> Int -> [Int]
konsDot li e = li++[e]

isEmpty :: [Int] -> Bool
isEmpty l = null l

isOneElmt :: [Int] -> Bool
isOneElmt l = length l == 1
