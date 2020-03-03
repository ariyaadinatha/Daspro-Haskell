konso :: Int -> [Int] -> [Int]
konso e li = [e]++li

konsDot :: [Int] -> Int -> [Int]
konsDot li e = li++[e]

isEmpty :: [Int] -> Bool
isEmpty l = null l

isOneElmt :: [Int] -> Bool
isOneElmt l = length l == 1


-- Definisi dan Spesifikasi
{- elPosGanjil l mengembalikan sebuah list yang berisi
semua elemen l pada posisi ganjil.
Contoh: elPosGanjil [2,4,6,8,1,3,5,7,9] = [2,6,1,5,9] -}
elPosGanjil :: [Int] -> [Int]

-- Realisasi
elPosGanjil l
   | isOneElmt l = konsDot [] (head l)
   | otherwise = konso (head l) (elPosGanjil(tail(tail l)))
