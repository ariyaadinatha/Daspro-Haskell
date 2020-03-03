konso :: Int -> [Int] -> [Int]
konso e li = [e]++li

konsDot :: [Int] -> Int -> [Int]
konsDot li e = li++[e]

isEmpty :: [Int] -> Bool
isEmpty l = null l

isOneElmt :: [Int] -> Bool
isOneElmt l = length l == 1

{- ================================================================ -}

-- Definisi dan Spesifikasi
isAllGanjil :: [Int] -> Bool
{- isAllGanjil l mengembalikan true apabila seluruh elemen l adalah bilangan ganjil.
Fungsi mengembalikan true jika l adalah list kosong -}

isAllGanjil l = if isEmpty l then True
                else if mod (head l) 2 /= 0 then isAllGanjil(tail l)
                else False
