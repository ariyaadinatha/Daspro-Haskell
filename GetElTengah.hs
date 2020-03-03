konso :: Int -> [Int] -> [Int]
konso e li = [e]++li

konsDot :: [Int] -> Int -> [Int]
konsDot li e = li++[e]

isEmpty :: [Int] -> Bool
isEmpty l = null l

isOneElmt :: [Int] -> Bool
isOneElmt l = length l == 1

{- =========================================================== -}
-- Definisi dan Spesifikasi
{- getElTengah l mengembalikan elemen l yang berada di
tengah-tengah. Apabila elemen l berjumlah genap (misalnya n),
maka yang dikembalikan adalah elemen pada posisi (n div 2).
Prekondisi: list tidak kosong
Contoh: getElTengah [3,2,6,5,8] = 6; getElTengah [1,2,3,4] = 2 -}
getElTengah :: [Int] -> Int

-- Realisasi
getElTengah l = if length l==2 || isOneElmt l then head l
                else getElTengah (tail(init l))

{- ==================================================================== -}

-- Tanpa Rekurens
--Alternatif 1
getElTengah1 :: [Int] -> Int
getElTengah1 l = if mod (length l) 2 == 0 then l !! ((div (length l) 2)-1)
                else l !! (div (length l) 2)
