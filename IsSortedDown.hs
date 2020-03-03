konso :: Int -> [Int] -> [Int]
konso e li = [e]++li

konsDot :: [Int] -> Int -> [Int]
konsDot li e = li++[e]

isEmpty :: [Int] -> Bool
isEmpty l = null l

isOneElmt :: [Int] -> Bool
isOneElmt l = length l == 1

{- ================================================================= -}
-- Definisi dan Spesifikasi
{- isSortedDown l mengembalikan True apabila l terurut mengecil.
Prekondisi: list tidak kosong
Contoh: isSortedDown [8,5,2,4] = False; isSortedDown [5,2,0] = True -}
isSortedDown :: [Int] -> Bool

-- Realisasi
isSortedDown l = if isOneElmt l then True
                 else if head l > head(tail l) then isSortedDown (tail l)
                 else False
