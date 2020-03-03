konso :: Int -> [Int] -> [Int]
konso e li = [e]++li

konsDot :: [Int] -> Int -> [Int]
konsDot li e = li++[e]

isEmpty :: [Int] -> Bool
isEmpty l = null l

isOneElmt :: [Int] -> Bool
isOneElmt l = length l == 1

{- ============================================================================================================= -}
-- credit @Hokki Suwanda
-- nilaiEkstrim
{- nilaiEkstrim l mengembalikan pasangan integer (min,max),
dengan min adalah nilai terkecil pada l dan max adalah nilai terbesar pada l.
Prekondisi: l tidak kosong Contoh: nilaiEkstrim [3,2,,0,56,28] = (0,56) -}

nilaiEkstrim :: [Int] -> (Int,Int)
nilaiEkstrim l =
   if isOneElmt l then (head l, head l) else
   let
    (min,max) = nilaiEkstrim (tail l)
   in
    if head l > max then (min,(head l))
    else if head l < min then ((head l),max)
    else (min,max)

-- Aplikasi
-- nilaiEkstrim1 [1,2,3,4,5]
-- hasil (1,5)

{- ============================================================================================================= -}

-- Jawaban alternatif menggunakan funsi antara
nilaiMaksimum :: [Int] -> Int
nilaiMaksimum l = if length l == 1 then head l
                  else if head l > nilaiMaksimum (tail l) then head l
                  else nilaiMaksimum(tail l)

nilaiMinimum :: [Int] -> Int
nilaiMinimum l = if length l==1 then head l
                 else if head l < nilaiMinimum (tail l) then head l
                 else nilaiMinimum(tail l)

nilaiEkstrim1 :: [Int] -> (Int,Int)
nilaiEkstrim1 l =
   let
    minl = nilaiMinimum
    maxl = nilaiMaksimum
   in
    (minl l, maxl l)
