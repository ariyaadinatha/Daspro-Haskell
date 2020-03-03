konso :: Int -> [Int] -> [Int]
konso e li = [e]++li

konsDot :: [Int] -> Int -> [Int]
konsDot li e = li++[e]

isEmpty :: [Int] -> Bool
isEmpty l = null l

isOneElmt :: [Int] -> Bool
isOneElmt l = length l == 1

{- ======================================================== -}

-- Definisi dan Spesifikasi
{- delAllX l x mengembalikan sebuah list l1 dan integer n, dengan l1 memuat elemen-elemen l
elemen bernilai x dihapus dan n memuat jumlah kemunculan elemen bernilai x pada l.-}
delAllX :: [Int] -> Int -> ([Int],Int)

-- Realisasi
delAllX l x
   | isEmpty l = ([],0)
   | otherwise =
      let
       (a,b) = delAllX (tail l) x
      in
       if (head l) == x then (a,b+1)
       else (konso (head l) a,b)


{- ===================================================================== -}
--Jawaban Alternatif, menggunakan fungsi antara
counter :: [Int] -> Int -> Int
counter l x = if null l then 0
              else if x== head l then 1 + (counter (tail l) x)
              else 0 + (counter (tail l) x)

delAllX1 :: [Int] -> Int -> [Int]
delAllX1 l x = if isEmpty l then []
              else if x/=(head l) then konso (head l) (delAllX1 (tail l) x)
              else (delAllX1 (tail l) x)
