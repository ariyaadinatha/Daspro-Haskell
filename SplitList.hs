konso :: Int -> [Int] -> [Int]
konso e li = [e]++li

konsDot :: [Int] -> Int -> [Int]
konsDot li e = li++[e]

isEmpty :: [Int] -> Bool
isEmpty l = null l

isOneElmt :: [Int] -> Bool
isOneElmt l = length l == 1

{- ================================================================================== -}

{- splitList l mengembalikan dua buah list lfront dan lback,
dengan lfront memuat elemen l bagian depan dan lback memuat elemen l
bagian belakang. Jumlah elemen pada lfront sama dengan atau 1 elemen
lebih banyak dari pada lback.
Contoh: splitList [3,2,6,5,8] = ([3,2,6],[5,8]) -}

-- Credit @Melitaaaaaaaaaaaaa
splitList l =
  if (isEmpty l) then ([], [])
  else if (isOneElmt l) then ([head l],[])
    else
      let
       (lfront, lback) = splitList (tail(init l))
      in
       ((konso (head l) lfront), (konsDot lback (last l)))

{- ============================================================================= -}

-- Jawaban menggunakan fungsi antara
-- splitList Alternatif 1
bagi1 :: [Int] -> Int
bagi1 l = div (length l) 2 + mod (length l) 2
bagi2 :: [Int] -> Int
bagi2 l = div (length l) 2

split1 :: [Int] -> Int -> [Int]
split1 l z
   | z==1 = konso (head l) []
   | otherwise = konso (head l) (split1 (tail l) (z-1))

split2 :: [Int] -> Int -> [Int]
split2 l z
   | z==1 = konso (last l) []
   | otherwise = konsDot (split2 (init l) (z-1)) (last l)

splitList1 :: [Int] -> ([Int],[Int])
splitList1 l= (split1 l (bagi1 l), split2 l (bagi2 l))

-- Aplikasi
-- splitList [1,2,3,4,5]
-- hasil ([1,2,3],[4,5])


{- ============================================================================= -}
-- Jawaban haram, jangan digunakan saat UTS karena tidak menggunakan konsep rekurens
-- pakai buat praktikum aja h3h3h3h3h3

splitList2 li =
  if isEmpty li then ([], [])
  else
   let
    median = (div (length li) 2) + (mod (length li) 2)
    li1= take median li
    li2= drop median li
   in
    (li1,li2)
