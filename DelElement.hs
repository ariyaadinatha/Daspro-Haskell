konso :: Int -> [Int] -> [Int]
konso e li = [e]++li

konsDot :: [Int] -> Int -> [Int]
konsDot li e = li++[e]

isEmpty :: [Int] -> Bool
isEmpty l = null l

isOneElmt :: [Int] -> Bool
isOneElmt l = length l == 1

{- ==================================== -}

-- Definisi dan Spesifikasi
{- delElement x l mengembalikan list l dengan elemen x yang telah dihapus dari l.
Jika x bukan elemen l, maka fungsi mengembalikan l semula.
Prekondisi: elemen l unik (setiap elemen hanya muncul 1 kali). -}
delElement :: Int -> [Int] -> [Int]

-- Realisasi
delElement x l = if isEmpty l then []
                 else if x==head l then delElement x (tail l)
                 else konso (head l) (delElement x (tail l))
