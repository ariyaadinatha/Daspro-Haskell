{-Buatlah definisi, spesifikasi, dan realisasi sebuah fungsi sumInteger yang menerima 2 (dua) buah integer positif
(>0), misalnya m dan n, dan sebuah fungsi f dan menghasilkan penjumlahan dari semua integer antara m dan n
(termasuk m dan n) yang memenuhi f. Jika dalam selang m dan n tidak ada yang memenuhi f, maka hasilnya
adalah 0.-}

sumInteger :: Int -> Int -> (Int -> Bool) -> Int
isGenap :: Int -> Bool
gtThan5 :: Int -> Bool

isGenap x = mod x 2 == 0
gtThan5 x = x>5

sumInteger m n f
   | m>n = 0
   | f m = m+sumInteger (m+1) n f
   | not (f (m)) = 0+sumInteger (m+1) n f



{- ============================================================================= -}
-- Jawaban Alternatif
sumInteger1 m n f = if m>n then 0
                    else if f(m) then m+sumInteger(m+1) n f
                    else 0+sumInteger (m+1) n f
