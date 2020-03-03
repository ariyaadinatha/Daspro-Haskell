-- Definisi dan Spesifikasi
pangkat :: Int -> Int -> Int

-- Realisasi
pangkat a b = if b==0 then 1
              else a*pangkat a (b-1)
