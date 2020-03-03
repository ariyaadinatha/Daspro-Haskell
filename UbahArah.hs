-- Definisi dan Realisasi
{- ubahArah s r adalah fungsi yang menerima masukan dua buah integer s dan r yang merepresentasikan arah
pergerakan dan besar perubahan arah yang akan dilakukan (dalam satu derajat). Fungsi mengembalikan arah
pergerakan yang baru dalam range 0 s.d. 359, setelah s diputar sebesar r. -}
ubahArah :: Int -> Int -> Int

-- Realisasi
ubahArah s x =
   let
    a=s+x
   in
    if a>360 then a-360
    else if a<0 then a+360
    else a
