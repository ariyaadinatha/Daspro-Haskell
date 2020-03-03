prevSecond :: Int -> Int -> Int -> (Int, Int, Int)
prevSecond j m d = if d==0 && m/=0 then (j,m-1,59)
                   else if m==0 && d==0 && j/=0 then (j-1,59,59)
                   else if j==0 && m==0 && d==0 then (23,59,59)
                   else
                   let
                    j1=j
                    m1=m
                    d1=d-1
                   in
                    (j1,m1,d1)
