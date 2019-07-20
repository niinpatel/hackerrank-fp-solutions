base =
  map
    (\r ->
       map
         (\c ->
            if r + c >= 33 && c - r <= 31
              then '1'
              else '_')
         [1 .. 63])
    [1 .. 32]

dropEven s = map fst $ filter (odd . snd) $ zip s [0 ..]

serp 0 = base
serp n = top ++ bottom
  where
    half = shrink $ serp (n - 1)
    top = map (\x -> padding ++ x ++ padding) half
    bottom = map (\x -> x ++ "_" ++ x) half

shrink = map dropEven . dropEven

padding = replicate 16 '_'

main = interact $ unlines . serp . read
