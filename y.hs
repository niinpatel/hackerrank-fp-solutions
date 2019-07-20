import Control.Monad

pp :: [String] -> IO ()
pp = mapM_ putStrLn


baseTop = replicate 31 (replicate 100 '_')  
baseMid = [replicate (33 + x) '_' ++ ['1'] ++ replicate (31 - 2 * x) '_' ++ ['1'] ++ replicate (100 - 33 + x - 33) '_' | x <- [ 0 .. 15 ]]
baseBottom = replicate 16 (replicate 49 '_' ++ ['1'] ++ replicate 50 '_')

base = baseTop ++ baseMid ++ baseBottom


baseY = [replicate x '_' ++ ['1'] ++ replicate (31 - 2 * x) '_' ++ ['1'] ++ replicate x '_' | x <- [ 0 .. 15 ]] ++ replicate 16 (replicate 16 '_' ++ ['1'] ++ replicate 16 '_')

shrink = dropAlternate . map dropAlternate

dropAlternate x = map fst $ filter (even . snd) $ zip x [0..]


y 1 = map (\x -> replicate 33 '_' ++ x ++ replicate 34 '_') baseY
y 2 = map (\x -> replicate 25 '_' ++ x ++ replicate 26 '_') (zipWith (\x y -> x ++ replicate 15 '_' ++ y) (shrink baseY) (shrink baseY)) ++ y 1
y 3 = map (\x -> replicate 21 '_' ++ x ++ replicate 22 '_') (zipWith (\x y -> x ++ replicate 7 '_' ++ y ++ replicate 7 '_' ++ x ++ replicate 7 '_' ++ y) (shrink $ shrink baseY) (shrink $ shrink baseY)) ++ y 2
