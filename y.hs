baseY = [replicate x '_' ++ ['1'] ++ replicate (31 - 2 * x) '_' ++ ['1'] ++ replicate x '_' | x <- [ 0 .. 15 ]] ++ replicate 16 (replicate 16 '_' ++ ['1'] ++ replicate 16 '_')

shrink = dropAlternate . map dropAlternate

dropAlternate x = map fst $ filter (even . snd) $ zip x [0..]

iterShrink n = iterate shrink baseY !! (n - 1)

getPadding starting n = scanl (-) starting [floor $ 8 * 2 ** (-x) | x <- [0..]] !! (n - 1)

leftRightPad n x = replicate (getPadding 33 n) '_' ++ x ++ replicate (getPadding 33 n + 1) '_'
copy n x = concat (replicate (2 ^ (n - 1) - 1) (x ++ replicate (getPadding 15 (n - 1)) '_')) ++ x 

y 0 = []
y n = map (leftRightPad n . copy n) (iterShrink n) ++ y (n - 1)

addTopPadding tree = replicate (63 - length tree) (replicate 100 '_') ++ tree

makeTree n = let tree = y n in addTopPadding tree

printTree :: [String] -> IO ()
printTree = mapM_ putStrLn

main = do
    n <- getLine
    printTree $ makeTree $ read n