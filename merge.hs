msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort xs') (msort xs'')
  where
    (xs', xs'') = splitAt (length xs `div` 2) xs

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge xs'@(x:xs) ys'@(y:ys)
  | x <= y    = x : merge xs  ys'
  | otherwise = y : merge xs' ys