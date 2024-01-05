import Debug.Trace 

data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix x1 x2 = (take (length x1) x2) == x1

removePrefix :: String -> String -> String
removePrefix = drop . length
--Pre: s is a prefix of s'

prefixes :: [a] -> [[a]]
prefixes [] = [[]]
prefixes s = prefixes' s 1

prefixes' :: [a] -> Int -> [[a]]
prefixes' s n
  | length s == n = [s] 
  | otherwise     = (take n s) : prefixes' s (n + 1)

suffixes :: [a] -> [[a]]
suffixes s = take (length s) (iterate tail s)

isSubstring :: String -> String -> Bool
isSubstring x1 x2 = any (== True) (map (isPrefix x1) (suffixes x2))

findSubstrings :: String -> String -> [Int]
findSubstrings x1 x2 = [k | (j, k) <- (zip (map (isPrefix x1) (suffixes x2)) [0..]), j == True]

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf x) = [x]
getIndices (Node []) = [] 
getIndices (Node ((_, t):xs)) = getIndices t ++ getIndices (Node xs)

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition [] y = ([], [], y)
partition x [] = ([], x, [])
partition x'@(x:xs) y'@(y:ys)
  | x /= y    = ([], x', y')
  | x' == y'  = (x', [], [])
  | p == x'   = (p, [], (suffixes y')!!(length p))
  | p == y'   = (p, (suffixes x')!!(length p), [])
  | otherwise = (p, (suffixes x')!!(length p), (suffixes y')!!(length p))
  where p = last [j | (j, k) <- zip (prefixes x') (prefixes y'), j == k]
  
findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' "" (Leaf i) = [i]
findSubstrings' _  (Leaf i) = []
findSubstrings' _  (Node []) = []
findSubstrings' s  (Node (t'@(t:ts)))
  | x == "" && y /= "" = findSubstrings' s (Node ts)
  | otherwise = findSubstrings' y (snd t) ++ findSubstrings' s (Node ts) 
  where (x, y, z) = partition s (fst t)

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s, n) t = insert' (s, n) [] t
-- insert (s, n) (Node []) = Node [(s, Leaf n)]
-- insert (s, n) (Node t'@(t:ts))
--   | x == ""    = insert (s, n) (Node ts)
--   | x == fst t = Node ((s, Leaf n):ts)
--   | otherwise  = Node ((x, Node [(y, Leaf n), (z, snd t)]):ts)
--   where (x, y, z) = partition s (fst t)

insert' :: (String, Int) -> [(String, SuffixTree)] -> SuffixTree -> SuffixTree
insert' (s, n) st (Node []) = Node ((s, Leaf n):st)
insert' (s, n) st (Node t'@(t:ts))
  | x == ""    = (insert' (s, n) (t:st) (Node ts))
  | x == fst t = Node ((s, Leaf n):(ts ++ st))
  | otherwise  = Node ((x, Node [(y, Leaf n), (z, snd t)]):(ts ++ st))
  where (x, y, z) = partition s (fst t)

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring 
  = undefined

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]


