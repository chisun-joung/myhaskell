-- Binary Tree Level Order Traversal II

data Tree = Empty | Node  Int Tree Tree deriving Show


--         1
--        / \
--       /   \
--      /     \
--     2       3
--    / \     /
--   4   5   6
--  /       / \
-- 7       8   9

t:: Tree
t = Node 1
            (Node 2
                  (Node 4
                        (Node 7 Empty Empty)
                        Empty)
                  (Node 5 Empty Empty))
            (Node 3
                  (Node 6
                        (Node 8 Empty Empty)
                        (Node 9 Empty Empty))
                  Empty)



levelorderTop :: Tree -> [[Int]]
levelorderTop Empty = []
levelorderTop (Node v l r) = [v] : merge ( levelorderTop l) (levelorderTop r)

merge :: [[Int]] -> [[Int]] -> [[Int]]
merge x [] = x
merge [] x = x
merge (x:xs) (y:ys) = (x ++ y) : merge xs ys

levelorderBottom :: Tree -> [[Int]]
levelorderBottom = reverse.levelorderTop