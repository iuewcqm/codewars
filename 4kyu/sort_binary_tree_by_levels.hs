-- https://www.codewars.com/kata/52bef5e3588c56132c0003bc--

data TreeNode a = TreeNode {
    left  :: Maybe (TreeNode a),
    right :: Maybe (TreeNode a),
    value :: a
  } deriving (Show)

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels = concat . sort
  where sort Nothing     = []
        sort (Just node) = [value node] : zipWith' (++) [] [] (sort $ left node) (sort $ right node)

zipWith' :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWith' f a b []     []     = []
zipWith' f a b (x:xs) []     = f x b : zipWith' f a b xs []
zipWith' f a b []     (y:ys) = f a y : zipWith' f a b [] ys
zipWith' f a b (x:xs) (y:ys) = f x y : zipWith' f a b xs ys

buildTree :: [a] -> Maybe (TreeNode a)
buildTree l  = fst $ walk $ split 1 l
  where split _ [] = []
        split n l = h : split (2*n) t
          where (h, t) = splitAt n l
        walk [] = (Nothing, [])
        walk ls@([] : _) = (Nothing, ls)
        walk ((h : t) : ls) = (Just $ TreeNode l r h, t : ls'')
          where (l, ls')  = walk ls
                (r, ls'') = walk ls'

main :: IO()
main = do
  print $ treeByLevels $ buildTree $ [1]
  print $ treeByLevels $ buildTree $ [1, 2, 3]
  print $ treeByLevels $ buildTree $ [1, 2, 3, 4, 5, 6, 7]
