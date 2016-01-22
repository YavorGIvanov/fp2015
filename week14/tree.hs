data Tree a = Empty | Node a (Tree a) (Tree a)

getInfo :: Tree a -> a
getInfo Empty = error "The empty tree doesn't have info!"
getInfo (Node info _ _) = info

getLeftT :: Tree a -> Tree a
getLeftT Empty = error "The empty tree doesn't have left tree!"
getLeftT (Node _ left _) = left

getRightT :: Tree a -> Tree a
getRightT Empty = error "The empty tree doesn't have right tree!"
getRightT (Node _ _ right) = right

levelSum :: Int -> Tree Int -> Int
levelSum _ Empty = 0
levelSum level tree
	|level < 1 = error "Level too small"
	|level == 1 = (getInfo tree)
	|otherwise = levelSum (level-1) (getLeftT tree) + levelSum (level-1) (getRightT tree)

maxLevel :: Tree Int -> Int
maxLevel Empty = 0
maxLevel tree = 1 + max (maxLevel (getLeftT tree)) (maxLevel (getRightT tree))

cone :: Tree Int -> Bool
cone Empty = True
cone tree = cone' 1 (maxLevel tree) tree
	where
		cone' level maxlvl tree 
			|level == maxlvl = True
			|otherwise = (levelSum level tree < levelSum (level + 1) tree) && (cone' (level + 1) maxlvl tree)