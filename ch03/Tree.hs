data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)


simpleTree = Node "Parent" (Node "Left Child" Empty Empty)
                           (Node "Right Child" Empty Empty)

simpleTree1 = Node "Parent" Empty Empty

data MyTree a = MyNode a (Maybe (MyTree a)) (Maybe (MyTree a))
                deriving (Show)
