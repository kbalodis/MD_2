data TTT aa = Leaf aa
                | Node (TTT aa) (TTT aa) (TTT aa)
                deriving (Eq,Ord,Show,Read)
    
mm :: (aa -> aa) -> (TTT aa) -> (TTT aa)
mm f (Leaf a) = Leaf (f a)
mm f (Node t1 t2 t3) = Node (mm f t1) (mm f t2) (mm f t3)
    
a :: Integer -> Integer
a x = x*x

b :: Integer -> Integer
b x = x `mod` 7

c :: Integer -> Integer
c x = x + 100

ff_a = mm a (Node (Leaf 5) (Node (Leaf 3) (Leaf 6) (Leaf 7)) (Leaf 0))
ff_b = mm b (Node (Leaf 5) (Node (Leaf 3) (Leaf 6) (Leaf 7)) (Leaf 0))
ff_c = mm c (Node (Leaf 5) (Node (Leaf 3) (Leaf 6) (Leaf 7)) (Leaf 0))