data TTT aa = Leaf [aa]
                | Node [aa] (TTT aa) (TTT aa) (TTT aa)
                deriving (Eq,Ord,Show,Read)
    
mm :: (aa -> aa) -> (TTT aa) -> (TTT aa)
mm f (Leaf a) = Leaf (map f a)
mm f (Node a t1 t2 t3) = Node (map f a) (mm f t1) (mm f t2) (mm f t3)
    
a :: Integer -> Integer
a x = x * x

b :: Integer -> Integer
b x = x `mod` 7

c :: Integer -> Integer
c x = x + 100

tree :: TTT Integer
tree = (Node [44, -23] (Leaf [5, -14, 12, 13, 5]) (Node [33, 16] (Leaf [3, 2]) (Leaf [6]) (Leaf [7])) (Node [33, 16] (Leaf [3, 2]) (Leaf [-6]) (Leaf [7])))

ff_a :: TTT Integer
ff_a = mm a (Node [44, -23] (Leaf [5, -14, 12, 13, 5]) (Node [33, 16] (Leaf [3, 2]) (Leaf [6]) (Leaf [7])) (Node [33, 16] (Leaf [3, 2]) (Leaf [-6]) (Leaf [7])))

ff_b :: TTT Integer
ff_b = mm b (Node [44, -23] (Leaf [5, -14, 12, 13, 5]) (Node [33, 16] (Leaf [3, 2]) (Leaf [6]) (Leaf [7])) (Node [33, 16] (Leaf [3, 2]) (Leaf [-6]) (Leaf [7])))

ff_c :: TTT Integer
ff_c = mm c (Node [44, -23] (Leaf [5, -14, 12, 13, 5]) (Node [33, 16] (Leaf [3, 2]) (Leaf [6]) (Leaf [7])) (Node [33, 16] (Leaf [3, 2]) (Leaf [-6]) (Leaf [7])))