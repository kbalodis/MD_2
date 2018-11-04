import Control.Monad
import Data.List as M
import Data.Maybe
import Data.Map.Strict

rmdups :: (Eq a) => [a] -> [a]
rmdups l = rmdups' l []             
  where rmdups' [] _ = []
        rmdups' (x:xs) ls | x `elem` ls = rmdups' xs ls | otherwise = x : rmdups' xs (x:ls)

aa xs ys = do
    let zs = rmdups xs
    forM_ zs $ \ss -> do
      case M.lookup ss ys of
        Just n -> print n
        Nothing -> return ()

aa1 = aa ["aaa","a","adsafa","v","dfdadfa","dddd", "c", "aaa","a","adsafa"] [("a", "aa"), ("bb", "bbb"), ("c", "def")]

-- cc_list :: [a] -> Maybe a
-- cc_list [] = Nothing
-- cc_list (x:xs) = x

-- type cc_list = [(Char, Char)]

-- append1 :: (a,a) -> [a] -> [a]
-- append1 x xs = M.map (\xss -> xss ++ [x]) xs

-- merge1 :: (Eq a) => [(a, a)] -> [(a, a)] -> [(a, a)]
-- merge1 [] _ = []
-- merge1 _ [] = []
-- merge1 (x:xs) (y:ys)
--     | snd x == fst y = (fst x, snd y) : merge1 xs ys
--     | otherwise = merge1 xs ys

merge1 :: (Eq a) => (a, a) -> [(a, a)] -> [(a, a)]
-- merge1 [] _ = []
merge1 _ [] = []
merge1 x (y:ys)
    | snd x == fst y = (fst x, snd y) : merge1 x ys
    | otherwise = merge1 x ys

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

cc xs ys = do
  let curr_xs = rmdups xs
  let curr_ys = rmdups ys
  let result = []
  forM_ curr_xs $ \ss -> do
    let x = merge1 ss curr_ys
    print(merge result x)
  -- print result
    -- result ++ merge1 xx curr_ys

cc1 = cc [("a","b"), ("e", "b")] [("b","c"), ("b", "d")] 
cc2 = cc [("hello", "hi"), ("foo", "baz"), ("this", "that")] [("foo", "bar"), ("hello", "world"), ("goo", "boo")]