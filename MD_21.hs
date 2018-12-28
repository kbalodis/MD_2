import Control.Monad
import Data.List as M
import Data.Maybe (maybeToList)
import Data.Map.Strict

-- No saraksta tiek izņemti duplikāti
rmdups :: (Eq a) => [a] -> [a]
rmdups l = rmdups' l []             
  where rmdups' [] _ = []
        rmdups' (x:xs) ls | x `elem` ls = rmdups' xs ls | otherwise = x : rmdups' xs (x:ls)

-- Funkcija aa, kas prasīta 1. mājas darba A. piemērā
aa xs ys = do
    let zs = rmdups xs
    let result = funct zs ys
    concat result

funct xs ys = M.map replace xs
    where replace x = maybeToList (M.lookup x ys)

-- Testpiemēri funkcijas aa testēšanai
aa1 = aa ["aaa","a","adsafa","v","dfdadfa","dddd", "c", "aaa","a","adsafa"] [("f", "15"),("v", "iii"),("a", "aa"), ("bb", "bbb"), ("c", "def")]
aa2 = aa ["c","a","bb","v","f","deadbeef", "c", "aaa","a","mmm"] [("f", "22"),("v", "iii"),("a", "dead"), ("mmm", "bbb"), ("c", "beef")]

-- Funkcija merge1, kas atrod korteža otrā elementa sakritības ar kortežu 
-- saraksta katra korteža pirmo elementu un atgriež attiecīgo sarakstu ar
-- atrastajiem, modificētajiem kortežiem
merge1 :: (Eq a) => (a, a) -> [(a, a)] -> [(a, a)]
merge1 _ [] = []
merge1 x (y:ys)
    | snd x == fst y = (fst x, snd y) : merge1 x ys
    | otherwise = merge1 x ys

-- Funkcija cc, kas prasīta mājas 1. uzdevuma B piemērā
cc xs ys = do
  let curr_xs = rmdups xs
  let curr_ys = rmdups ys
  forM_ curr_xs $ \ss -> do
    print (merge1 ss curr_ys)

-- Testpiemēri funkcijas cc testēšanai
cc1 = cc [("a","b"), ("e", "b"), ("a","b"), ("e", "b"), ("a","b"), ("e", "b")] [("b","c"), ("b", "d"), ("b","c"), ("b", "d"), ("b","c"), ("b", "d")] 
cc2 = cc [("hello", "hi"), ("foo", "baz"), ("this", "that"), ("boom", "bounty"), ("building", "country")] [("foo", "bar"), ("hi", "world"), ("goo", "boo"), ("bounty", "tequila"), ("country", "floors")]