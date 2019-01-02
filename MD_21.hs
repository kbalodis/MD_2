-- No saraksta tiek izņemti duplikāti
rmdups :: (Eq a) => [a] -> [a]
rmdups l = rmdups' l []             
  where rmdups' [] _ = []
        rmdups' (x:xs) ls | x `elem` ls = rmdups' xs ls | otherwise = x : rmdups' xs (x:ls)

-- Ar "list comprehension" metodi tiek atlasīti/tulkoti vajadzīgie elementi no saraksta un vārdnīcas.
-- [<elements, kas vajadzīgs> | <x mainīgais satur saraksta elementus>, <y mainīgais satur vārdnīcas elementus>, <nosacījums, pie kura tiek pievienots vajadzīgais elements>]
funct :: Eq a => [a] -> [(a, a)] -> [a]
funct xs ys = [(snd y) | x <- xs, y <- ys, x == (fst y)]

-- Funkcija aa, kas prasīta 1. mājas darba A. piemērā
aa xs ys = do
    let zs = rmdups xs
    funct zs ys

-- Testpiemēri funkcijas aa testēšanai
aa1 = aa ["aaa","a","adsafa","v","dfdadfa","dddd", "c", "aaa","a","adsafa"] [("f", "15"),("v", "iii"),("a", "aa"), ("bb", "bbb"), ("c", "def")]
aa2 = aa ["c","a","bb","v","f","deadbeef", "c", "aaa","a","mmm"] [("f", "22"),("v", "iii"),("a", "dead"), ("mmm", "bbb"), ("c", "beef")]
aa3 = aa [1,2,3] [(1,2), (1,4), (2,4), (2,5)]

-- Ar "list comprehension" metodi tiek tulkoti divu vārdnīcu attiecīgie elementi, atgriež jaunu vārdnīcu
-- [<iztulkotais vārdnīcas elements> | <x mainīgais kas satur pirmās vārdnīcas elementus>, <y mainīgais, kas satur otrās vārdnīcas elementus>, <nosacījums, pie kura tiek pievienots jauns elements vārdnīcai>]
funct2 :: Eq a => [(a, a)] -> [(a, a)] -> [(a, a)]
funct2 xs ys = [(fst x, snd y) | x <- xs, y <- ys, (snd x) == (fst y)]

-- Funkcija cc, kas prasīta mājas 1. uzdevuma B piemērā
cc xs ys = do
  let curr_xs = rmdups xs
  let curr_ys = rmdups ys
  funct2 curr_xs curr_ys

-- Testpiemēri funkcijas cc testēšanai
cc1 = cc [("a","b"), ("e", "b"), ("a","b"), ("e", "b"), ("a","b"), ("e", "b")] [("b","c"), ("b", "d"), ("b","c"), ("b", "d"), ("b","c"), ("b", "d")] 
cc2 = cc [("hello", "hi"), ("foo", "baz"), ("this", "that"), ("boom", "bounty"), ("building", "country")] [("foo", "bar"), ("hi", "world"), ("goo", "boo"), ("bounty", "tequila"), ("country", "floors")]