import Control.Monad
import Data.List as M

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
