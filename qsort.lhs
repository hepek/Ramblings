>import System.IO.Unsafe
>import Control.Arrow
>import Control.Monad
>import Control.Applicative
>import Random
>
>red     = "\ESC[1;31m"
>clear   = "\ESC[0m"
>uTrace  = unsafePerformIO . trace
>trace s = putStrLn $ red ++ (show . length) s ++ clear
>
>
>qsort []     = []
>qsort (x:xs) =      
>      uTrace xs
>      `seq` 
>      zip2nd ls (qsort ls) ++ [x] ++ zip2nd rs (qsort rs)
>    where
>    ls = filter (<  x) xs
>    rs = filter (>= x) xs
>
>        -- forces the second argument to have the same length
>        -- as the first.
>    zip2nd []      _      =  []
>    zip2nd (_:xs) ~(y:ys) = y:zip2nd xs ys
>
>qsortM :: Ord a => [a] -> IO [a]
>qsortM [] = return []
>qsortM (x:xs) = do
>      trace xs
>      join <$> sequence [qsortM ls, return [x], qsortM rs]
>    where
>       ls = filter (< x) xs
>       rs = filter (>=x) xs
>
>main = do     
>     a <- newStdGen
>     let lst = take 20 $ randomRs (0,200) a ::[Int]
>     print $ qsort lst
>--     print =<< ((liftM (take 2)) (qsortM lst))
>     return ()