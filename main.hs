import System.Random
import Control.Monad
import System.IO.Unsafe
import Data.List

-- USEFUL STUFF

fixa f = let {x = f x} in x

giveRand :: Random c => c -> c -> IO c
giveRand m n = getStdRandom $ randomR (m,n)

encageIO :: [a] -> IO [a]
encageIO x = return $ x

-- 1 QUESTAO

myLast :: [a] -> a
myLast [] = error "NOPE"
myLast [x] = x
myLast (_:xs) = myLast xs

-- 2 QUESTAO

butLast :: [a] -> a
butLast [] = error "NOPE"
butLast [x] = error "NOPE"
butLast [x,_] = x
butLast (_:xs) = butLast xs

-- 3 QUESTAO

elementAt :: [a] -> Integer -> a
elementAt [] (_) = error "NOPE"
elementAt xs 0 = head xs
elementAt (_:xs) n = elementAt xs (n-1)

-- 4 QUESTAO

myLength :: [a] -> Integer
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 5 QUESTAO

myReverse :: [a] -> [a] -> [a]
myReverse [] [] = []
myReverse [] xd = xd
myReverse (x:xs) [] = myReverse xs [x]
myReverse (x:xs) xd = myReverse xs (x:xd)

-- 6 QUESTAO

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = error "NOPE"
isPalindrome [x] = True
isPalindrome [x,y] = (x == y)
isPalindrome x = ((head x) == (last x)) && (isPalindrome (init (tail x)))

-- 7 QUESTAO
data NestedList a = Elem7 a | List7  [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem7 a) = [a]
myFlatten (List7 []) = []
myFlatten (List7 (x:xs)) = myFlatten x ++ myFlatten (List7 xs)

-- 8 QUESTAO

myCompress :: (Eq a) => [a] -> [a]
myCompress (x:xs) = foldl (\a b -> if(last a == b) then a else (a ++ [b])) [x] xs

-- 9 QUESTAO

myPacking :: (Eq a) => [a] -> [[a]]
myPacking (x:xs) = foldl (\a b -> if(head (last a) == b) then init a ++ [last a ++ [b]] else a ++ [[b]]) [[x]] xs

-- 10 QUESTAO

myEncode :: (Eq a) => [a] -> [(Int, a)]
myEncode (x:xs) = foldl (\a b -> if (snd (last a)) == b then (init a) ++ [((fst (last a)) + 1, b)] else a ++ [(1,b)]) [(1, x)] xs

-- 11 QUESTAO
data MultiList a = Elem11 a | Tuple11 (Int, a) deriving (Show)

myEncode2 :: (Eq a) => [a] -> [MultiList a]
myEncode2 x = map (\a -> if(fst a == 1) then (Elem11 (snd a)) else (Tuple11 a)) (myEncode x)

-- 12 QUESTAO

myDecode2 :: (Eq a) => [MultiList a] -> [a]
myDecode2 x = foldl (++) []
    (map (\a -> case a of Elem11 s -> [s]
                          (Tuple11 s) -> (replicate (fst s) (snd s)))
    x)

-- 13 QUESTAO

--Previously done in question 10

-- 14 QUESTAO

myDuplicate :: [a] -> [a]
myDuplicate x = foldl (++) [] (map (\a -> [a,a]) x)

-- 15 QUESTAO

myReplicate :: (Eq a) => [a] -> Int -> [a]
myReplicate x n = foldl (++) [] (map (fixa (\a b c -> if(b == 0) then [] else c:(a (b-1) c)) n) x)

-- 16 QUESTAO

myDroping :: [a] -> Int -> Int -> [a]
myDroping [] n m = []
myDroping x n m = if(mod n m == 0) then myDroping (tail x) (n+1) m else (head x):myDroping (tail x) (n+1) m

-- 17 QUESTAO

mySplit :: (Eq a) => [a] -> Int -> [a] -> [[a]]
mySplit (x:xs) 1 xd = [xd++[x], xs]
mySplit (x:xs) n xd = mySplit xs (n-1) (xd++[x])

-- 18 QUESTAO

mySlice :: [a] -> Int -> Int -> [a] -> [a]
mySlice x 1 0 xd = xd
mySlice (x:xs) 1 n xd = mySlice xs 1 (n-1) (xd++[x])
mySlice (x:xs) m n xd = mySlice xs (m-1) (n-1) xd

-- 19 QUESTAO

myRotate :: (Eq a) => [a] -> Int -> [[a]] -> [a]
myRotate x n [[]] = myRotate [] n (mySplit x n [])
myRotate [] n xd = last xd ++ head xd

-- 20 QUESTAO

myRemoveAt :: [a] -> Int -> [a]
myRemoveAt (x:xs) 1 = xs
myRemoveAt (x:xs) n = x:(myRemoveAt xs (n-1))

-- 21 QUESTAO

myInsertAt :: a -> [a] -> Int -> [a] -> [a]
myInsertAt w x 1 xd = (xd++[w])++x
myInsertAt w (x:xs) n xd = myInsertAt w xs (n-1) (xd++[x])

-- 22 QUESTAO

myRange :: Int -> Int -> [Int]
myRange a b
  | a == b = [a]
  | otherwise = [a]++(myRange (a+1) b)

-- 23 QUESTAO

myRandomSelectRepetition :: [a] -> Int -> [a] -> [a]
myRandomSelectRepetition x 1 xd = (x !! (unsafePerformIO $ (giveRand 1 9))):xd
myRandomSelectRepetition x n xd = myRandomSelectRepetition x (n-1) $ (x !! (unsafePerformIO $ (giveRand 1 9))):xd

-- 24 QUESTAO

myRandomRangeSelect :: Int -> Int -> Int -> IO [Int]
myRandomRangeSelect m n s = replicateM s $ giveRand m n

-- 25 QUESTAO

(++.) :: Monad m => m[a] -> [a] -> m[a]
ms1 ++. ms2 = do
  s1 <- ms1
  return $ s1 ++ ms2

myRandPerm :: [Int] -> IO [Int] -> IO [Int]
myRandPerm [] xd = xd
myRandPerm x xd = do
  val <- giveRand 0 $ length x - 1
  let firstPart = take val x
  let secondPart = drop  (val + 1) x
  myRandPerm (firstPart++secondPart) (xd ++. [x !! val])

-- 26 QUESTAO

myGenComb :: [a] -> Int -> [[a]]
myGenComb _ 0 = [[]]
myGenComb x n = [y:xs | y:xd <- tails x,
                        xs <- myGenComb xd (n-1)]

myGenComb2 :: [a] -> Int -> [[a]]
myGenComb2 _ 0 = [[]]
myGenComb2 x n = let ok (y:xd) = (let oj xs = [y:xs]
                                  in concatMap oj (myGenComb2 xd (n-1)) )
                     ok (_) = []
                 in concatMap ok (tails x)

-- 27 QUESTAO

{-REMOVE ALL ELEMENTS IN LIST1 THAT ARE IN LIST2-}
myRemoveList :: (Eq a) => [a] -> [a] -> [a]
myRemoveList x1 x2
  | x2 == [] = x1
  | otherwise = myRemoveList [x | x <- x1, not(x == head(x2))] (tail(x2))


mySubGroups :: (Eq a) => [a] -> [Int]-> [[[a]]]
mySubGroups x1 ns
  | length(x1) == head(ns) = [[x1]]
  | otherwise = [xs:y | xs <- myGenComb2 x1 (head(ns)),
                        y <- mySubGroups (myRemoveList x1 xs) (tail(ns))]

-- 28 QUESTAO

{- THIS FUNCTION TAKES A LIST OF LISTS
   AND A LIST AND INSERT IT BEFORE THE
   FIRST LIST WHICH LENGTH IS EQUAL OR
   GREATER THAN THE PASSED ARGUMENT -}
myInsertLen :: [[a]] -> [a] -> [[a]]
myInsertLen x a
  | length(x) == 0 = [a]
  | length(head x) >= length(a) = a:x
  | otherwise = head(x):myInsertLen (tail(x)) a

{- APPLIES THE PREVIOUS FUNTION TO ALL THE LIST-}
myLenSort :: [[a]] -> [[a]]
myLenSort [[]] = [[]]
myLenSort (x:xs) = foldl(\a b -> myInsertLen a b) [x] xs

myFreqSort :: [[a]] -> [[a]]
myFreqSort = concat . myLenSort .
              groupBy (\a b -> length(a) == length(b)) . myLenSort

-- 31 QUESTAO

isPrime :: Int -> Bool
isPrime x = teste 2
  where
    teste n
      | n*n > x = True
      | x `mod` n == 0 = False
      | otherwise = teste (n+1)

-- 32 QUESTAO

{- GIVEN X2 > X1-}
myGCD :: Int -> Int -> Int
myGCD x1 x2
  | remainder == 0 = x1
  | otherwise = myGCD remainder x1
    where
      quotient = x2 `div` x1
      remainder = x2 `mod` x1

--33 QUESTAO

myCoprime :: Int -> Int -> Bool
myCoprime x1 x2
  | gcdx == 1 = True
  | otherwise = False
    where
      gcdx = myGCD x1 x2

--34 QUESTAO

{-SLOW AND EASY USING COPRIMES FOR ALL
  VALUES. OPTIMAL SOULTION IS WITH THE
  FACTORIZATION OF X DONE IN QUESTION 37-}
myTotientFun :: Int -> Int
myTotientFun x = length $ [y | y <- [1..x], myCoprime y x]


--35 QUESTAO
{- LITTLE RECURSIVE LAMBDA :) -}
myPrimeFactors :: Int -> [Int]
myPrimeFactors x = (fixa (\a b c -> if b==1 then [] else if(b `mod` c == 0) then c:(a (b `div` c) c) else a b (c+1))) x 2

--36 QUESTAO
{- GET THE RESULTS FROM BEFORE, GROUP
   AND GET THE LENGTH-}
myPrimeFactorsMult :: Int -> [(Int,Int)]
myPrimeFactorsMult x = map (\a -> (head(a), length(a)) ) $ groupBy (==) $ myPrimeFactors x

--37 QUESTAO

myGoodTotientFun :: Int -> Int
myGoodTotientFun x = foldl (\c (a,b) -> c*(a-1)*a^(b-1)) 1 $ myPrimeFactorsMult x

--38 QUESTAO
{- NO SOLUTION REQUIRED JUST COMPARE PERFORMANCE
   BEETWEEN QUESTION 34 AND 37-}

-- 39 QUESTAO

myPrimesRange :: Int -> Int -> [Int]
myPrimesRange x1 x2 = [y | y <- [x1..x2], isPrime y]

--40 QUESTAO

myLittleGoldbach :: Int -> (Int,Int)
myLittleGoldbach x = let val = head . filter (\[a,b] -> a+b == x) $ myGenComb [y | y <- [2..x], isPrime y] 2
                     in (head(val),head(tail(val)))

--41 QUESTAO

myGoldbachList :: Int -> Int -> [(Int,Int)]
myGoldbachList x1 x2 = map myLittleGoldbach [y | y <- [val1,val1+2..val2]::[Int] , not(isPrime y)]
                        where
                          val1 = if odd(x1) then x1+1 else x1
                          val2 = if odd(x2) then x2-1 else x2

-- 46 QUESTAO

and46 :: Bool -> Bool -> Bool
and46 a b = a && b
or46 :: Bool -> Bool -> Bool
or46 a b = a || b
nand46 :: Bool -> Bool -> Bool
nand46 a b = not(a && b)
nor46 :: Bool -> Bool -> Bool
nor46 a b = not(a || b)
xor46 :: Bool -> Bool -> Bool
xor46 a b = not(a == b)
equ46 :: Bool -> Bool -> Bool
equ46 a b = a == b
impl46 :: Bool -> Bool -> Bool
impl46 a b = if(a == False && b == True) then False else True

myTable :: (Bool -> Bool -> Bool) -> IO ()
myTable f = do
  mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b) | a <- [True,False] , b <- [True, False]]

-- 47 QUESTAO

infixl 4 `and46`
infixl 6 `or46`

--48 QUESTAO

{-NAO FEITA-}

--49 QUESTAO

myGrayCode :: Int -> [[Int]]
myGrayCode x
  | x == 1 = [[0], [1]]
  | otherwise = (map (\a -> 0:a) subCode) ++ (map (\a -> 1:a) $ reverse subCode)
    where
      subCode = myGrayCode (x-1)

--50 QUESTAO

{-NAO FEITA-}

--54A QUESTAO

{-IMPOSSIBLE TO DO CHECK WIKI-}

--55 QUESTAO

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)

genBalancedTree :: Int -> [Tree Int]
genBalancedTree 0 = [Empty]
genBalancedTree n = let (q,r) = (n-1) `quotRem` 2
                    in [Branch 0 left right | cont1 <- [q..q+r],
                                              left  <- genBalancedTree cont1,
                                              right <- genBalancedTree (n-cont1-1)]
--56 QUESTAO

symmetrI :: Eq a => Tree a -> Tree a -> Bool
symmetrI Empty Empty = True
symmetrI (Branch _ a b) (Branch _ c d) = (symmetrI a c) && (symmetrI b d)
symmetrI _ _ = False

symmetry :: Eq a => Tree a -> Bool
symmetry Empty = True
symmetry (Branch _ b c) = symmetrI b c

--57 QUESTAO

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree a Empty = Branch a Empty Empty
insertTree a (Branch b c d)
  | a < b = Branch b (insertTree a c) d
  | otherwise = Branch b c (insertTree a d)

construct :: Ord a => [a] -> Tree a
construct xs = foldl(\a b -> insertTree b a) Empty xs

--58 QUESTAO

allBalanSimm :: Int -> [Tree Int]
allBalanSimm x = filter (symmetry) (genBalancedTree x)

--59 QUESTAO
{-DO LATER-}

-- 60 QUESTAO
{-DO LATER -}

--61 QUESTAO

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch a b c) = 1 + (countLeaves b) + countLeaves c

{-PART A-}

treeToList :: Tree a -> [a]
treeToList Empty = []
treeToList (Branch a b c) = treeToList b ++ [a]  ++ treeToList c

--62 QUESTAO

internalNodes :: Tree a -> [a]
internalNodes Empty = []
internalNodes (Branch _ Empty Empty) = []
internalNodes (Branch a b c) = [a] ++ internalNodes b ++ internalNodes c

{-PART B-}

atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch a _ _) 1 = [a]
atLevel (Branch _ b c) n = (atLevel b (n-1)) ++ (atLevel c (n-1))

--63 QUESTAO



main = do
  valor <- encageIO [1,2,3,4,5,6,7,8,9,10]
  valor2 <- encageIO [1,3,8]
  print $ allBalanSimm 5
  print $ countLeaves $ head $ allBalanSimm 5
