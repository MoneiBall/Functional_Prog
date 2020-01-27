module Endterm
import StdEnv

// 1. Map a list of functions to a value. E.g. mapfun [f,g,h] x = [f x, g x, h x]
mapfun :: [Int -> Int] Int -> [Int]
mapfun [] x = []
mapfun [f : fx] x = [f x : mapfun fx x]

//Start = mapfun [inc, inc, inc] 3 // [4, 4, 4]

//zip2		:: ![.a] [.b] 		-> [(.a,.b)]		//	[(a0,b0),(a1,b1),...
//unzip		::	![(.a,.b)] 		-> ([.a],[.b])		//	([a0,a1,...],[b0,b1,...])


// 6. Replicate n>0 times the element of a list e.g. n=3 [3..6] ->
replicate :: Int Int -> [Int]
replicate 0 k = []
replicate n k = [k : replicate (n-1) k]

f55 :: Int [Int] -> [[Int]]
f55 n x = map (replicate n) x 
//Start = f55 3 [3..6] // [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]

// 6. generate the list [[1],[2,2],[3,3,3],[4,4,4,4],...,[10,..,10]]
l6 :: [[Int]]
l6 = [[a \\ b <- [1..a]] \\ a <- [1..10]] 

//Start = l6


// sikis 2qosa noqtededi. cixartsan listden oxuyur
// 8. exam question Find most frequent number or char 

StrtoChar :: String -> [Char]
StrtoChar str = [x \\ x <-: str]
//
f10 [] = []
f10 [x:xs] = [filter ((==) x) xs] ++ f10 xs 
//
maxl str = maxList (map (\x = length x )(f10 (StrtoChar str)))
//
yekun [x:xs] max
|length x == max = hd x 
= yekun xs max
//
final str = yekun (f10 (StrtoChar str)) (maxl str)

//Start = final "helllllanjbhvhjvhhhhhhhhhhhhhaakaaao"

// 2. write a function that removes neighbour duplicates in a list
duplicrem :: [Int] -> [Int]
duplicrem [] = []
duplicrem [x:xs]
|(length xs > 0) && (x == hd xs) = duplicrem xs 
= [x : duplicrem xs]
//Start = duplicrem [1, 0, 5, 0, 0, 6, 7, 5, 0, 0, 0, 8, 0, 5, 0, 0, 0] 

// 1. Compute the sum of the list of tuples [(1,1), (2,2), (3,3)] -> (6,6)
sumtup :: [(Int,Int)] -> (Int,Int)
sumtup [] = (0,0)
sumtup [t:ts] = (fst t + fst (sumtup ts),snd t + snd (sumtup ts))
//Start = sumtup [(1,1), (2,2), (3,3)]

// 7. Form triple tuples of 3 lists selecting one element from each list.
// E.g. for ([1..10],[20..25],[35..47]) the result is 
//[(1,20,35),(2,21,36),(3,22,37),(4,23,38),(5,24,39),(6,25,40)]
tri :: ([Int],[Int],[Int]) -> [(Int,Int,Int)]
tri (a,b,c) = [(x,y,z) \\ x <- a & y <- b & z <- c]
//Start = tri ([1..10],[20..25],[35..47])


//3. Given a list of triple tuples make a tuple of 3 lists like:
clist :: [(Int, Int, Int)] -> ([Int], [Int], [Int])
clist x = ([fst3 y \\ y <- x],[snd3 y \\ y <-x],[thd3 y \\ y <-x])

//Start = clist [(1,2,1), (3,1,4), (8,5,4), (5,7,0), (8,9,1)]  // ([1,3,8,5,8],[2,1,5,7,9],[1,4,4,0,1])


// 7. Check if a list contains 3 equal elements one after the other 
// (it can be anywhere in the list) 
// for [1,2,3,3,3,2,4,5] is True for [1 .. 5] is False
dlist :: [Int] -> Bool
dlist [] = False
dlist [x] = False
dlist [x1,x2] = False
dlist [x1,x2,x3 : xs]
| x1 == x2 && x2 == x3 = True
= dlist [x2,x3 : xs]

//Start = dlist [1,2,2,3,4,3,3,2,4,5,5,2] 


// 8. Extract the third element of the sublists (if there is no such element, ignore that sublist)
// [[1,2,3], [3,4,5,6], [], [5,7,8,11], [1], [8,9]]-> [3,5,8]
qlist :: [[Int]] -> [Int]
qlist x = map (\x = x!!2) (filter (\x= length x >2) x)

//Start = qlist [[1,2,3], [3,4,5,6], [], [5,7,8,11], [1], [8,9]]

//=============================HW========================================//

//HW4 // Task 1.
//Given two lists of tuples, return a list of common tuples using list comprehension.
Intersect :: [(Int, Int)] [(Int, Int)] -> [(Int, Int)]
Intersect x y = [x \\ x <- y & y <- x | x == y]
//Start = Intersect [(1,1),(2,1),(1,2),(2,2),(1,3),(2,3)] [(1,1),(2,1),(1,2),(2,2)] //[(1,1),(2,1),(1,2),(2,2)]
//Start = Intersect [(1,1),(2,1),(1,2),(2,2)] [(1,3),(2,3),(1,4),(2,4)] //[]
//Start = Intersect [(1,1),(2,1),(1,2),(2,2)] [] //[]
//Start = Intersect [] [] //[]

//HW4 // Task 2.

//Using list comprehension, create a list of 'k' leap years starting from year 'x'.
//For negative year 'x' or negative 'k', return the empty list.



LeapYearList :: Int Int -> [Int]
LeapYearList x k
| x < 0 || k < 0 = []
| otherwise = take k [x \\ x <- [t,t+4..] | ((x rem 4 == 0) && (x rem 100 <> 0)) || (x rem 400 == 0)] where t = x + 4 - (x rem 4)

//Start = LeapYearList 2000 5 //[2004, 2008, 2012, 2016, 2020]
//Start = LeapYearList 1997 3 //[2000, 2004, 2008]
//Start = LeapYearList 1899 1 //[1904]
//Start = LeapYearList -1294 4 //[]
//Start = LeapYearList 1993 -6 //[]


//HW1 Given n and k, k<n, compute the value of n choose k (n(n-1)...(n-k+1)) / (12...k). It must be solved without using a factorial function.
ff :: Int Int -> Int
ff n k
|n < k = 0
|k == 0 = 1
= n * ff (n-1)(k-1) / k

//Start = f 5 3

//HW2 check if a number is palindrom e.g.12321
p :: Int -> [Int]
p 0 = []
p n = [n rem 10 : p (n/10)]  
//Start = p 12321
//digits :: Int [Int] -> [Int]
//digits p x = 

pali :: Int -> Bool
pali x = (p x) == reverse (p x)

//Start = pali 12321 // True

//HW2 [element ++ occurence ++ frequency] 
MakeFrequenceTable :: [Int] -> [[Int]]
MakeFrequenceTable [] = [[]]
MakeFrequenceTable x = [[a] ++ [length( filter( (==) (a)) x)] ++ [(length( filter( (==) (a)) x) *100) / length x] \\ a <- n]
where n = removeDup x
//Start = MakeFrequenceTable [1..10]

//HW2 Given a list of two numbers n and k, generate a list of k pieces of multiples of n. For lists with only 1 member it should return that number back. For empty list it should return an empty list. */
Multiples :: [Int] -> [Int]
Multiples [] = []
Multiples [n] = [n]
Multiples [n,0] = []
Multiples [n,k] = map ((+) n) [0 : Multiples[n,k-1]] 

//Start = Multiples [2,3] //[2,4,6]

//HW2 Write a function that will produce a list of the n-th prime from a list of natural numbers n_1...n_k. For example, a parameter list of [7,1,3] should return a list of the 7th, 1st, and 3rd primes. Empty case is required with an output of -1.*/
primes :: Int -> Bool
primes n = length [x \\ x<-[1..n] | n rem x == 0] == 2
PrimeList [] = [-1]
PrimeList [0] = [0]
PrimeList n = [[x \\ x <-[1..] | primes x || x== 1]  !!(a-1) \\ a <- n]















