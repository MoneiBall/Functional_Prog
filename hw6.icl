module hw6
import StdEnv

// 1. Generate the first 10 multiples of a number. // e.g. x=5 then the list is [5,10,15,20,25,30,35,40,45,50]
f1 :: Int -> [Int]
f1 x = take 10 ((map ((*) x) [1..] ))
//Start = f1 5

// 2. Given a list of lists, generate list of lists of pairs like in the following: // e.g. [[1,2,3], [4,5], [6,7,8], [9,11,1,1,1]] -> // [[(1,1),(2,1),(3,1)], [(4,2),(5,2)], [(6,3),(7,3),(8,3)], [(9,4),(11,4),(1,4),(1,4),(1,4)]]
f2 :: [[Int]] ->[[(Int, Int)]]
f2 [] = []
f2 a =  f2 (init a) ++ [[(x,y) \\ x <- last a, y
 <- [length a]]]
Start = f2 [[1,2,3], [4,5], [6,7,8], [9,11,1,1,1]]


// 3. Sum up the elements of the sublists and arrange them im decreased order. // e.g. [[1,1,2], [10,7,5], [3,1,2], [9,3,1,1]] -> [22,14,6,4]
f3 :: [[Int]] -> [Int]
f3 x = sortBy (>) (map sum x)
 
//Start = f3 [[1,1,2], [10,7,5], [3,1,2], [9,3,1,1]]

// 4. Remove the duplicates of a list that are in a sequence and replace it with 0. // e.g. [1,2,2,2,2,3,6,4,2,2,1,1,1,3,3,5,4,4,4,4,6] -> [1,0,3,6,4,0,0,0,5,0,6] // f4 :: [Int] -> [Int]




