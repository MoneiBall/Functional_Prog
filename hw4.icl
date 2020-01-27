module hw4
import StdEnv

//Task 1.
//Given two lists of tuples, return a list of common tuples using list comprehension.
Intersect :: [(Int, Int)] [(Int, Int)] -> [(Int, Int)]
Intersect x y = [x \\ x <- y & y <- x | x == y]
//Start = Intersect [(1,1),(2,1),(1,2),(2,2),(1,3),(2,3)] [(1,1),(2,1),(1,2),(2,2)] //[(1,1),(2,1),(1,2),(2,2)]
//Start = Intersect [(1,1),(2,1),(1,2),(2,2)] [(1,3),(2,3),(1,4),(2,4)] //[]
//Start = Intersect [(1,1),(2,1),(1,2),(2,2)] [] //[]
//Start = Intersect [] [] //[]

//Task 2.

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





